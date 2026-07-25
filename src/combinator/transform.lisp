(defpackage :cltpt/transform
  (:use :cl)
  (:export
   :transform
   :reconstruct
   :generate
   :decode))

(in-package :cltpt/transform)

;; this does not work with "streams" yet. the idea is to eventually use the combinator
;; and the transformer in the same synchronized pass and transform the parsed data on-the-fly to
;; make things more efficient. currently we would be re-transforming the text every time the
;; result of the parser changes if we wanted to do "incremental" parsing/transforming.
;; incremental parsing+transforming would require keeping track of the state of the
;; parser-combinator so as to invoke the corresponding transformation functions and apply
;; backtracking to the transformed text (which would ideally be built in chunks as we advance
;; through the tree) if the combinator backtracks.
;; also, the transformer itself should return a reader aswell, one that it should fill up
;; continuously while reading from its input reader.

;; this only works in cases where the original source rule that was used to parse the text matches
;; the same structure of 'dest-rule'
(defun reconstruct (reader match dest-rule)
  (if (stringp dest-rule)
      dest-rule
      (let* ((rule (if (keywordp (car dest-rule))
                       (getf dest-rule :pattern)
                       dest-rule))
             (id (and (keywordp (car dest-rule))
                      (getf dest-rule :id)))
             (submatch (and id (cltpt/combinator:find-submatch match id))))
        ;; if we found the corresponding submatch, we extract the text from it, otherwise
        ;; we run the combinator's transform function.
        (if submatch
            (cltpt/combinator:match-text submatch reader)
            (let* ((matcher (car rule))
                   (reconstructor (reconstructor-for-combinator matcher)))
              (and reconstructor
                   (apply reconstructor
                          reader
                          match
                          (cdr rule))))))))

(defun reconstructor-for-combinator (matcher)
  (let ((func-name (intern (concatenate 'string "RECONSTRUCT-" (symbol-name matcher))
                           (find-package :cltpt/transform))))
    (when (fboundp func-name)
      func-name)))

(defun reconstruct-consec (reader match &rest all)
  (let ((parts (loop for child in all
                     for submatch in (cltpt/combinator:match-children match)
                     for res = (reconstruct reader submatch child)
                     unless res return nil
                       collect res)))
    (when parts
      (apply #'concatenate 'string parts))))

(defun reconstruct-any (reader match &rest alternatives)
  (loop for alt in alternatives
        for res = (reconstruct reader match alt)
        when res return res))

(defun reconstruct-literal (reader str)
  str)

(defun reconstruct-pair (reader match opening closing &key rules-for-content)
  (let* ((children (cltpt/combinator:match-children match))
         (content-children (butlast (cdr children)))
         (full-pair-rule (list 'cltpt/combinator:pair
                               opening
                               closing
                               :rules-for-content rules-for-content))
         (content-parts (loop for child in content-children
                              for rule in rules-for-content
                              for res = (or (reconstruct reader child full-pair-rule)
                                            (reconstruct reader child rule))
                              unless res return nil
                                collect res)))
    (when content-parts
      (concatenate 'string
                   opening
                   (apply #'concatenate 'string content-parts)
                   closing))))

;; the "generation" direction: interpret a rule against a lisp VALUE instead of text, producing
;; the text that would parse back into it (`generate' is value->text).
;;
;; annotations, via the plist wrapper (:pattern ... :id ...):
;; - :value :: makes generation take the route of this branch when the value matches.
;; - :get   :: function applied to the value before passing it on.
;; - :data  :: (consec subrules only) T (the default) marks a data slot that "consumes" one element
;;             off the consec's list VALUE. :data nil marks fixed syntax that consumes nothing.

(defun resolve-rule (rule)
  "resolve a rule given as a symbol to its value, like `apply-rule' does."
  (if (and rule (symbolp rule))
      (symbol-value rule)
      rule))

(defun rule-plist-p (rule)
  (and (consp rule) (keywordp (car rule))))

(defun generator-for-combinator (matcher)
  (let ((func-name (intern (concatenate 'string "GENERATE-" (symbol-name matcher))
                           (find-package :cltpt/transform))))
    (when (fboundp func-name)
      func-name)))

(defun generate (rule value)
  "interpret RULE in the generation direction: produce the text representing VALUE.
returns a string, or NIL if RULE cannot represent VALUE (which makes `any' backtrack to its next
alternative, mirroring the parsing direction)."
  (let ((rule (resolve-rule rule)))
    (cond
      ((stringp rule) rule)
      ((rule-plist-p rule)
       (let* ((get (getf rule :get))
              (value (if get (funcall get value) value))
              (value-cell (member :value rule))
              (pattern (getf rule :pattern)))
         (if value-cell
             ;; if :value is present we only proceed if its assigned value is equal to VALUE
             (when (equal value (cadr value-cell))
               (generate pattern value))
             (generate pattern value))))
      (t (let ((generator (generator-for-combinator (car rule))))
           (when generator
             (apply generator value (cdr rule))))))))

(defun generate-any (value &rest alternatives)
  (loop for alt in alternatives
        for res = (generate alt value)
        when res return res))

(defun generate-literal (value str)
  str)

(defun consec-part-data-p (part)
  (let ((part (resolve-rule part)))
    (cond
      ((stringp part) nil)
      ((not (consp part)) nil)
      ((rule-plist-p part) (getf part :data t))
      (t t))))

(defun generate-consec (value &rest parts)
  (let ((remaining value)
        (has-data)
        (texts))
    (dolist (part parts)
      (let ((text (cond
                    ((consec-part-data-p part)
                     (setf has-data t)
                     (and (consp remaining)
                          (prog1 (generate part (car remaining))
                            (setf remaining (cdr remaining)))))
                    (t (generate part nil)))))
        (unless text
          (return-from generate-consec nil))
        (push text texts)))
    (when (and has-data remaining)
      (return-from generate-consec nil))
    (apply #'concatenate 'string (nreverse texts))))

(defun generate-separated-atleast-one (value sep-rule el-rule)
  (when (consp value)
    (let* ((sep (generate sep-rule nil))
           (parts (loop for (el . rest) on value
                        for text = (generate el-rule el)
                        unless text return nil
                          collect text
                        when rest collect sep)))
      (when parts
        (apply #'concatenate 'string parts)))))

(defun generate-number-matcher (value)
  (when (numberp value)
    (write-to-string value)))

(defun generate-all-but (value &rest args)
  (when (stringp value)
    value))

;; the "decoding" direction: interpret a rule against a parsed MATCH to recover the lisp VALUE it
;; represents. `decode' is match->value while `generate' is value->text.

(defun decoder-for-combinator (matcher)
  (let ((func-name (intern (concatenate 'string "DECODE-" (symbol-name matcher))
                           (find-package :cltpt/transform))))
    (when (fboundp func-name)
      func-name)))

(defun decode (reader match rule)
  "interpret RULE in the decoding direction against MATCH, recovering the lisp value."
  (let ((rule (resolve-rule rule)))
    (cond
      ((stringp rule)
       (values nil nil))
      ((rule-plist-p rule)
       (let ((value-cell (member :value rule))
             (pattern (getf rule :pattern)))
         (if value-cell
             (values (cadr value-cell) t)
             (decode reader match pattern))))
      (t (let ((decoder (decoder-for-combinator (car rule))))
           (if decoder
               (apply decoder reader match (cdr rule))
               (values nil nil)))))))

(defun decode-literal (reader match str)
  (values nil nil))

(defun decode-all-but (reader match &rest args)
  (values (cltpt/combinator:match-text match reader) t))

(defun decode-number-matcher (reader match)
  (values (read-from-string (cltpt/combinator:match-text match reader))
          t))

(defun decode-any (reader match &rest alternatives)
  (let ((child (car (cltpt/combinator:match-children match))))
    (decode reader
            child
            (cltpt/combinator:match-rule child))))

(defun decode-consec (reader match &rest parts)
  (let ((values (loop for part in parts
                      for child in (cltpt/combinator:match-children match)
                      append (multiple-value-bind (v present) (decode reader child part)
                               (when present
                                 (list v))))))
    (if (and values (null (cdr values)))
        (values (car values) t)
        (values values t))))

(defun decode-separated-atleast-one (reader match sep-rule el-rule)
  (let ((values (loop for child in (cltpt/combinator:match-children match)
                      for i from 0
                      when (evenp i)
                        collect (decode reader child el-rule))))
    (values values t)))