(in-package :cltpt/org-mode)

(defvar *org-babel-min-lines-for-block-output*
  10
  "verbatim results with at least this many lines go in an example block instead of \": \" lines.")

(defun org-src-block-lang (obj)
  (let ((lang-match (cltpt/combinator:find-submatch
                     (cltpt/base:text-object-match obj)
                     'lang)))
    (cltpt/base:text-object-match-text obj lang-match)))

(defun org-src-block-results-options (obj)
  "the list of :results options for OBJ, split and downcased. NIL when absent or non-string."
  (let ((results (org-block-keyword-value obj "results")))
    (when (stringp results)
      (remove ""
              (mapcar #'string-downcase
                      (cltpt/str-utils:str-split results " "))
              :test #'equal))))

(defun org-src-block-result-type (obj)
  (let ((options (org-src-block-results-options obj)))
    (cond ((member "value" options :test #'equal) :value)
          ((member "output" options :test #'equal) :output)
          (t :output))))

(defun org-src-block-results-drawer-p (src-block)
  "whether SRC-BLOCK requests drawer-wrapped results (e.g. :results value drawer)."
  (member "drawer" (org-src-block-results-options src-block) :test #'equal))

(defun org-src-block-results-table-p (src-block)
  "whether SRC-BLOCK wants value results formatted as an org table."
  (member "table" (org-src-block-results-options src-block) :test #'equal))

(defun org-src-block-results-list-p (src-block)
  "whether SRC-BLOCK wants value results formatted as an org list."
  (member "list" (org-src-block-results-options src-block) :test #'equal))

;; TODO: i cant yet think of how to do this through the transform.lisp module to make it more generic.
(defun babel-value-to-org-table (value)
  "format lisp VALUE as org table text. scalars become one cell, flat lists one row."
  (labels ((cell (v)
             (if (stringp v)
                 v
                 (princ-to-string v)))
           (row (cells)
             (let ((sep (format nil " ~C " *table-v-delimiter*)))
               (format nil
                       "~C ~A ~C"
                       *table-v-delimiter*
                       (cltpt/str-utils:str-join (mapcar #'cell cells) sep)
                       *table-v-delimiter*))))
    (cond ((null value)
           "")
          ((and (listp value) (every #'listp value))
           (cltpt/str-utils:str-join (mapcar #'row value) (string #\newline)))
          ((listp value)
           (row value))
          (t
           (row (list value))))))

(defun babel-value-to-org-list (value)
  "format lisp VALUE as org list text. scalars become one item, lists one item per element."
  (labels ((item (v)
             (format nil
                     "- ~A"
                     (if (listp v)
                         (cltpt/str-utils:str-join
                          (mapcar (lambda (e)
                                    (if (stringp e)
                                        e
                                        (princ-to-string e)))
                                  v)
                          " ")
                         (if (stringp v)
                             v
                             (princ-to-string v))))))
    (cond ((null value)
           "")
          ((listp value)
           (cltpt/str-utils:str-join (mapcar #'item value) (string #\newline)))
          (t
           (item value)))))

(defun parse-babel-var-spec (str)
  "parse a :var value like \"a=blk1\" into a (NAME . VALUE) cons."
  (let ((eq-pos (and str (position #\= str))))
    (when eq-pos
      (cons (subseq str 0 eq-pos)
            (subseq str (1+ eq-pos))))))

(defun org-src-block-var-specs (obj)
  "all (NAME . VALUE) var bindings declared across OBJ's :var keyword(s)."
  (loop for (kw . val) in (cltpt/base:text-object-property obj :keywords-alist)
        for spec = (and (equal kw "var") (stringp val) (parse-babel-var-spec val))
        when spec collect spec))

(defun find-org-src-block-by-name (root name)
  "the src-block under ROOT whose :name is NAME, or NIL."
  (let ((found))
    (cltpt/base:map-text-object
     root
     (lambda (obj)
       (when (and (not found)
                  (typep obj 'org-src-block)
                  (equal name (org-block-keyword-value obj "name")))
         (setf found obj))))
    found))

(defun babel-ref-value (obj val)
  "resolve a :var right-hand side VAL to a lisp value, for the block OBJ that declared it.
a VAL naming another src-block in OBJ's document yields that block's computed value, otherwise
VAL is read as a lisp value."
  (let ((blk (find-org-src-block-by-name (cltpt/base:text-object-root obj) val)))
    (if blk
        (org-src-block-value blk)
        (read-from-string val))))

(defun org-src-block-assignments (blk)
  "the (NAME . VALUE) assignments to bind before BLK's code, resolving each var's referenced block (if any)."
  (loop for (name . ref) in (org-src-block-var-specs blk)
        collect (cons name (babel-ref-value blk ref))))

(defun org-src-block-value (obj)
  "the value OBJ produces, to be consumed by another block's :var."
  (let ((result (eval-block obj)))
    (when result
      (let ((text (cltpt/reader:reader-to-string result)))
        (if (eq (org-src-block-result-type obj) :value)
            (cltpt/babel:babel-decode
             (intern (string-upcase (org-src-block-lang obj)) :cltpt/babel)
             text)
            text)))))

(defmethod eval-block ((obj org-src-block))
  (let* ((code (org-src-block-code obj))
         (lang (org-src-block-lang obj))
         (eval-property (org-block-keyword-value obj "eval"))
         (should-eval (not (member eval-property
                                   (list "no" "no-export")
                                   :test #'string=)))
         (results-property (org-block-keyword-value obj "results"))
         (result-type (org-src-block-result-type obj))
         (reconstruct-property (org-block-keyword-value obj "reconstruct"))
         (transform-property (org-block-keyword-value obj "transform"))
         ;; results-rule is for when we want to grab a specific portion of the output and transform
         ;; it into something else.
         (results-rule (cond
                         ((consp results-property) results-property)
                         ;; ((equal results-property "file")
                         ;;  )
                         ;; ((equal results-property "output")
                         ;;  )
                         (t '(cltpt/combinator:atleast-one-discard (cltpt/combinator:all-but nil)))))
         (reconstruct-rule (when (consp reconstruct-property)
                             reconstruct-property)))
    (when (and should-eval
               (cltpt/babel:babel-supported-p lang))
      (multiple-value-bind (out-rdr err-rdr)
          (cltpt/babel:babel-eval*
           (intern (string-upcase lang) :cltpt/babel)
           code
           (org-src-block-assignments obj)
           result-type
           :main (not (equal (org-block-keyword-value obj "main") "no")))
        ;; ideally we should be working with streams.. transformer should work in an "async" manner
        ;; with the parser.
        (let* ((match (car (cltpt/combinator:parse out-rdr (list results-rule))))
               (result (when match
                         (or (when reconstruct-rule
                               (cltpt/transform:reconstruct out-rdr match reconstruct-rule))
                             (when transform-property
                               (funcall transform-property out-rdr match))
                             (cltpt/combinator:match-text match out-rdr)))))
          (values (when result
                    (cltpt/reader:reader-from-string result))
                  err-rdr))))))

;; note that org-mode strips the last newline in the text, but we dont do that, it doesnt make
;; much sense, it makes babel output text lossy. 
(defun babel-result-text-to-org-verbatim (text)
  "format TEXT as verbatim org results."
  (let ((lines (cltpt/str-utils:str-split text (string #\newline))))
    (if (>= (length lines) *org-babel-min-lines-for-block-output*)
        (format nil "#+begin_example~%~A~%#+end_example" text)
        (cltpt/str-utils:str-join
         (mapcar (lambda (line)
                   (concatenate 'string ": " line))
                 lines)
         (string #\newline)))))

(defun org-src-block-results-change (src-block result-text)
  "the `cltpt/buffer:change' replacing SRC-BLOCK's results with RESULT-TEXT, in absolute offsets.
when SRC-BLOCK has no results yet, the change's region is zero-width at the block's end, so it
inserts rather than replaces."
  (let* ((base-begin (cltpt/base:text-object-begin-in-root src-block))
         (results-match (cltpt/base:text-object-find-submatch src-block 'results))
         (block-end (+ base-begin (cltpt/base:text-object-text-length src-block))))
    ;; table/list/etc. results need value text (e.g. [1, 2, 3]) decoded and re-emitted as org
    ;; markup for the parser to pick up.
    (when (eq (org-src-block-result-type src-block) :value)
      (let ((formatter (cond ((org-src-block-results-table-p src-block)
                              #'babel-value-to-org-table)
                             ((org-src-block-results-list-p src-block)
                              #'babel-value-to-org-list))))
        (when formatter
          (setf result-text
                (funcall formatter
                         (cltpt/babel:babel-decode
                          (intern (string-upcase (org-src-block-lang src-block)) :cltpt/babel)
                          result-text))))))
    (cond
      ((org-src-block-results-drawer-p src-block)
       (setf result-text (format nil ":RESULTS:~%~A~%:END:" result-text)))
      ;; plain text gets ": " lines like org does, so it parses back as results and a rerun
      ;; replaces it instead of appending.
      ((not (or (org-src-block-results-table-p src-block)
                (org-src-block-results-list-p src-block)
                (member "raw" (org-src-block-results-options src-block) :test #'equal)
                (org-block-keyword-value src-block "transform")
                (org-block-keyword-value src-block "reconstruct")))
       (setf result-text (babel-result-text-to-org-verbatim result-text))))
    (cltpt/buffer:make-change
     :region (cltpt/buffer:make-region
              :begin (if results-match
                         (cltpt/combinator:match-begin-absolute results-match)
                         block-end)
              :end (if results-match
                       (cltpt/combinator:match-end-absolute results-match)
                       block-end))
     :operator (concatenate 'string
                            (format nil (if results-match
                                            "#+RESULTS:~%"
                                            "~%~%#+RESULTS:~%"))
                            result-text))))

(defmethod eval-blocks ((doc org-document))
  "evaluate the code of org-src-block instances in DOC and register the results as scheduled changes."
  (labels ((handle-obj (obj)
             (when (typep obj 'org-src-block)
               (let ((result (eval-block obj)))
                 (when result
                   (let ((change (org-src-block-results-change
                                  obj
                                  (cltpt/reader:reader-to-string result))))
                     (setf (cltpt/buffer:change-args change)
                           '(:delegate nil
                             :reparse t))
                     (cltpt/buffer:schedule-change* doc change)))))))
    (cltpt/base:map-text-object
     doc
     #'handle-obj)
    (cltpt/buffer:apply-scheduled-changes
     doc
     :on-apply (cltpt/base:make-reparse-callback doc *org-mode*))))

(defmethod cltpt/base:convert-tree :before ((doc org-document) fmt-src fmt-dest &rest args)
  ;; evaluate blocks to prepare them for conversion.
  (when *org-enable-babel*
    (eval-blocks doc)))