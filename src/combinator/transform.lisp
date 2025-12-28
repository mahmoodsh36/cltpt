(defpackage :cltpt/transform
  (:use :cl)
  (:export
   :transform
   :reconstruct))

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

;; rule is a combinator rule, replacements is a list of plists to replace inner rules from the
;; original RULE with.
;; e.g. rule=(cltpt/combinator:separated-atleast-one ,(string #\newline) (:name myline :pattern (cltpt/combinator:all-but-newline)))
;; replacements=((:name myline :replacement (:name mypaddedline :pattern '(lambda (reader writer match) (match-text reader match)))))
(defun transform (reader match replacements)
  (let ((full-rule (cltpt/combinator:match-rule match)))
    ;; if the rule is a string its just a literal
    (if (stringp full-rule)
        full-rule
        (let* ((rule (if (keywordp (car full-rule))
                         (getf full-rule :pattern)
                         full-rule))
               (id (and (keywordp (car full-rule))
                        (getf full-rule :id)))
               (replacement (and id (cdr (assoc id replacements :test 'string=)))))
          (when replacement
            (setf rule
                  (if (keywordp (car replacement))
                      (getf replacement :pattern)
                      replacement)))
          (let* ((matcher (car rule))
                 (transformer (transformer-for-combinator matcher)))
            (or (and transformer
                     (apply transformer
                            reader
                            (cdr rule)
                            replacements
                            (cltpt/combinator:match-children match)))
                (cltpt/combinator:match-text match reader)))))))

;; re-interning like that is probably not ideal, is there a better way to go about things?
;; ideally we shouldnt have to implement the transformation functions ourselves, but have
;; a process that goes through the original combinator and figures out how to transform the
;; results based on the structure of the matching rule (e.g. we shouldnt have to implement
;; 'transform-literal' explicitly.)
(defun transformer-for-combinator (matcher)
  (let ((func-name (intern (concatenate 'string "TRANSFORM-" (symbol-name matcher))
                           (find-package :cltpt/transform))))
    (when (fboundp func-name)
      func-name)))

(defun transform-consec (reader subrules replacements &rest matches)
  (with-output-to-string (out)
    (loop for match in matches
          do (write-string (transform reader match replacements) out))
    out))

(defun transform-literal (reader subrules match)
  (cltpt/combinator:match-text match reader))

(defun transform-separated-atleast-one (reader subrules replacements &rest matches)
  (with-output-to-string (out)
    (loop for rest on matches
          for match = (car rest)
          do (write-string (transform reader match replacements) out)
             (unless (null (cdr rest))
               (write-char #\, out)))
    out))

;; this one simply extracts specific stuff from the match for embedding them in the new string
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
                     for res = (reconstruct reader match child)
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