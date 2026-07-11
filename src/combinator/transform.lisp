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