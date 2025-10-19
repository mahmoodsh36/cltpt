(defpackage :cltpt/combinator/utils
  (:use :cl)
  (:export :find-submatch :find-submatch-all :find-submatch-last
           :match-text))

(in-package :cltpt/combinator/utils)

;; (defun find-submatch (match submatch-id &optional (test 'string=))
;;   "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
;;   (cltpt/tree:tree-find
;;    match
;;    submatch-id
;;    :key (lambda (node)
;;           (getf (car node) :id))
;;    :test test))

;; the other `find-submatch' isnt correct in some cases.
;; TODO: figure out why the other find-submatch isnt correct.
(defun find-submatch (match submatch-id)
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
  (when (and (consp (car match)) (symbolp (caar match)))
    (when (string= (getf (car match) :id) submatch-id)
      (return-from find-submatch match))
    (loop for child in (cdr match)
          do (let ((result (find-submatch child submatch-id)))
               (when result
                 (return-from find-submatch result))))))

;; TODO: easy to optimize, we dont have to iterate through the whole tree to find the last instance
(defun find-submatch-last (match submatch-id &optional (test 'string=))
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID. return the last such submatch found."
  (let ((last-found))
    (cltpt/tree:tree-map
     match
     (lambda (submatch)
       (when (funcall test submatch-id (getf (car submatch) :id))
         (setf last-found submatch))))
    last-found))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (cltpt/tree:tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))))

(defun match-text (match)
  (when match
    (subseq (getf match :str)
            (getf match :begin)
            (getf match :end))))