(defpackage :cltpt
  (:use :cl))
(in-package :cltpt)
(asdf:load-system :cl-ppcre)

;; for macros (code executions that return objects)
(defvar *text-macro-seq* "#")
;; for metadata? (just plists perhaps..)
;; (defvar *text-metadata-key* #\%)

(defun find-nested-pairs (str1 begin end &optional (pos 0))
  "returns conses of the form (begin . end).
example usage:
  (find-nested-pairs \"hello(mystr()here)\" \"(\" \")\")
((5 . 17) (11 . 12))"
  (let ((my-stack)
        (my-pos pos)
        (all-found))
    (loop while (< my-pos (length str1))
          do (let ((substr (subseq str1 my-pos)))
               (if (uiop:string-prefix-p begin substr)
                   ;; we found an instance of `begin', push it onto the stack
                   (progn
                     (push (cons my-pos -1) my-stack)
                     (setf my-pos (+ my-pos (length begin))))
                   ;; if we didnt detect a `begin', check if we have an instance of `end'
                   (if (uiop:string-prefix-p end substr)
                       ;; if stack isnt empty, pull one instance, we found a match, if stack
                       ;; is empty, we found an `end' with no `begin', we ignore it.
                       (when my-stack
                         (setf (cdr (car my-stack)) (1+ my-pos))
                         (push (car my-stack) all-found)
                         (setf my-stack (cdr my-stack))
                         (setf my-pos (+ my-pos (length end))))
                       ;; neither an instance of `begin' nor `end', continue searching
                       (setf my-pos (1+ my-pos))))))
    all-found))

(defun find-nested-pairs-regex (str1 begin end &optional (pos 0))
  "similar to `find-nested-pairs', but accepts regexes.

returns a list of conses of the form ((begin . end) . (begin . end))."
  (let ((my-stack)
        (my-pos pos)
        (all-found))
    (loop while (< my-pos (length str1))
          do (let* ((substr (subseq str1 my-pos))
                    (begin-match-result (cl-ppcre:all-matches begin substr))
                    (end-match-result (cl-ppcre:all-matches end substr))
                    ;; where the matched `begin' starts (if at all)
                    (begin-match-begin-idx (car begin-match-result))
                    ;; where the matched `begin' ends (if at all)
                    (begin-match-end-idx (cadr begin-match-result))
                    ;; where the matched `end' starts (if at all)
                    (end-match-begin-idx (car end-match-result))
                    (end-match-end-idx (cadr end-match-result)))
               (if (equal begin-match-begin-idx 0)
                   ;; we found a (possibly nested) instance of `begin', push it onto the stack
                   (progn
                     (let ((begin-match-region (cons my-pos (+ my-pos begin-match-end-idx))))
                       (push begin-match-region my-stack)
                       (setf my-pos (+ my-pos begin-match-end-idx))))
                   ;; if we didnt detect a `begin', check if we have an instance of `end'
                   (if (equal end-match-begin-idx 0)
                       ;; if stack isnt empty, pull one instance, we found a match, if stack
                       ;; is empty, we found an `end' with no `begin', we ignore it.
                       (when my-stack
                         (let* ((begin-match-region (car my-stack))
                                (end-match-region (cons my-pos (+ my-pos end-match-end-idx)))
                                (pair (cons begin-match-region end-match-region)))
                           (push pair all-found)
                           (setf my-stack (cdr my-stack))
                           (setf my-pos (+ my-pos end-match-end-idx))))
                       ;; neither an instance of `begin' nor `end', continue searching
                       (setf my-pos (1+ my-pos))))))
    all-found))

(defstruct region
  (begin 0 :type integer)
  (end -1 :type integer))

(defstruct text-macro
  (region nil :type region)
  ;; evaluation result may be equal to a final text-object constructed for the macro, but not necessarily
  result
  final-text-object)

;; sketch note /home/mahmooz/brain/pen/2025-02-28-Note-12-53.xopp
;; we need to keep all macros including those that "end" other objects, perhaps
;; not macros that return the value 'end though, as those should be just discarded from the
;; parse tree itself, they should be however stored in the text object himself so that we know
;; where the ending starts and ends
(defun parse (str1 &optional (as-tree t))
  "parse a string, returns an object tree.
we apply two passes on the string, the first to detect the locations of the macros and
where their ending macros are (if at all).
the order of the results grabbed from `find-nested-pairs' matters for this function as
it reverses it."
  (let ((macros) ;; includes all macros including opening and closing ones
        (final-macro-pairs) ;; once done we would have paired each macro with its ending (if its there)
        (final-objects)
        (my-stack))
    ;; first pass
    (let ((macro-regions (reverse (find-nested-pairs str1 (format nil "~A(" *text-macro-seq*) ")"))))
      (loop for macro-region in macro-regions ;; these regions are represented as just conses
            do (let* ((macro-begin-idx (car macro-region))
                      (macro-end (cdr macro-region))
                      (macro-text (subseq str1
                                          (+ (length *text-macro-seq*) macro-begin-idx)
                                          macro-end))
                      (result)
                      (final-text-object))
                 (handler-case (eval (read-from-string macro-text))
                   (error (c)
                     (format t "error while evaluating macro ~A: ~A.~%" macro-text c)
                     (setf result 'broken))
                   (:no-error (result1)
                     ;; (format t "evaluated macro ~A: ~A~%" macro-text result1)
                     (setf result result1)
                     (if (typep result1 'text-object)
                         (setf final-text-object result1)
                         (setf final-text-object (make-instance 'default-text-object)))))
                 (when (equal result 'broken)
                   (setf final-text-object (make-instance 'default-text-object)))
                 (let ((new-text-macro
                         (make-text-macro
                          :region (make-region :begin macro-begin-idx :end macro-end)
                          :result result
                          :final-text-object final-text-object)))
                   ;; handle scope of macro pair (if we found one)
                   (when (subtypep (class-of final-text-object) 'cltpt::text-object)
                     (text-object-init final-text-object str1 new-text-macro nil))
                   (let ((done))
                     (loop for prev-macro in (reverse my-stack) for prev-macro-idx from 0 while (not done)
                           ;; check if this macro is the closing of a previous macro, if so,
                           ;; we have a "lexical" scope and we should drop everything in between
                           do (when (and prev-macro
                                         (not (find prev-macro final-macro-pairs :key 'car))
                                         (text-object-ends-by
                                          (text-macro-final-text-object prev-macro)
                                          (text-macro-result new-text-macro)))
                                ;; this macro is the ending of the previous one, we're done with them
                                ;; not efficient
                                ;; (setf final-objects (remove (text-macro-final-text-object prev-macro) final-objects))
                                (push (cons prev-macro new-text-macro) final-macro-pairs)
                                (text-object-init final-text-object
                                                  str1
                                                  prev-macro
                                                  new-text-macro)
                                (setf (text-macro-final-text-object prev-macro) final-text-object)
                                (loop for i from 1 to prev-macro-idx
                                      do (let ((popped (text-macro-final-text-object (car my-stack))))
                                           (push popped
                                                 (text-object-children
                                                  (text-macro-final-text-object prev-macro)))
                                           ;; this could be done more generailly and DRYed by having a function in the region struct
                                           (decf (region-begin (text-macro-region (text-object-opening-macro popped)))
                                                 (region-end (text-macro-region prev-macro)))
                                           (decf (region-end (text-macro-region (text-object-opening-macro popped)))
                                                 (region-end (text-macro-region prev-macro)))
                                           (setf my-stack (cdr my-stack))))
                                (setf done t)))
                     (unless done
                       (push new-text-macro my-stack)
                       (push final-text-object final-objects)))
                   (push new-text-macro macros)))))
    ;; at this point we have `final-macro-pairs' and their respective text objects
    ;; but the objects have no notion of parenthood, we need to handle this here
    ;; which is probably not the most efficient way to go about it, as we could
    ;; probably do that while iterating and here we'd doing repetitive work.
    ;; but the algorithm isnt hard to grasp this way, maybe we'll want an improvement
    ;; in the future.
    (if as-tree
        (let ((doc (make-document str1 final-objects)))
          (setf (text-object-children doc) (mapcar (lambda (entry) (text-macro-final-text-object entry)) my-stack))
          doc)
        (mapcar (lambda (entry) (text-macro-final-text-object entry)) my-stack))))

(defun is-macro-contained-in-pair (macro macro-pair)
  (and (> (region-begin (text-macro-region macro))
          (region-begin (text-macro-region (car macro-pair))))
       (< (region-begin (text-macro-region macro))
          (region-begin (text-macro-region (cdr macro-pair))))))

;; this should parse the output of text-object-export to handle dynamic modifications
;; actually this is a behavior that is controlled by REPARSE
;; todo: optimize
(defun export-tree (text-object backend &optional reparse)
  (multiple-value-bind (export-result to-recurse) (text-object-export text-object backend)
    (if to-recurse
        (let ((final-result "")
              (idx 0)
              (children (text-object-sorted-children
                         (if reparse
                             (parse export-result)
                             text-object))))
          (loop for child in children
                for i from 1
                do (let ((child-result (export-tree child backend reparse)))
                     (setf final-result
                           (concatenate 'string
                                        final-result
                                        (subseq export-result
                                                idx
                                                (text-object-begin child))
                                        child-result))
                     (setf idx (text-object-end child))))
          (setf final-result (concatenate 'string final-result (subseq export-result idx)))
          final-result)
        export-result)))