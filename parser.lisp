(defpackage :cl-text
  (:use :cl :cl-ppcre))
(in-package :cl-text)

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
                         (setf (cdr (car my-stack)) my-pos)
                         (push (car my-stack) all-found)
                         (setf my-stack (cdr my-stack))
                         (setf my-pos (+ my-pos (length end))))
                       ;; neither an instance of `begin' nor `end', continue searching
                       (setf my-pos (1+ my-pos))))))
    all-found))

;; INCOMPLETE
(defun find-nested-pairs-regex (str1 begin end &optional (pos 0))
  "similar to `find-nested-pairs', but accepts regexes, returns (:begin :end :text)"
  (let ((my-stack)
        (my-pos pos)
        (all-found))
    (loop while (< my-pos (length str1))
          do (let* ((substr (subseq str1 my-pos))
                    (begin-match-result (cl-ppcre:scan begin substr))
                    (end-match-result (cl-ppcre:scan end substr))
                    ;; where the matched `begin' starts (if at all)
                    (begin-match-begin-idx (car begin-match-result))
                    ;; where the matched `begin' ends (if at all)
                    (begin-match-end-idx (cadr begin-match-result))
                    ;; where the matched `end' starts (if at all)
                    (end-match-begin-idx (car end-match-result))
                    (end-match-end-idx (cadr end-match-result)))
               (if (equal begin-match-begin-idx 0)
                   ;; we found an instance of `begin', push it onto the stack
                   (progn
                     (push (list 'begin my-pos
                                 'end -1
                                 'text (subseq substr begin-match-begin-idx begin-match-end-idx))
                           my-stack)
                     (setf my-pos (+ my-pos 1)))
                   ;; if we didnt detect a `begin', check if we have an instance of `end'
                   (if (equal end-match-begin-idx 0)
                       ;; if stack isnt empty, pull one instance, we found a match, if stack
                       ;; is empty, we found an `end' with no `begin', we ignore it.
                       (when my-stack
                         (setf (getf (car my-stack) ') 45)
                         (setf (cdr ) my-pos)
                         (push (car my-stack) all-found)
                         (setf my-stack (cdr my-stack))
                         (setf my-pos (+ my-pos 1)))
                       ;; neither an instance of `begin' nor `end', continue searching
                       (setf my-pos (1+ my-pos))))))
    all-found))

(defstruct text-macro
  begin-point ;; the point at which the macro text begins
  end-point ;; the point at which the macro text ends
  text ;; the text of the macro
  result ;; the result of macro evaluation
  )

(defstruct text-macro-pair
  open-macro close-macro)

(defun parse (str1)
  "parse a string, returns an object tree.
we apply two passes on the string, the first to detect the locations of the macros and
where their ending macros are (if at all)."
  (let ((macro-pairs))
    ;; first pass
    (let ((continue t)
          (my-stack)
          (idx 0)
          ;; this regex is for detecting macro openings without an odd number of backslahes before them
          (macro-begin-regex
            (format nil "(^|[^\\\\])(\\\\{2})*~A\\(" *text-macro-seq*))
          (macros))
      (setf macros (find-nested-pairs-regex str1 macro-begin-regex "\\)"))
      (format t "got ~a~%" macros)
      ;; (loop while continue
      ;;       do (let* (
      ;;                 (macro-begin (search macro-begin-pattern str1 :test 'equal :start2 idx))
      ;;                 (macro-end))
      ;;            (if macro-begin
      ;;                (progn
      ;;                  (setf macro-end (find-match-forward
      ;;                                   str1
      ;;                                   (+ macro-begin (length macro-begin-pattern))
      ;;                                   "("
      ;;                                   ")"))
      ;;                  (if macro-end
      ;;                      (progn
      ;;                        (let ((macro-text (subseq str1 (1+ macro-begin) (1+ macro-end)))
      ;;                              (result))
      ;;                          (format t "found macro ~a~%" macro-text)
      ;;                          ;; we have detected a macro, try to evaluate it to get an organ-object
      ;;                          (handler-case (setf result (eval (read-from-string macro-text)))
      ;;                            (error (c)
      ;;                              (format t "error while evaluating macro ~A: ~A." macro-text c))
      ;;                            (:no-error (_)
      ;;                              (format t "evaluated macro ~A: ~A" macro-text result))))
      ;;                        (setf idx (+ idx macro-end)))
      ;;                      (setf continue nil)))
      ;;                (setf continue nil))
      ;;            (setq continue (and continue (< idx (length str1))))))
      )
    (let ((macro-pairs))
      )))

(defun find-closing-macro ()
  )