(in-package :cltpt/base)

(defun last-atom (seq)
  (car (last seq)))

(defun ensure-directory (dir)
  (unless (probe-file dir)
    (uiop:ensure-directory-pathname dir)))

(defun md5-str (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (ironclad:ascii-string-to-byte-array s))))

;; this is a temporary workaround because `find-class' is really slow..
(defparameter *class-map* (make-hash-table))
(defun find-class-faster (class-sym)
  (let ((result (gethash class-sym *class-map*)))
    (unless result
      (setf result (find-class class-sym))
      (setf (gethash class-sym *class-map*) result))
    result))

(defun bind-and-eval (bindings func)
  "dynamically binds symbols from BINDINGS (a list of symbol,value pairs) and executes FUNC."
  (let ((keys (mapcar #'car bindings))
        (values (mapcar #'eval (mapcar #'cadr bindings))))
    (progv keys values
      (funcall func))))
(defun bind-and-eval* (bindings func)
  (if (null bindings)
      (funcall func)
    (destructuring-bind (sym val-expr) (car bindings)
      (progv (list sym) (list (eval val-expr))
        (bind-and-eval* (cdr bindings) func)))))
;; example
;; (bind-and-eval '((x 1) (y (+ 1 2))) (lambda () (+ x y)))
;; (bind-and-eval* '((x 1) (y (+ x 2))) (lambda () (+ x y)))

(defmacro pcase (keyform &body clauses)
  "a case matcher that can take variables, like elisp's `pcase'.

example usage: `(let ((myvar 'latex)) (pcase 'latex ('html 1) (myvar 2)))'"
  (let ((keyval-gensym (gensym "pcase")))
    `(let ((,keyval-gensym ,keyform))
       (cond
         ,@(loop for clause in clauses
                 if (member (car clause) '(t otherwise) :test #'eq)
                   collect `(t ,@(cdr clause))
                 else
                   collect (let ((test-form (car clause))
                                 (body (cdr clause)))
                             `((eql ,keyval-gensym ,test-form)
                               ,@body)))))))

(defun plistp (list1)
  (keywordp (car list1)))

(defun flatten (l depth)
  "flatten L by at most DEPTH levels."
  (and l
       (if (zerop depth)
           l
           (if (atom (car l))
               (cons (car l) (flatten (cdr l) depth))
               (append (flatten (car l) (1- depth))
                       (flatten (cdr l) depth))))))