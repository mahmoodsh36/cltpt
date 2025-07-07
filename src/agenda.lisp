(defpackage :cltpt/agenda
  (:use :cl :cltpt/base)
  (:export :todo :make-todo))

(in-package :cltpt/agenda)

(defstruct todo
  title
  description
  scheduled
  deadline
  state
  tags
  children
  parent
  src-text-obj)

(defclass agenda ()
  ((todos
   :initform nil
   :accessor agenda-todos
   :documentation "collection of todos in this agenda.")))

(defmethod agenda-between ((agn agenda) begin-date end-date)
  )

(defmethod agenda-today ((agn agenda))
  )

(defmethod text-object-agenda-data ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :todo))

(defun collect-todos (obj-tree)
  (labels ((my-lambda (obj)
             (text-object-agenda-data obj)))
    (remove-if-not
     'identity
     (cltpt/base::flatten
      (cltpt/base::map-text-object obj-tree #'my-lambda)))))

(defmethod text-object-agenda-change-state ((fmt1 cltpt/base:text-format) obj-tree)
  )