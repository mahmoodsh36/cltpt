(defpackage :cltpt/agenda
  (:use :cl :cltpt/base)
  (:export :todo :make-todo :todo
   :agenda-todos
   :todo-scheduled :todo-deadline :todo-timed :todo-title :todo-state :todo-tags
   :from-roamer :todo-node
   ))

(in-package :cltpt/agenda)

;; (defstruct duration
;;   start-date
;;   end-date
;;   repeat)

;; (defstruct state
;;   start
;;   name
;;   is-done) ;; done may mean cancelled or archived, etc.. not just DONE

(defstruct todo
  title
  description
  scheduled
  timed ;; timestamp
  deadline
  state ;; a string (keyword)
  state-history
  tags
  node) ;; node refers to a cltpt/roam:node

(defclass agenda ()
  ((todos
   :initform nil
   :initarg :todos
   :accessor agenda-todos
   :documentation "collection of todos in this agenda.")))

(defmethod todos-between ((agn agenda) begin-date end-date)
  (let ((results))
    ))

(defmethod from-roamer ((rmr cltpt/roam:roamer))
  "takes a cltpt/roam:roamer RMR and constructs an agenda object from it.
the new agenda object will contain all the todos found in the nodes of the roamer."
  (let ((results))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for obj = (cltpt/roam:node-text-obj node)
          for todo = (text-object-agenda-data obj)
          do
             (when todo
               (setf (todo-node todo) node)
               (push todo results)))
    (make-instance 'agenda :todos results)))

(defun is-between-timestamps (ts ts-begin ts-end)
  (and (local-time:timestamp>= ts ts-begin)
       (local-time:timestamp<= ts ts-end)))

;; receives an agenda and timestamps for end/begin
(defmethod todos-between ((agn agenda) ts-begin ts-end)
  (let ((results))
    (loop for todo in (agenda-todos agn)
          do (let ((candidates (list (todo-scheduled todo)
                                     (todo-deadline todo)
                                     (todo-timed todo))))
               (loop for candidate in candidates
                     do (when (is-between-timestamps
                               candidate
                               ts-begin
                               ts-end)
                          (push todo results)
                          (return)))))
    results))

(defmethod text-object-agenda-data ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :todo))

(defmethod text-object-agenda-change-state ((fmt1 cltpt/base:text-format) obj-tree)
  )