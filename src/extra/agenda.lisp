(defpackage :cltpt/agenda
  (:use :cl)
  (:export
   :task :make-task :task :agenda-tasks
   :task-state :task-tags :task-title :task-description :task-records
   :from-roamer :task-node :tasks-between
   :task-record :make-task-record :task-record-task :make-repeated-timestamp
   :make-time-range))

(in-package :cltpt/agenda)

(defvar *done-states*
  '(done cancelled cancel close))

;; <2025-02-02 Sun 10:00:00 +1w>
(defstruct repeated-timestamp
  time ;; could be a time-range or a timestamp
  repeat)

;; <2025-02-02 Sun 10:00:00 +1w>--<2025-02-02 Sun 14:00:00 +1w>
(defstruct time-range
  begin
  end)

(defstruct task-record
  type ;; what do we call the keywords "SCHEDULED", "DEADLINE", etc?
  task
  time)

;; CLOSED: [2024-04-02 Tue 19:27:34] SCHEDULED: <2024-04-02 Tue>
;; (defstruct record-scheduled
;;   time)

;; <2024-11-26 Tue 12:00:00>
;; (defstruct record-timestamp
;;   time)

;; <2025-02-02 Sun 10:00:00 +1w>--<2025-02-02 Sun 14:00:00 +1w>
;; (defstruct record-time-range
;;   repeated-ts)

;; CLOSED: [2025-09-17 Wed 23:44:42] DEADLINE: <2025-09-13 Sat>
;; (defstruct record-deadline
;;   time)

(defgeneric deadline (record)
  (:documentation "a record that behaves as a deadline should return a timestamp as a deadline."))

(defgeneric start-task (record)
  (:documentation "a record that behaves as a \"when to start\" should return a timestamp."))

(defgeneric end-task (record)
  (:documentation "a record that behaves as a \"when to end\" should return a timestamp."))

(defgeneric repeat-task (record time-range)
  (:documentation "a repetitive `task-record' should return as many instances as it needs for the given time range."))

(defgeneric is-between (time begin-time end-time)
  (:documentation "given 3 timestamps, check whether the first is between the other 2."))

(defmethod is-between ((time local-time:timestamp)
                       (begin local-time:timestamp)
                       (end local-time:timestamp))
  (and (local-time:timestamp>= time begin)
       (local-time:timestamp<= time end)))

(defmethod is-between ((time time-range)
                       (begin local-time:timestamp)
                       (end local-time:timestamp))
  (and (is-between (time-range-begin time) begin end)
       (is-between (time-range-end time) begin end)))

(defmethod is-between ((time repeated-timestamp)
                       (begin local-time:timestamp)
                       (end local-time:timestamp))
  (and (local-time:timestamp>= (repeated-timestamp-time time) begin)
       (local-time:timestamp<= (repeated-timestamp-time time) end)))

(defstruct task
  title
  description
  records ;; list of task-record
  state ;; a symbol
  tags
  node ;; node refers to a cltpt/roam:node
  children
  parent)

(defclass agenda ()
  ((tasks
   :initform nil
   :initarg :tasks
   :accessor agenda-tasks
   :documentation "collection of tasks in this agenda.")))

(defmethod from-roamer ((rmr cltpt/roam:roamer))
  "takes a cltpt/roam:roamer RMR and constructs an agenda object from it.
the new agenda object will contain all the tasks found in the nodes of the roamer."
  (let ((results))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for obj = (cltpt/roam:node-text-obj node)
          for task = (text-object-agenda-data obj)
          do (when task
               (setf (task-node task) node)
               (push task results)))
    ;; set parents and children according to the hierarchy of text objects
    ;; from which they were extracted.
    (loop for task in results
          do (loop for other-task in results
                   do (let* ((task-node (task-node task))
                             (task-text-obj (cltpt/roam:node-text-obj task-node))
                             (other-task-node (task-node other-task))
                             (other-task-text-obj
                               (cltpt/roam:node-text-obj other-task-node))
                             (is-parent (eq (cltpt/tree:tree-parent task-text-obj)
                                            other-task-text-obj)))
                        (when is-parent
                          (setf (task-parent task) other-task)
                          (push task (task-children other-task))))))
    (make-instance 'agenda :tasks results)))

(defmethod text-object-agenda-data ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :task))

(defmethod cltpt/tree:tree-children ((node task))
  (task-children node))

(defmethod cltpt/tree:tree-parent ((node task))
  (task-parent node))

(defmethod cltpt/outline:outline-text ((node task))
  (format nil "task: ~A" (task-title node)))

;; (defmethod cltpt/tree:is-subtree ((subtree task) child)
;;   (typep child 'task))

(defmethod tasks-between ((agn agenda) begin-ts end-ts)
  "return all tasks that have a `task-record' whose `time' is between BEGIN-TS and END-TS."
  (let ((results))
    (loop for task1 in (agenda-tasks agn)
          do (loop for record in (task-records task1)
                   for time = (task-record-time record)
                   do (when (is-between time begin-ts end-ts)
                        (push task1 results)
                        (return))))
    results))

;; (defmethod text-object-agenda-change-state ((fmt1 cltpt/base:text-format) obj-tree)
;;   )