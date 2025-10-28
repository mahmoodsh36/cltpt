(defpackage :cltpt/agenda/task
  (:use :cl)
  (:import-from :cltpt/agenda/time
   :time-range :make-time-range :time-range-begin :time-range-end)
  (:export
   :task :make-task :task :agenda-tasks

   :task-state :task-tags :task-title :task-description :task-records
   :task-record :make-task-record :task-record-task :task-parent :task-children
   :task-record-repeat :task-record-time
   :make-record-scheduled

   :repeat-task :deadline :start-task
   :task-node :text-object-task))

(in-package :cltpt/agenda/task)

(defstruct task-record
  type ;; what do we call the keywords "SCHEDULED", "DEADLINE", etc?
  task
  time
  repeat)

;; CLOSED: [2024-04-02 Tue 19:27:34] SCHEDULED: <2024-04-02 Tue>
(defstruct (record-scheduled (:include task-record))
  )

;; CLOSED: [2025-09-17 Wed 23:44:42] DEADLINE: <2025-09-13 Sat>
(defstruct (record-deadline (:include task-record))
  )

(defstruct task
  title
  description
  records ;; list of task-record
  state
  ;; state-history
  tags
  node ;; node refers to a cltpt/roam:node
  children
  parent)

(defmethod text-object-task ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :task))

;; (defmethod cltpt/tree:tree-children ((node task))
;;   (task-children node))

;; (defmethod cltpt/tree:tree-parent ((node task))
;;   (task-parent node))

;; (defmethod cltpt/outline:outline-text ((node task))
;;   (format nil "~A: ~A" (task-state node) (task-title node)))

;; (defmethod cltpt/tree:is-subtree ((subtree task) child)
;;   (typep child 'task))

(defgeneric deadline (record)
  (:documentation "a record that behaves as a deadline should return a timestamp as a deadline."))

(defgeneric start-task (record)
  (:documentation "a record that behaves as a \"when to start\" should return a timestamp."))

(defgeneric end-task (record)
  (:documentation "a record that behaves as a \"when to end\" should return a timestamp."))

(defgeneric repeat-task (record time-range)
  (:documentation "a repetitive `task-record' should return as many instances as it needs for the given time range."))

(defmethod deadline ((rec record-deadline))
  (task-record-time rec))

;; only `record-deadline' sets a deadline, atleast for now.
(defmethod deadline ((rec t))
  nil)

(defmethod start-task ((rec record-scheduled))
  (task-record-time rec))

(defmethod start-task ((rec task-record))
  (task-record-time rec))

(defmethod repeat-task ((rec task-record) (rng time-range))
  (let* ((time (task-record-time rec))
         (repeat (task-record-repeat rec))
         (time-begin (if (typep time 'time-range)
                         (time-range-begin time)
                         time))
         (time-end (and (typep time 'time-range) (time-range-end time)))
         (increment (when time-end
                      (local-time:timestamp-difference time-end time-begin))))
    (when repeat
      (let ((dates1 (cltpt/base:list-dates time-begin
                                           (time-range-end rng)
                                           repeat))
            (dates2 (when time-end
                      (cltpt/base:list-dates time-end
                                             (time-range-end rng)
                                             repeat))))
        (loop for date1 in dates1
              for date2 = (when increment
                            (local-time:timestamp+ date1 increment :sec))
              collect (make-task-record
                       :type 'dupe
                       :task (task-record-task rec)
                       :time (if date2
                                 (make-time-range :begin date1
                                                  :end date2)
                                 date1)))))))