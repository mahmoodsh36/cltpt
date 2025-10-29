(defpackage :cltpt/agenda/task
  (:use :cl)
  (:import-from :cltpt/agenda/time
   :time-range :make-time-range :time-range-begin :time-range-end)
  (:import-from :cltpt/agenda/state
   :state-name)
  (:export
   :task :make-task :task :agenda-tasks

   :task-tags :task-title :task-description :task-records
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
    ;; remove any increments of value 0 because those are irrelevant and problematic
    (setf repeat
          (loop for (key value) on repeat by #'cddr
                unless (zerop value)
                  append (list key value)))
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

;; without this printing a node might cause an infinite loop
(defmethod print-object ((obj task-record) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "-> ~A."
            (cltpt/tree/outline:outline-text obj))))

(defmethod cltpt/tree/outline:outline-text ((rec task-record))
  (labels ((format-ts (ts)
             (local-time:format-timestring
              nil
              ts
              :format '((:hour 2 #\0)
                        #\:
                        (:min 2 #\0))))
           (format-time (time)
             (if (typep time 'time-range)
                 (format nil "~A--~A"
                         (format-ts (time-range-begin time))
                         (format-ts (time-range-end time)))
                 (format-ts time))))
    (let ((task1 (task-record-task rec)))
      (if (deadline rec)
          (format nil
                  "DEADLINE: ~A ~A"
                  (state-name (task-state task1))
                  (format-time (task-record-time rec))
                  (task-title task1))
          (if (start-task rec)
              (format nil
                      "START: (~A) ~A ~A"
                      (state-name (task-state task1))
                      (format-time (task-record-time rec))
                      (task-title task1))
              (format nil
                      "~A (~A) ~A"
                      (state-name (task-state task1))
                      (format-time (task-record-time rec))
                      (task-title task1)))))))

(defmethod cltpt/tree:tree-children ((node task-record))
  nil)