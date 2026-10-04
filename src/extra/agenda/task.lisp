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
   :task-record-repeat :task-record-time :task-record-generated-p
   :make-record-scheduled

   :repeat-task :deadline :start-task
   :task-node :text-object-task
   :task-last-repeat :task-repeating-p))

(in-package :cltpt/agenda/task)

(defstruct task-record
  ;; t for instances made by repeat-task
  generated-p
  task
  time
  ;; repeat interval plist like (:day 1), (:week 2) or (:hour 3), nil when not repeating
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
  parent
  ;; timestamp from :LAST_REPEAT: property, repeated entries on or before this are ignored
  last-repeat)

;; it might look redundant that we're explicitly checking whether some repeat plist has a positive
;; value but its not, in org, having a 0 increment is supported and implies the increment is
;; 'disabled', and it happens when you permanently cancel a repeating timestamp.
;; e.g. the following timestamp shouldnt be interpretered as a repeating one.
;; <2025-05-05 Mon 13:00-16:00 +0w>
(defun task-repeating-p (task)
  (some
   (lambda (rec)
     (loop for (nil value) on (task-record-repeat rec) by #'cddr
           thereis (and value (plusp value))))
   (task-records task)))

(defmethod text-object-task ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :task))

(defgeneric deadline (record)
  (:documentation "a record that behaves as a deadline should return a timestamp as a deadline."))

(defgeneric start-task (record)
  (:documentation "a record that behaves as a \"when to start\" should return a timestamp."))

(defgeneric repeat-task (record time-range)
  (:documentation "a repetitive `task-record' should return as many instances as it needs for the given time range."))

(defmethod deadline ((rec record-deadline))
  (task-record-time rec))

;; only `record-deadline' sets a deadline, atleast for now.
(defmethod deadline ((rec t))
  nil)

(defmethod start-task ((rec record-scheduled))
  (task-record-time rec))

;; only `record-scheduled' sets a 'start', atleast for now.
(defmethod start-task ((rec t))
  nil)

(defmethod repeat-task ((rec task-record) (rng time-range))
  (let* ((time (task-record-time rec))
         (repeat (task-record-repeat rec))
         (time-begin (if (typep time 'time-range)
                         (time-range-begin time)
                         time))
         (time-end (and (typep time 'time-range) (time-range-end time)))
         (increment (when time-end
                      (local-time:timestamp-difference time-end time-begin)))
         (last-repeat (when (task-record-task rec)
                        (task-last-repeat (task-record-task rec)))))
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
              ;; skip entries on or before the LAST_REPEAT date
              unless (and last-repeat
                          (local-time:timestamp<= date1 last-repeat))
                ;; copy so the dupe keeps its struct type (record-deadline etc)
                collect (let ((dupe (copy-structure rec)))
                          (setf (task-record-generated-p dupe) t
                                (task-record-time dupe) (if date2
                                                            (make-time-range :begin date1
                                                                             :end date2)
                                                            date1)
                                (task-record-repeat dupe) nil)
                          dupe))))))

;; without this printing a node might cause an infinite loop
(defmethod print-object ((obj task-record) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "-> ~A."
            (cltpt/tree/outline:outline-text obj))))

(defmethod cltpt/tree:tree-children ((node task-record))
  nil)