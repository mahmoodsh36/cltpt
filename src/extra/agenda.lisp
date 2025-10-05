(defpackage :cltpt/agenda
  (:use :cl)
  (:export
   :task :make-task :task :agenda-tasks
   :task-state :task-tags :task-title :task-description :task-records
   :from-roamer :task-node :tasks-between
   :task-record :make-task-record :task-record-task :make-repeated-timestamp
   :make-time-range :make-record-scheduled :make-record-timestamp
   :make-record-deadline :render-agenda))

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

;; <2024-11-26 Tue 12:00:00>
;; <2025-02-02 Sun 10:00:00 +1w>--<2025-02-02 Sun 14:00:00 +1w>
(defstruct task-record
  type ;; what do we call the keywords "SCHEDULED", "DEADLINE", etc?
  task
  time)

;; CLOSED: [2024-04-02 Tue 19:27:34] SCHEDULED: <2024-04-02 Tue>
(defstruct (record-scheduled (:include task-record))
  )

;; CLOSED: [2025-09-17 Wed 23:44:42] DEADLINE: <2025-09-13 Sat>
(defstruct (record-deadline (:include task-record))
  )

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

;; (defmethod cltpt/tree:tree-children ((node task))
;;   (task-children node))

;; (defmethod cltpt/tree:tree-parent ((node task))
;;   (task-parent node))

;; (defmethod cltpt/outline:outline-text ((node task))
;;   (format nil "~A: ~A" (task-state node) (task-title node)))

;; (defmethod cltpt/tree:is-subtree ((subtree task) child)
;;   (typep child 'task))

(defun records-between (records begin-ts end-ts)
  (loop for record in records
        for time = (task-record-time record)
        if (is-between time begin-ts end-ts)
          collect record))

(defmethod agenda-records-between ((agn agenda) begin-ts end-ts)
  (loop for task1 in (agenda-tasks agn)
        append (records-between (task-records task1) begin-ts end-ts)))

(defmethod agenda-tasks-between ((agn agenda) begin-ts end-ts)
  "return all tasks that have a `task-record' whose `time' is between BEGIN-TS and END-TS."
  (loop for task1 in (agenda-tasks agn)
        when (records-between (task-records task1) begin-ts end-ts)
          collect task1))

(defstruct agenda-outline-node
  text
  time-range
  children)

(defmethod cltpt/tree:tree-children ((node agenda-outline-node))
  (agenda-outline-node-children node))

;; (defmethod cltpt/outline:outline-text ((node agenda-outline-node))
;;   (let ((date-text
;;           (local-time:format-timestring
;;            nil
;;            (time-range-begin (agenda-outline-node-time-range node))
;;            :timezone local-time:*default-timezone*
;;            :format '(:long-weekday
;;                      #\space
;;                      (:day 2 #\0)
;;                      #\space
;;                      :long-month
;;                      #\space
;;                      :year
;;                      #\space
;;                      (:hour 2 #\0)
;;                      #\:
;;                      (:min 2 #\0)
;;                      ;; #\space
;;                      ;; "W"
;;                      ;; (:iso-week-number 2 #\0)
;;                      ))))
;;     date-text))

(defmethod cltpt/outline:outline-text ((node agenda-outline-node))
  (agenda-outline-node-text node))

(defmethod cltpt/tree:is-subtree ((node agenda-outline-node) (child task-record))
  t)

(defmethod cltpt/outline:outline-text ((rec task-record))
  (let ((task1 (task-record-task rec)))
    (if (deadline rec)
        (format nil "DEADLINE: ~A" (task-title task1))
        (if (start-task rec)
            (format nil "START: ~A" (task-title task1))
            (format nil "~A" (task-title task1))))))

(defmethod cltpt/tree:tree-children ((node task-record))
  nil)

(defmethod render-agenda ((agn agenda) &key begin-ts end-ts)
  (with-output-to-string (out)
    (let* ((agenda-forest))
      (let* ((fully-displayed-days 1)
             (hour-diff 2)
             (begin-ts (if begin-ts
                           (cltpt/base:truncate-to-day begin-ts)
                           (cltpt/base:today-timestamp)))
             (end-ts (or end-ts
                         (cltpt/base:add-duration begin-ts '(:day 14)))))
        (loop for (day . next-day)
                in (cltpt/base:list-date-pairs begin-ts end-ts '(:day 1))
              for i from 0
              do (let ((day-node (make-agenda-outline-node
                                  :time-range (make-time-range
                                               :begin day
                                               :end next-day))))
                   (setf (agenda-outline-node-text day-node)
                         (local-time:format-timestring
                          nil
                          day
                          :format '(:long-weekday
                                    #\space
                                    (:day 2 #\0)
                                    #\space
                                    :long-month)))
                   (if (< i fully-displayed-days)
                       (loop
                         for (hour . next-hour)
                           in (cltpt/base:list-date-pairs
                               day
                               next-day
                               (list :hour hour-diff))
                         for hour-node = (make-agenda-outline-node
                                          :time-range (make-time-range
                                                       :begin hour
                                                       :end next-hour))
                         do (push hour-node (agenda-outline-node-children day-node))
                            (setf (agenda-outline-node-text hour-node)
                                  (local-time:format-timestring
                                   nil
                                   hour
                                   :format '((:hour 2 #\0)
                                             #\:
                                             (:min 2 #\0))))
                            (loop
                              for my-record
                                in (agenda-records-between agn hour next-hour)
                              do (format t "here1 ~A~%" my-record)(push my-record
                                       (agenda-outline-node-children hour-node))))
                       (loop for my-record
                               in (agenda-records-between agn day next-day)
                         do (push my-record
                                  (agenda-outline-node-children day-node))))
                   (push day-node agenda-forest)
                   (setf (agenda-outline-node-children day-node)
                         (nreverse (agenda-outline-node-children day-node))))))
      (setf agenda-forest (nreverse agenda-forest))
      (write-sequence (cltpt/outline:render-forest agenda-forest) out)
      out)))

;; (defmethod text-object-agenda-change-state ((fmt1 cltpt/base:text-format) obj-tree)
;;   )