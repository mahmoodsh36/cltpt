(defpackage :cltpt/agenda
  (:use :cl)
  (:import-from
   :cltpt/agenda/state
   :state-name :state-is-terminal :make-state :state-by-name
   :state-desc-name :state-desc-is-terminal
   :make-state-desc :make-state-sequence-desc
   :state-sequence-desc :state-sequence-desc-state-descs :cycle)
  (:import-from
   :cltpt/agenda/time
   :time-range :make-time-range :time-range-begin
   :time-range-end :is-between)
  (:import-from
   :cltpt/agenda/task
   :make-record-scheduled :make-record-deadline
   :task :make-task :task :agenda-tasks
   :task-state :task-tags :task-title :task-description :task-records
   :task-record :make-task-record :task-record-task :task-parent :task-children
   :task-record-repeat :task-record-time

   :repeat-task :deadline :start-task

   :task-node
   :text-object-task)
  (:export
   :task :make-task :task :agenda-tasks
   :task-state :task-tags :task-title :task-description :task-records
   :task-record :make-task-record :task-record-task :task-parent :task-children
   :task-record-repeat :task-record-time

   :from-roamer :task-node :tasks-between
   :make-time-range :make-record-scheduled :make-record-deadline
   :make-record-timestamp :make-record-deadline :render-agenda
   :build-agenda-forest :agenda-outline-node :agenda-outline-node-expansion-state

   :state-name :state-is-terminal :make-state :state-by-name
   :state-desc-name :state-desc-is-terminal
   :make-state-desc :make-state-sequence-desc
   :state-sequence-desc :state-sequence-desc-state-descs :cycle

   :text-object-task

   :*agenda-time-format*

   :cycle :state-by-name))

(in-package :cltpt/agenda)

(defvar *agenda-time-format*
  '((:hour 2 #\0)
    #\:
    (:min 2 #\0))
  "time format displayed in agenda trees.")

(defvar *agenda-seqs*
  (list
   (make-state-sequence-desc
    :name 'basic
    :state-descs (list
                  (make-state-desc
                   :name 'todo
                   :is-terminal nil)
                  (make-state-desc
                   :name 'done
                   :is-terminal t)))))

;; this grabs a state by its name, its not very smart and it cant tell the
;; difference between two states of the same name from different sequences.
;; but this is the way that org-mode does it so we use that for now.
(defun state-by-name (name)
  (loop for seq-desc in *agenda-seqs*
        do (loop for state-desc in (state-sequence-desc-state-descs seq-desc)
                 do (when (string= (state-desc-name state-desc) name)
                      (return-from state-by-name
                        (make-state :desc state-desc
                                    :sequence-desc seq-desc))))))

(defvar *agenda-include-done*
  t
  "whether to include agenda nodes that are in one of the `*done-states*'.")

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
          for task = (text-object-task obj)
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

(defun records-between (records begin-ts end-ts)
  (loop for record in records
        append (let ((new-records
                       (repeat-task record
                                    (make-time-range :begin begin-ts
                                                     :end end-ts))))
                 (if new-records
                     (loop for new-record in new-records
                           when (is-between (task-record-time new-record)
                                            begin-ts end-ts)
                             collect new-record)
                     (and (is-between (task-record-time record) begin-ts end-ts)
                          (list record))))))

(defmethod agenda-records-between ((agn agenda) begin-ts end-ts)
  (loop for task1 in (agenda-tasks agn)
        ;; we dont include a task if its DONE (in a terminal state), unless
        ;; `*agenda-include-done*' is set to `t'
        append (unless (and (state-is-terminal (task-state task1))
                            (not *agenda-include-done*))
                 (records-between (task-records task1) begin-ts end-ts))))

(defmethod agenda-tasks-between ((agn agenda) begin-ts end-ts)
  "return all tasks that have a `task-record' whose `time' is between BEGIN-TS and END-TS."
  (loop for task1 in (agenda-tasks agn)
        when (records-between (task-records task1) begin-ts end-ts)
          collect task1))

(defstruct agenda-outline-node
  parent
  text
  time-range
  children
  expansion-state)

(defmethod cltpt/tree:tree-children ((node agenda-outline-node))
  (case (agenda-outline-node-expansion-state node)
    (expanded
     (agenda-outline-node-children node))
    (t (loop for child in (agenda-outline-node-children node)
             append (if (typep child 'agenda-outline-node)
                        (cltpt/tree:tree-children child)
                        (list child))))))

(defmethod cltpt/tree:has-children ((node agenda-outline-node))
  ;; if it has another agenda-outline-node as a child then it should be expandable
  ;; if it has any tasks as children then it also is expandable/collapsable
  (loop for child in (agenda-outline-node-children node)
        thereis (or (typep child 'agenda-outline-node)
                    (typep child 'task-record))))

(defmethod cltpt/tree:tree-parent ((node agenda-outline-node))
  (agenda-outline-node-parent node))

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

(defmethod cltpt/tree/outline:outline-text ((node agenda-outline-node))
  (agenda-outline-node-text node))

(defmethod cltpt/tree:is-subtree ((node agenda-outline-node)
                                  (child task-record))
  t)

;; without this printing a node might cause an infinite loop
(defmethod print-object ((obj agenda-outline-node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "-> agenda-outline-node with ~A children."
            (length (cltpt/tree:tree-children obj)))))

(defmethod build-agenda-forest ((agn agenda) &key begin-ts end-ts)
  "build a forest representing the data of AGN for the given dates.

the returned list of trees should be implemented using the `cltpt/tree' and `cltpt/tree/outline' interface."
  (let* ((agenda-forest))
    (let* ((fully-displayed-days 1)
           (hour-diff 2)
           (begin-ts (if begin-ts
                         (cltpt/base:truncate-to-day begin-ts)
                         (cltpt/base:today-timestamp)))
           (end-ts (or end-ts
                       (cltpt/base:add-duration begin-ts '(:day 7)))))
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
                      (setf (agenda-outline-node-parent hour-node) day-node)
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
                        do (push my-record
                                 (agenda-outline-node-children hour-node))))
                 (when (< i fully-displayed-days)
                   (setf (agenda-outline-node-expansion-state day-node)
                         'expanded))
                 (push day-node agenda-forest)
                 (setf (agenda-outline-node-children day-node)
                       (reverse (agenda-outline-node-children day-node))))))
    (nreverse agenda-forest)))

(defmethod render-agenda ((agn agenda) &key begin-ts end-ts)
  (with-output-to-string (out)
    (let* ((agenda-forest (build-agenda-forest agn
                                               :begin-ts begin-ts
                                               :end-ts end-ts)))
      (write-sequence (cltpt/tree/outline:render-forest agenda-forest) out)
      out)))

(defmethod cltpt/tree/outline:outline-text ((rec task-record))
  (labels ((format-ts (ts)
             (local-time:format-timestring
              nil
              ts
              :format *agenda-time-format*))
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