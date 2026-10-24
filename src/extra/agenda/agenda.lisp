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
   :task-record-repeat :task-record-time :task-record-generated-p :task-record-date-only-p

   :repeat-task :deadline :start-task

   :task-node
   :text-object-task
   :task-last-repeat :task-repeating-p :task-undated-p)
  (:export
   :task :make-task :task :agenda-tasks
   :task-state :task-tags :task-title :task-description :task-records
   :task-record :make-task-record :task-record-task :task-parent :task-children
   :task-record-repeat :task-record-time :task-record-generated-p :task-record-date-only-p
   :task-last-repeat :task-repeating-p

   :from-roamer :task-node :tasks-between
   :make-time-range :time-range-begin :time-range-end :time-range
   :make-record-scheduled :make-record-deadline
   :make-record-timestamp :make-record-deadline :render-agenda
   :build-agenda-forest :agenda-outline-node :agenda-outline-node-expansion-state
   :agenda-outline-node-parent
   :agenda-overdue-node :agenda-overdue-node-record :agenda-overdue-node-days
   :agenda-undated-node :agenda-undated-node-task
   :agenda-node-task

   :state-name :state-is-terminal :make-state :state-by-name
   :state-desc-name :state-desc-is-terminal
   :make-state-desc :make-state-sequence-desc
   :state-sequence-desc :state-sequence-desc-state-descs
   :cycle

   :repeat-task :deadline :start-task

   :text-object-task

   :*agenda-time-format*
   :*agenda-json-time-format*
   :*agenda-seqs*
   :make-state-desc :make-state-sequence-desc))

(in-package :cltpt/agenda)

(defvar *agenda-time-format*
  '((:hour 2 #\0)
    #\:
    (:min 2 #\0))
  "time format displayed in agenda trees.")

(defvar *agenda-day-time-format*
  '(:long-weekday
    #\space
    (:day 2 #\0)
    #\space
    :long-month
    #\space
    :year)
  "time format displayed in agenda trees for day entries.")

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
                   :is-terminal t))))
  "list of state-sequence definitions that are used to determine agenda state changing behavior.")

(defvar *agenda-json-time-format*
  local-time:+iso-8601-format+
  "time format for begin/end in agenda json output.")

(defclass agenda ()
  ((tasks
    :initform nil
    :initarg :tasks
    :accessor agenda-tasks
    :documentation "collection of tasks in this agenda.")))

;; an overdue node is simply a task-record that 'records' (points to) an overdue task.
(defstruct agenda-overdue-node
  record
  days)

;; the node for a task that has no timestamp (displayed at the end of the tree of the first node in
;; the agenda forest)
(defstruct agenda-undated-node
  task)

(defstruct agenda-outline-node
  parent
  text
  time-range
  children
  expansion-state)

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

(defun text-obj-has-tag-in-ancestors-p (text-obj include-tags)
  (or (intersection (cltpt/base:text-object-property text-obj :tags)
                    include-tags
                    :test 'equal)
      (let ((parent (cltpt/tree:tree-parent text-obj)))
        (when parent
          (text-obj-has-tag-in-ancestors-p parent include-tags)))))

(defun task-has-tag-in-ancestors-p (task include-tags)
  (text-obj-has-tag-in-ancestors-p
   (cltpt/roam:node-text-obj (task-node task))
   include-tags))

(defmethod from-roamer ((rmr cltpt/roam:roamer) &key include-tags exclude-tags)
  "takes a cltpt/roam:roamer RMR and constructs an agenda object from it.
the new agenda object will contain all the tasks found in the nodes of the roamer.

INCLUDE-TAGS: only include tasks that correspond to a text-object that has some ancestor that
contains atleast one of the given tags.
EXCLUDE-TAGS: drop tasks that correspond to a text-object that has some ancestor containing atleast
one of the given tags. exclusion wins over inclusion."
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
    (when include-tags
      (setf results
            (remove-if-not
             (lambda (task)
               (task-has-tag-in-ancestors-p task include-tags))
             results)))
    (when exclude-tags
      (setf results
            (remove-if
             (lambda (task)
               (task-has-tag-in-ancestors-p task exclude-tags))
             results)))
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

(defmethod agenda-records-between ((agn agenda) begin-ts end-ts &key include-done)
  (loop for task1 in (agenda-tasks agn)
        ;; we dont include a task if its DONE (in a terminal state), unless
        ;; `include-done' is set to `t'
        append (unless (and (state-is-terminal (task-state task1))
                            (not include-done))
                  (records-between (task-records task1) begin-ts end-ts))))

(defun task-record-effective-start (record)
  "return the timestamp by which RECORD should be ordered/compared.
for ranged times this is the begin of the range."
  (let ((time (task-record-time record)))
    (if (typep time 'time-range)
        (time-range-begin time)
        time)))

(defun task-record-repeating-p (record)
  "whether RECORD has a positive repeat interval. like `task-repeating-p' but per-record."
  (let ((repeat (task-record-repeat record)))
    (loop for (nil value) on repeat by #'cddr
          thereis (plusp value))))

(defmethod agenda-overdue-records ((agn agenda) begin-ts &key include-done)
  "return non-repeating DEADLINE/SCHEDULED records with time before BEGIN-TS, oldest first."
  (let ((overdue
          (loop for task1 in (agenda-tasks agn)
                unless (and (state-is-terminal (task-state task1))
                            (not include-done))
                append (loop for rec in (task-records task1)
                             when (and (or (deadline rec) (start-task rec))
                                       (not (task-record-repeating-p rec))
                                       (local-time:timestamp< (task-record-effective-start rec)
                                                              begin-ts))
                             collect rec))))
    (sort overdue #'local-time:timestamp< :key #'task-record-effective-start)))

(defmethod agenda-undated-tasks ((agn agenda) &key include-done)
  "return tasks with no timestamp records."
  (loop for task in (agenda-tasks agn)
        when (and (task-undated-p task)
                  (or include-done
                      (not (state-is-terminal (task-state task)))))
        collect task))

(defgeneric agenda-node-task (node)
  (:documentation "the task behind agenda display NODE, or nil for nodes with no task (e.g. day headers)."))

(defmethod agenda-node-task ((rec task-record))
  (task-record-task rec))

(defmethod agenda-node-task ((node agenda-overdue-node))
  (task-record-task (agenda-overdue-node-record node)))

(defmethod agenda-node-task ((node agenda-undated-node))
  (agenda-undated-node-task node))

(defmethod agenda-node-task ((node t))
  nil)

(defun record-overdue-days (record timestamp)
  "whole days between RECORD's effective start and TIMESTAMP."
  (round (/ (local-time:timestamp-difference
             (cltpt/base:truncate-to-day timestamp)
             (cltpt/base:truncate-to-day (task-record-effective-start record)))
            local-time:+seconds-per-day+)))

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
        thereis (typep child
                       '(or
                         agenda-outline-node
                         agenda-overdue-node
                         agenda-undated-node
                         task-record))))

(defmethod cltpt/tree:tree-parent ((node agenda-outline-node))
  (agenda-outline-node-parent node))

(defmethod cltpt/tree/outline:outline-text ((node agenda-outline-node))
  (agenda-outline-node-text node))

(defmethod cltpt/tree:is-subtree ((node agenda-outline-node)
                                  (child task-record))
  t)

(defmethod cltpt/tree:is-subtree ((node agenda-outline-node)
                                  (child agenda-overdue-node))
  t)

(defmethod cltpt/tree:is-subtree ((node agenda-outline-node)
                                  (child agenda-undated-node))
  t)

;; without this printing a node might cause an infinite loop
(defmethod print-object ((obj agenda-outline-node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "-> agenda-outline-node with ~A children."
            (length (cltpt/tree:tree-children obj)))))

(defmethod build-agenda-forest ((agn agenda)
                                &key
                                begin-ts end-ts
                                first-repeat-only
                                today
                                include-done (include-undated t))
  "build a forest representing the data of AGN for the given dates.

the returned list of trees should be implemented using the `cltpt/tree' and `cltpt/tree/outline' interface.

BEGIN-TS: start of the date range (defaults to today, truncated to day).
END-TS: end of the date range (defaults to 7 days after BEGIN-TS).
FIRST-REPEAT-ONLY: when non-nil, show only the first occurrence of each repeating task in the range.
INCLUDE-DONE: when non-nil, include tasks that are in a terminal (done) state.
TODAY: the day overdue entries are listed under, defaults to the current day.
INCLUDE-UNDATED: when non-nil (default), show tasks that have no timestamp records (no SCHEDULED,
DEADLINE or active timestamp). they appear as children of the first displayed day's entry."
  (let* ((agenda-forest)
         ;; tracks tasks that have already had a repeat instance shown
         (seen-repeat-tasks (make-hash-table)))
    (labels ((filtered-records-between (begin end)
               "records between BEGIN and END, filtered by FIRST-REPEAT-ONLY."
               (loop for my-record
                       in (agenda-records-between agn begin end :include-done include-done)
                     when (or (not first-repeat-only)
                              (not (task-record-generated-p my-record))
                              (not (gethash (task-record-task my-record) seen-repeat-tasks)))
                       collect (progn
                                 (when (task-record-generated-p my-record)
                                   (setf (gethash (task-record-task my-record) seen-repeat-tasks)
                                         t))
                                 my-record))))
      (let* ((fully-displayed-days 1)
             (hour-diff 2)
             (begin-ts (if begin-ts
                           (cltpt/base:truncate-to-day begin-ts)
                           (cltpt/base:today-timestamp)))
             (end-ts (or end-ts
                         (cltpt/base:add-duration begin-ts '(:day 7))))
             (today (if today
                        (cltpt/base:truncate-to-day today)
                        (cltpt/base:today-timestamp)))
             (overdue-nodes
               (mapcar
                (lambda (rec)
                  (make-agenda-overdue-node
                   :record rec
                   :days (record-overdue-days rec today)))
                (agenda-overdue-records agn today :include-done include-done)))
             (undated-nodes
               (mapcar (lambda (task1)
                         (make-agenda-undated-node :task task1))
                       (agenda-undated-tasks agn :include-done include-done))))
        (loop for (day . next-day)
                in (cltpt/base:list-date-pairs begin-ts end-ts '(:day 1))
              for i from 0
              do (let ((day-node (make-agenda-outline-node
                                  :time-range (make-time-range
                                               :begin day
                                               :end next-day)))
                       (date-only-records))
                   (setf (agenda-outline-node-text day-node)
                         (local-time:format-timestring nil day :format *agenda-day-time-format*))
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
                        (loop for my-record in (filtered-records-between hour next-hour)
                              do (if (task-record-date-only-p my-record)
                                     (push my-record date-only-records)
                                     (push my-record
                                           (agenda-outline-node-children hour-node)))))
                   (when (< i fully-displayed-days)
                     (setf (agenda-outline-node-expansion-state day-node)
                           'expanded))
                   (push day-node agenda-forest)
                   ;; untimed entries go after the time grid
                   (setf (agenda-outline-node-children day-node)
                         (append (reverse (agenda-outline-node-children day-node))
                                 (reverse date-only-records)
                                 (when (local-time:timestamp= day today)
                                   overdue-nodes)
                                 (when (and include-undated (zerop i))
                                   undated-nodes)))))))
    (nreverse agenda-forest)))

(defmethod render-agenda ((agn agenda) &key begin-ts end-ts first-repeat-only include-done today (include-undated t) style)
  "render AGN as a string agenda tree.

BEGIN-TS, END-TS, FIRST-REPEAT-ONLY, INCLUDE-DONE, TODAY, INCLUDE-UNDATED: see `build-agenda-forest'.
 STYLE: a render-style object from cltpt/tree/outline (e.g., *lsblk-style*, *json-style*)"
  (with-output-to-string (out)
    (let* ((agenda-forest (build-agenda-forest agn
                                               :begin-ts begin-ts
                                               :end-ts end-ts
                                               :first-repeat-only first-repeat-only
                                               :include-done include-done
                                               :today today
                                               :include-undated include-undated))
           (style-obj (or style cltpt/tree/outline:*lsblk-style*))
           (rendered (cltpt/tree/outline:render-outline agenda-forest style-obj)))
      (write-sequence rendered out)
      out)))

(defun format-agenda-tags (tags)
  "format TAGS list as \":tag1:tag2:\" or \"\" when empty."
  (if tags
      (format nil ":~{~A~^:~}:" tags)
      ""))

(defun pad-agenda-tags (text tags &optional (target-col 72))
  "pad TEXT so TAGS ends at TARGET-COL."
  (if (string= tags "")
      text
      (let* ((padding (max 1 (- target-col (length text) (length tags)))))
        (format nil
                "~A~A~A"
                text
                (make-string padding :initial-element #\space)
                tags))))

(defun record-outline-text (rec &optional overdue-days)
  "outline line for REC, OVERDUE-DAYS swaps the DEADLINE/SCHEDULED leader for an overdue one."
  (labels ((format-time-from-ts (ts)
             (local-time:format-timestring
              nil
              ts
              :format *agenda-time-format*))
           (format-time (time)
             (if (typep time 'time-range)
                 (format nil
                         "~A--~A"
                         (format-time-from-ts (time-range-begin time))
                         (format-time-from-ts (time-range-end time)))
                 (format-time-from-ts time))))
    (let* ((task1 (task-record-task rec))
           (state (state-name (task-state task1)))
           ;; overdue and date-only entries carry no time value
           (time (unless (or overdue-days (task-record-date-only-p rec))
                   (format-time (task-record-time rec))))
           (title (task-title task1)))
      (pad-agenda-tags
       (cond
         ((deadline rec)
          (format nil
                  "~A~A ~@[~A ~]~A"
                  (if overdue-days
                      (format nil "~2d d. ago: " overdue-days)
                      "Deadline:  ")
                  state
                  time
                  title))
         ((start-task rec)
          (format nil
                  "~A(~A) ~@[~A ~]~A"
                  (if overdue-days
                      (format nil "Sched.~2dx: " overdue-days)
                      "Scheduled: ")
                  state
                  time
                  title))
         (time (format nil "~A (~A) ~A" state time title))
         (t (format nil "~A ~A" state title)))
       (format-agenda-tags (task-tags task1))
       72))))

(defmethod cltpt/tree/outline:outline-text ((rec task-record))
  (record-outline-text rec))

(defmethod cltpt/tree/outline:outline-text ((node agenda-overdue-node))
  (record-outline-text (agenda-overdue-node-record node) (agenda-overdue-node-days node)))

(defun undated-outline-text (task1)
  "outline line for an undated TASK1 (no timestamp records): \"STATE title :tags:\"."
  (pad-agenda-tags
   (format nil "~A ~A"
           (state-name (task-state task1))
           (task-title task1))
   (format-agenda-tags (task-tags task1))))

(defmethod cltpt/tree/outline:outline-text ((node agenda-undated-node))
  (undated-outline-text (agenda-undated-node-task node)))

(defun json-time (ts)
  (when ts
    (local-time:format-timestring nil ts :format *agenda-json-time-format*)))

(defmethod cltpt/tree/outline:outline-json-fields ((node agenda-outline-node))
  (let ((rng (agenda-outline-node-time-range node)))
    (list (cons "begin" (json-time (time-range-begin rng)))
          (cons "end" (json-time (time-range-end rng))))))

(defmethod cltpt/tree/outline:outline-json-fields ((rec task-record))
  (let* ((task1 (task-record-task rec))
         (time (task-record-time rec))
         (begin (task-record-effective-start rec))
         (end (when (typep time 'time-range)
                (time-range-end time)))
         (state (task-state task1))
         (node (task-node task1)))
    (list (cons "title" (task-title task1))
          (cons "state" (when state (state-name state)))
          (cons "done" (if (and state (state-is-terminal state))
                           t
                           :false))
          (cons "type" (cond ((deadline rec) "deadline")
                             ((start-task rec) "scheduled")
                             (t "timestamp")))
          (cons "repeat" (if (task-record-generated-p rec)
                             t
                             :false))
          (cons "begin" (json-time begin))
          (cons "end" (json-time end))
          (cons "all_day" (if (task-record-date-only-p rec)
                              t
                              :false))
          (cons "tags" (map 'vector #'princ-to-string (task-tags task1)))
          (cons "file" (when node (cltpt/roam:node-file node))))))

(defmethod cltpt/tree/outline:outline-json-fields ((node agenda-overdue-node))
  (append (cltpt/tree/outline:outline-json-fields (agenda-overdue-node-record node))
          (list (cons "overdue_days" (agenda-overdue-node-days node)))))

(defmethod cltpt/tree:tree-children ((node agenda-overdue-node))
  nil)

(defmethod cltpt/tree/outline:outline-json-fields ((node agenda-undated-node))
  (let* ((task1 (agenda-undated-node-task node))
         (state (task-state task1))
         (roam-node (task-node task1)))
    (list (cons "title" (task-title task1))
          (cons "state" (when state (state-name state)))
          (cons "done" (if (and state (state-is-terminal state))
                           t
                           :false))
          (cons "type" "undated")
          (cons "repeat" :false)
          (cons "begin" nil)
          (cons "end" nil)
          (cons "all_day" :false)
          (cons "tags" (map 'vector #'princ-to-string (task-tags task1)))
          (cons "file" (when roam-node (cltpt/roam:node-file roam-node))))))

(defmethod cltpt/tree:tree-children ((node agenda-undated-node))
  nil)