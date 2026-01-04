(defpackage :cltpt/buffer
  (:use :cl :cltpt/buffer/region :cltpt/buffer/change)
  (:export
   :buffer
   :make-buffer
   :buffer-parent
   :buffer-children
   :buffer-region
   :buffer-text
   :buffer-own-text
   :buffer-region-begin
   :buffer-region-end
   :buffer-region-length
   :buffer-begin-absolute
   :buffer-end-absolute
   :buffer-insert
   :buffer-delete
   :buffer-replace
   :buffer-apply-changes
   :buffer-propagate
   :buffer-fetch-parent-text
   :buffer-scheduled-levels
   :schedule-change :schedule-change*
   :schedule-batch
   :apply-scheduled-changes

   ;; region functions (re-exported from cltpt/buffer/region)
   :region
   :make-region
   :region-begin
   :region-end
   :region-text
   :region-length
   :region-decf
   :region-incf
   :region-contains
   :region-encloses
   :region-encloses-strict
   :region-clone
   :region-compress
   :region-compress-by
   :region-replace
   :region-intersection

   ;; re-export change function
   :make-change :change-region :change-operator :change-args :change-begin :change-end))

(in-package :cltpt/buffer)

;; this package implements a tree-based text "buffer" capable of distinct text regions and incremental change management.
;; - buffers can be nested. changes in children propagate up to the root when requested, keeping the tree in sync.
;; - "scheduled changes" are pending changes that are shifted based on the edits made by earlier changes or immediate operations.
;; - changes are applied sequentially, with automatic adjusting of coordinates occurring after every edit to keep the integrity of the data. also, when a buffer grows or shrinks, its siblings' regions within the parent are automatically adjusted.
;; - this isnt yet used for "incremental changes" in the sense of incremental parsing on in-editor buffer edits. it mainly serves for "smart" modification of the text during conversion.
;; - the code can be made faster by not concatenating all the time and using a "string builder".

(defclass buffer ()
  ((parent
    :initarg :parent
    :accessor buffer-parent
    :initform nil)
   (children
    :initarg :children
    :accessor buffer-children
    :initform nil)
   (region
    :initarg :region
    :accessor buffer-region
    :initform nil
    :documentation "the region struct relative to the parent.")
   (text
    :initarg :text
    :accessor buffer-own-text
    :initform nil
    :documentation "the virtual text of this buffer. If nil, refers to parent.")
   (scheduled-levels
    :accessor buffer-scheduled-levels
    :initform nil
    :documentation "a list of levels (list of batches)."))
  (:documentation "a buffer represents a region of text with incremental change management."))

(defun make-buffer (&key parent region text children)
  (let ((buf (make-instance 'buffer
                            :parent parent
                            :region region
                            :text text
                            :children children)))
    (when parent
      (push buf (buffer-children parent)))
    buf))

(defmethod buffer-region-begin ((buffer buffer))
  (region-begin (buffer-region buffer)))

(defmethod (setf buffer-region-begin) (val (buffer buffer))
  (setf (region-begin (buffer-region buffer)) val))

(defmethod buffer-region-end ((buffer buffer))
  (region-end (buffer-region buffer)))

(defmethod (setf buffer-region-end) (val (buffer buffer))
  (setf (region-end (buffer-region buffer)) val))

(defmethod buffer-region-length ((buffer buffer))
  (- (buffer-region-end buffer) (buffer-region-begin buffer)))

(defmethod buffer-begin-absolute ((buffer buffer))
  (let ((pos (buffer-region-begin buffer))
        (b buffer))
    (loop for parent = (buffer-parent b)
          while parent
          do (incf pos (buffer-region-begin parent))
             (setf b parent))
    pos))

(defmethod buffer-end-absolute ((buffer buffer))
  (let ((pos (buffer-region-end buffer))
        (b buffer))
    (loop for parent = (buffer-parent b)
          while parent
          do (incf pos (buffer-region-begin parent))
             (setf b parent))
    pos))

(defmethod buffer-text ((buffer buffer))
  (or (buffer-own-text buffer)
      (when (buffer-parent buffer)
        (let ((parent-text (buffer-text (buffer-parent buffer))))
          (subseq parent-text (buffer-region-begin buffer) (buffer-region-end buffer))))))

(defun resolve-delegation (buffer start end)
  "recursively find the deepest child of BUFFER that fully contains START..END."
  (dolist (child (buffer-children buffer))
    (let ((c-begin (buffer-region-begin child))
          (c-end (buffer-region-end child)))
      (when (and (>= start c-begin) (<= end c-end))
        (return-from resolve-delegation
          (resolve-delegation child (- start c-begin) (- end c-begin))))))
  (values buffer start end))

(defun transform-coord-start (p m-start m-end new-len)
  (let ((delta (- new-len (- m-end m-start))))
    (cond
      ((< p m-start) p)
      ((>= p m-end) (+ p delta))
      (t m-start))))

(defun transform-coord-end (p m-start m-end new-len)
  (let ((delta (- new-len (- m-end m-start))))
    (cond
      ((<= p m-start) p)
      ((>= p m-end) (+ p delta))
      (t (+ m-start new-len)))))

(defun rebase-list-tracking (changes m-start m-end new-len)
  "rebase changes using tracking and adapting to changes."
  (let ((new-list))
    (dolist (c changes)
      (let ((old-start (change-begin c))
            (old-end (change-end c)))
        (setf (region-begin (change-region c))
              (transform-coord-start old-start m-start m-end new-len))
        (setf (region-end (change-region c))
              (transform-coord-end old-end m-start m-end new-len))
        (push c new-list)))
    (nreverse new-list)))

(defun rebase-list-dropping (changes m-start m-end new-len)
  "rebase changes and drop conflicts (we dont adapt like in `rebase-list-tracking')."
  (let ((delta (- new-len (- m-end m-start)))
        (new-list))
    (dolist (c changes)
      (let ((c-start (change-begin c))
            (c-end (change-end c)))
        (cond
          ((>= c-start m-end)
           (setf (region-begin (change-region c)) (+ c-start delta))
           (setf (region-end (change-region c)) (+ c-end delta))
           (push c new-list))
          ((<= c-end m-start)
           (push c new-list))
          ;; drop
          (t nil))))
    (nreverse new-list)))

(defun rebase-all-levels (buffer m-start m-end new-len)
  (when (buffer-scheduled-levels buffer)
    (setf (buffer-scheduled-levels buffer)
          (mapcar (lambda (level)
                    (mapcar (lambda (batch)
                              (rebase-list-tracking batch m-start m-end new-len))
                            level))
                  (buffer-scheduled-levels buffer)))))

(defun buffer-shift-children (buffer start-pos delta)
  (dolist (child (buffer-children buffer))
    (when (>= (buffer-region-begin child) start-pos)
      (incf (buffer-region-begin child) delta)
      (incf (buffer-region-end child) delta))))

(defmethod buffer-replace ((buffer buffer) start end text &key (delegate t) propagate)
  (if delegate
      (multiple-value-bind (target t-start t-end) (resolve-delegation buffer start end)
        (if (eq target buffer)
            (buffer-replace buffer start end text :delegate nil :propagate propagate)
            (buffer-replace target t-start t-end text :delegate t :propagate propagate)))
      (progn
        (unless (buffer-own-text buffer)
          (setf (buffer-own-text buffer) (buffer-text buffer)))
        (let* ((current-text (buffer-own-text buffer))
               (insert-len (length text))
               (net-change (- insert-len (- end start)))
               (new-text (concatenate 'string
                                      (subseq current-text 0 start)
                                      text
                                      (subseq current-text end))))
          (setf (buffer-own-text buffer) new-text)
          (setf (buffer-children buffer)
                (loop for child in (buffer-children buffer)
                      for c-start = (buffer-region-begin child)
                      for c-end = (buffer-region-end child)
                      unless (and (>= c-start start) (<= c-end end))
                        collect child))
          (dolist (child (buffer-children buffer))
            (let ((c-start (buffer-region-begin child)) (c-end (buffer-region-end child)))
              (cond ((>= c-start end) (incf (buffer-region-begin child) net-change))
                    ((> c-start start) (setf (buffer-region-begin child) (+ start insert-len))))
              (cond ((>= c-end end) (incf (buffer-region-end child) net-change))
                    ((> c-end start) (setf (buffer-region-end child) (+ start insert-len))))))
          (rebase-all-levels buffer start end insert-len)
          (when propagate
            (buffer-propagate buffer))))))

(defmethod buffer-insert ((buffer buffer) pos text &key (delegate t) propagate)
  (buffer-replace buffer pos pos text :delegate delegate :propagate propagate))

(defmethod buffer-delete ((buffer buffer) start end &key (delegate t) propagate)
  (buffer-replace buffer start end "" :delegate delegate :propagate propagate))

(defmethod buffer-propagate ((buffer buffer) &key stop-at-text)
  "propagate local changes to parent."
  (when (buffer-parent buffer)
    (let* ((parent (buffer-parent buffer))
           (own-text (buffer-text buffer))
           (old-len (buffer-region-length buffer))
           (new-len (length own-text)))
      (if (buffer-own-text parent)
          (progn
            (let ((start (buffer-region-begin buffer))
                  (end (buffer-region-end buffer))
                  (parent-text (buffer-own-text parent)))
              (setf (buffer-own-text parent)
                    (concatenate 'string
                                 (subseq parent-text 0 start)
                                 own-text
                                 (subseq parent-text end)))
              (setf (buffer-region-end buffer) (+ start new-len))
              (buffer-shift-children parent end (- new-len old-len))
              (rebase-all-levels parent start end new-len))
            (unless stop-at-text
              (buffer-propagate parent :stop-at-text stop-at-text)))
          (let ((parent-text (buffer-text parent)))
            (setf (buffer-own-text parent) parent-text)
            (buffer-propagate buffer :stop-at-text stop-at-text)
            (setf (buffer-own-text parent) nil))))))

(defmethod buffer-fetch-parent-text ((buffer buffer))
  (when (buffer-parent buffer)
    (setf (buffer-own-text buffer)
          (subseq (buffer-text (buffer-parent buffer))
                  (buffer-region-begin buffer)
                  (buffer-region-end buffer)))))

(defun discard-scheduled-in-region (buffer start end)
  "removes any pending scheduled changes in BUFFER that are fully contained within [START, END)."
  (let ((new-levels))
    (dolist (level (buffer-scheduled-levels buffer))
      (let ((new-level))
        (dolist (batch level)
          (let ((kept-changes
                  (remove-if (lambda (c)
                               (and (>= (change-begin c) start)
                                    (<= (change-end c) end)))
                             batch)))
            (when kept-changes
              (push kept-changes new-level))))
        (when new-level
          (push (nreverse new-level) new-levels))))
    (setf (buffer-scheduled-levels buffer) (nreverse new-levels))))

(defmethod schedule-batch ((buffer buffer) changes &key new-level (delegate t) discard-contained)
  (when changes
    (if delegate
        (let ((current-target)
              (current-batch)
              (first-sub-batch t))
          (dolist (change changes)
            (let ((start (change-begin change))
                  (end (change-end change)))
              (multiple-value-bind (target t-start t-end) (resolve-delegation buffer start end)
                (when (and current-target (not (eq target current-target)))
                  (schedule-batch current-target
                                  (nreverse current-batch)
                                  :new-level (when first-sub-batch new-level)
                                  :delegate nil
                                  :discard-contained discard-contained)
                  (setf current-batch nil)
                  (setf first-sub-batch nil))
                (setf current-target target)
                (push (make-change :region (make-region :begin t-start :end t-end)
                                   :operator (change-operator change)
                                   :args (change-args change))
                      current-batch))))
          (when (and current-target current-batch)
            (schedule-batch current-target
                            (nreverse current-batch)
                            :new-level (when first-sub-batch new-level)
                            :delegate nil
                            :discard-contained discard-contained)))
        (progn
          (when discard-contained
            (dolist (c changes)
              (discard-scheduled-in-region buffer (change-begin c) (change-end c))))
          (let ((levels (buffer-scheduled-levels buffer)))
            (if (or new-level (null levels))
                (setf (buffer-scheduled-levels buffer) (append levels (list (list changes))))
                (nconc (car (last levels)) (list changes))))))))

(defmethod schedule-change ((buffer buffer) start end operator &key new-level (delegate t) discard-contained)
  (schedule-batch buffer
                  (list (make-change :region (make-region :begin start :end end)
                                     :operator operator))
                  :new-level new-level
                  :delegate delegate
                  :discard-contained discard-contained))

(defmethod schedule-change* ((buffer buffer) change)
  (let ((new-level (getf (change-args change) :new-level))
        (delegate (getf (change-args change) :delegate t))
        (discard-contained (getf (change-args change) :discard-contained)))
    (schedule-batch buffer
                    (list change)
                    :new-level new-level
                    :delegate delegate
                    :discard-contained discard-contained)))

(defmethod apply-scheduled-changes ((buffer buffer) &key propagate-to on-apply)
  (unless (buffer-own-text buffer)
    (setf (buffer-own-text buffer) (buffer-text buffer)))
  (loop while (buffer-scheduled-levels buffer)
        do (let ((current-level (pop (buffer-scheduled-levels buffer))))
             (loop while current-level
                   do (let ((current-batch (pop current-level)))
                        (loop while current-batch
                              do (let* ((change (pop current-batch))
                                        (start (change-begin change))
                                        (end (change-end change))
                                        (op (change-operator change))
                                        (text (if (functionp op)
                                                  (funcall op
                                                           (subseq (buffer-own-text buffer)
                                                                   start
                                                                   end))
                                                  op)))
                                   (buffer-replace buffer
                                                   start
                                                   end
                                                   text
                                                   :delegate nil
                                                   :propagate nil)
                                   (when on-apply
                                     (funcall on-apply buffer change text))
                                   (setf current-batch
                                         (rebase-list-dropping current-batch
                                                               start
                                                               end
                                                               (length text)))
                                   (setf current-level
                                         (mapcar (lambda (b)
                                                   (rebase-list-dropping b
                                                                         start
                                                                         end
                                                                         (length text)))
                                                 current-level))))))))
  (when propagate-to
    (buffer-propagate buffer :stop-at-text (eq propagate-to :text))))