(defpackage :cltpt/reader
  (:use :cl)
  (:export
   :reader-char :reader-string= :reader-string-equal :is-before-eof :is-after-eof :make-reader
   :reader-buffer :reader-stream :reader-start-position :reader :reader-input-stream :stream-index
   :is-le-eof :reader-from-string :reader-buffer-fill :reader-from-input :reader-eof-reached
   :reader-fully-consume :reader-ensure-fill-upto))

(in-package :cltpt/reader)

;; this module implements a "buffered" reader for streams, it allows us to work with streams
;; as sequences and somewhat like we would work with strings.

(defconstant +default-initial-size+ (* 1024 8)
  "the starting size of the buffer.")

(defconstant +default-growth-factor+ 2.0
  "multiplier for buffer expansion. like an array-list.")

(defclass reader (sequence)
  ((stream
    :initarg :stream
    :accessor reader-stream)
   (buffer
    :initform (make-array +default-initial-size+
                          :element-type 'character
                          :adjustable t
                          :fill-pointer 0)
    :accessor reader-buffer)
   (start-position
    :initform nil
    :accessor reader-start-position
    :documentation "file position where reading started (for seekable streams).")
   (eof-reached
    :initform nil
    :accessor reader-eof-reached
    :documentation "t if the underlying stream has been fully consumed.")
   (growth-factor
    :initarg :growth-factor
    :initform +default-growth-factor+
    :accessor reader-growth-factor)))

(defun make-reader (stream &key
                             (initial-size +default-initial-size+)
                             (growth-factor +default-growth-factor+))
  (let ((r (make-instance 'reader
                          :stream stream
                          :growth-factor growth-factor)))
    (when (/= initial-size +default-initial-size+)
      (setf (reader-buffer r)
            (make-array initial-size :element-type 'character
                                     :adjustable t
                                     :fill-pointer 0)))
    ;; capture starting position for seekable streams
    (setf (reader-start-position r)
          (ignore-errors (file-position stream)))
    r))

(defmethod reader-buffer-fill ((reader reader))
  (fill-pointer (reader-buffer reader)))

(defun reader-from-string (str)
  (cltpt/reader:make-reader (make-string-input-stream str)))

(defun reader-from-input (input)
  "INPUT may be a stream, a string or a reader. a reader object is returned that reads INPUT."
  (cond
    ((streamp input)
     (make-reader input))
    ((stringp input)
     (reader-from-string input))
    ((typep input 'reader)
     input)
    (t (error "invalid input type ~A" (type-of input)))))

(defun reader-ensure-fill-upto (reader target-pos)
  "fill the buffer up to TARGET-POS (or EOF). blocks until TARGET-POS is available.
reads all available data, not just up to target, to minimize I/O calls.
returns T if target position is available, NIL if EOF reached before target."
  (let ((buf (reader-buffer reader)))
    ;; already have enough?
    (when (< target-pos (fill-pointer buf))
      (return-from reader-ensure-fill-upto t))
    ;; EOF already reached?
    (when (reader-eof-reached reader)
      (return-from reader-ensure-fill-upto nil))
    ;; need to read more, so just read all available data
    (let ((stream (reader-stream reader)))
      (loop
        ;; ensure we have the capacity
        (let* ((current-fill (fill-pointer buf))
               (capacity (array-dimension buf 0)))
          (when (<= capacity target-pos)
            (let ((new-len (max (1+ target-pos)
                                (ceiling (* capacity (reader-growth-factor reader))))))
              (adjust-array buf new-len :element-type 'character)
              (setf capacity new-len)))
          ;; read as much as we can
          (let ((read-end capacity))
            (setf (fill-pointer buf) capacity)
            (let ((new-fill (read-sequence buf stream :start current-fill :end read-end)))
              (setf (fill-pointer buf) new-fill)
              (cond
                ;; EOF - no more data
                ((= new-fill current-fill)
                 (setf (reader-eof-reached reader) t)
                 (return-from reader-ensure-fill-upto (< target-pos new-fill)))
                ;; got some data - check if we have enough
                ((< target-pos new-fill)
                 (return-from reader-ensure-fill-upto t))
                ;; need more, continue loop
                (t nil)))))))))

(defmethod reader-fully-consume ((reader reader))
  "forces the reader to fully consume the underlying stream until EOF.
blocks until the stream is fully consumed."
  (loop until (reader-eof-reached reader)
        do (reader-ensure-fill-upto reader (fill-pointer (reader-buffer reader)))))

(defun reader-char (reader idx)
  "read character at a specific position. returns NIL if it is beyond EOF."
  (when (reader-ensure-fill-upto reader idx)
    (aref (reader-buffer reader) idx)))

(defun is-before-eof (reader idx)
  "return whether IDX is strictly before EOF."
  (let ((char (reader-char reader idx)))
    (not (null char))))

(defun is-le-eof (reader idx)
  "whether index is less or equal to the position of eof."
  (is-before-eof reader (1- idx)))

;; TODO: this is inconsistent with 'is-before-eof', rename it
(defun is-after-eof (reader idx)
  "return whether IDX is (not strictly) after EOF."
  (not (is-before-eof reader idx)))

(defun reader-string= (reader string &key (start1 0) end1 (start2 0) end2)
  "behaves like string= does for strings. (inplace-comparison)"
  (let ((end1 (or end1 most-positive-fixnum))
        (end2 (or end2 (length string))))
    (loop for i1 from start1 below end1
          for i2 from start2 below end2
          for char1 = (reader-char reader i1)
          for char2 = (char string i2)
          do (cond
               ((null char1)
                (return-from reader-string= nil))
               ((char/= char1 char2)
                (return-from reader-string= nil)))
          finally (return-from reader-string= t))))

(defun reader-string-equal (reader string &key (start1 0) end1 (start2 0) end2)
  "behaves like string-equal does for strings. (inplace-comparison, case-insensitive)"
  (let ((end1 (or end1 most-positive-fixnum))
        (end2 (or end2 (length string))))
    (loop for i1 from start1 below end1
          for i2 from start2 below end2
          for char1 = (reader-char reader i1)
          for char2 = (char string i2)
          do (cond
               ((null char1)
                (return-from reader-string-equal nil))
               ((char-not-equal char1 char2)
                (return-from reader-string-equal nil)))
          finally (return-from reader-string-equal t))))

;; TODO: why dont we just make the 'reader' class implement the gray stream interface?
;; this is used for lisp-sexp
(defclass reader-input-stream (sb-gray:fundamental-character-input-stream)
  ((reader :initarg :reader :reader stream-reader)
   (index :initarg :index :accessor stream-index)))

(defmethod sb-gray:stream-read-char ((stream reader-input-stream))
  (let ((reader (stream-reader stream))
        (idx (stream-index stream)))
    (let ((char (reader-char reader idx)))
      (if char
          (progn
            (incf (stream-index stream))
            char)
          :eof))))

(defmethod sb-gray:stream-unread-char ((stream reader-input-stream) char)
  (decf (stream-index stream))
  nil)

(defmethod sb-sequence:length ((reader reader))
  "returns the length of the reader's content.
errors if the stream has not been fully consumed yet."
  (unless (reader-eof-reached reader)
    (error "cannot get length of reader: stream not fully consumed. use reader-char or iterate to EOF first."))
  (fill-pointer (reader-buffer reader)))

(defmethod sb-sequence:elt ((reader reader) index)
  (check-type index (integer 0 *))
  (let ((char (reader-char reader index)))
    (unless char
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(1- (sb-sequence:length reader)))))
    char))

(defmethod (setf sb-sequence:elt) (new-value (reader reader) index)
  (error "reader is immutable/read-only sequence."))

(defmethod sb-sequence:make-sequence-iterator ((reader reader) &key from-end start end)
  (when from-end
    (error "traversing reader from end is not efficiently supported."))
  (let ((start (or start 0))
        (limit (or end nil)))
    (values start
            limit
            from-end
            ;; TODO: name these in comments perhaps
            (lambda (seq it from-end)
              (1+ it))
            (lambda (seq it limit from-end)
              (if limit
                  (>= it limit)
                  (is-after-eof seq it)))
            (lambda (seq it)
              (reader-char seq it))
            (lambda (new-value seq it)
              (error "reader is immutable."))
            (lambda (seq it)
              it)
            (lambda (seq it)
              it))))

;; there's not really a point in making this return a reader so we just return a string
(defmethod sb-sequence:make-sequence-like ((reader reader) length
                                           &key initial-element initial-contents)
  "create a string of given length, since reader is a character sequence."
  (declare (ignore reader))
  (let ((result (make-string length)))
    (cond
      (initial-contents
       (loop for i from 0 below length
             for c across initial-contents
             do (setf (char result i) c)))
      (initial-element
       (fill result initial-element)))
    result))