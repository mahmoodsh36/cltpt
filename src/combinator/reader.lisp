(defpackage :cltpt/reader
  (:use :cl)
  (:export
   :reader-next-char :reader-char :reader-string= :reader-string-equal
   :is-before-eof :is-after-eof :make-reader :reader-buffer :reader-stream
   :reader-start-position :reader :reader-input-stream :stream-index :reader-cursor
   :is-le-eof :reader-from-string :current-length :reader-from-input))

(in-package :cltpt/reader)

;; TODO: this module needs optimization. it reads one character at a time from the stream
;; which is inefficient. it should be reading in chunks instead.
;; this module implements a "buffered" reader for streams, it allows us to work with streams
;; as sequences and somewhat like we would work with strings.

(defconstant +default-initial-size+ (1024 * 8)
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
   (cursor
    :initform 0
    :accessor reader-cursor
    :documentation "current read position within the buffer.")
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

(defun reader-next-char (reader)
  "reads the next character. returns NIL if if there is none.

if the character is not in the buffer, it extends the buffer and appends new data from the stream."
  ;; the cursor is inside the cache range so we just return the char from there.
  (when (< (reader-cursor reader) (fill-pointer (reader-buffer reader)))
    (let ((char (char (reader-buffer reader) (reader-cursor reader))))
      (incf (reader-cursor reader))
      (return-from reader-next-char char)))
  (let ((buf (reader-buffer reader)))
    ;; if the buffer is physically full, we must resize it to make room at the end.
    (when (= (fill-pointer buf) (array-dimension buf 0))
      (let* ((old-len (array-dimension buf 0))
             (new-len (ceiling (* old-len (reader-growth-factor reader)))))
        (adjust-array buf new-len :element-type 'character)))
    (let* ((stream (reader-stream reader))
           ;; blocking read to get the 1 char we actually need
           (first-char (read-char stream nil :eof)))
      (if (eq first-char :eof)
          (progn
            (setf (reader-eof-reached reader) t)
            nil)
          (progn
            (vector-push first-char buf)
            ;; get whatever else is ready
            (loop while (and (< (fill-pointer buf) (array-dimension buf 0))
                             (listen stream))
                  ;; TODO: fix this, it is inefficient to read one char at a time.
                  do (let ((c (read-char stream nil :eof)))
                       (cond
                         ((eq c :eof)
                          (return))
                         (t
                          (vector-push c buf)))))
            ;; return the character we wanted and increment the cursor
            (incf (reader-cursor reader))
            first-char)))))

(defun reader-char (reader idx)
  "read character at a specific position. returns NIL if it is beyond EOF."
  ;; fill the buffer up to the index we need.
  (loop while (<= (reader-cursor reader) idx)
        ;; TODO: fix this, it is inefficient to read one char at a time.
        do (let ((char (reader-next-char reader)))
             (unless char
               (return-from reader-char nil))))
  (aref (reader-buffer reader) idx))

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

(defmethod current-length ((reader reader))
  (fill-pointer (reader-buffer reader)))