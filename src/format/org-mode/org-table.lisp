(in-package :cltpt/org-mode)

(defparameter *table-v-delimiter* #\|
  "the character for vertical cell delimiters.")
(defparameter *table-h-delimiter* #\-
  "the character for horizontal rule lines.")
(defparameter *table-intersection-delimiter* #\+
  "the character for intersections in horizontal rules.")

(defun get-line-bounds (str pos)
  "returns (values line-start, line-end, next-line-start) for the line at pos."
  (when (>= pos (length str))
    (return-from get-line-bounds
      (values (length str) (length str) (length str))))
  (let* ((line-start (or (position #\newline str :end pos :from-end t)
                         -1))
         (actual-line-start (1+ line-start))
         (line-end (or (position #\newline str :start actual-line-start)
                       (length str)))
         (next-line-start (if (< line-end (length str))
                              (1+ line-end)
                              line-end)))
    (values actual-line-start line-end next-line-start)))

(defun is-table-line-at-pos-p (str pos)
  "checks if the line at pos starts with a delimiter after trimming whitespace."
  (multiple-value-bind (line-start line-end) (get-line-bounds str pos)
    (let ((trimmed-start
            (position-if-not
             (lambda (c)
               (member c '(#\space #\tab)))
             str
             :start line-start
             :end line-end)))
      (and trimmed-start
           (member (char str trimmed-start)
                   (list *table-v-delimiter* *table-intersection-delimiter*))))))

(defun is-hrule-line-at-pos-p (str pos)
  "checks if the line at pos is a horizontal rule."
  (multiple-value-bind (line-start line-end) (get-line-bounds str pos)
    (let ((trimmed-start
            (position-if-not (lambda (c) (member c '(#\space #\tab)))
                             str :start line-start :end line-end)))
      (and trimmed-start
           (member (char str trimmed-start)
                   (list *table-v-delimiter* *table-intersection-delimiter*))
           (loop for i from (1+ trimmed-start) below line-end
                 for char = (char str i)
                 always (member char (list #\space #\tab
                                           *table-h-delimiter*
                                           *table-intersection-delimiter*
                                           *table-v-delimiter*)))))))

(defun find-content-bounds (str start end)
  "finds the start and end of non-whitespace content within the slice [start, end)."
  (let ((content-start
          (position-if-not
           (lambda (c)
             (member c '(#\space #\tab)))
           str
           :start start
           :end end))
        (content-end
          (position-if-not
           (lambda (c)
             (member c '(#\space #\tab)))
           str
           :start start
           :end end
           :from-end t)))
    (if content-start
        (values content-start (1+ content-end))
        (values start start))))

(defun parse-table-row (ctx str row-start-offset inline-rules)
  "parses a single table row, creating a structured tree for cells and their content.

returns (values row-node, next-line-start-offset)."
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds str row-start-offset)
    (let* ((trimmed-line-start
             (position-if-not
              (lambda (c) (member c '(#\space #\tab)))
              str
              :start line-start
              :end line-end))
           (row-children)
           (current-pos trimmed-line-start))
      (loop
        (let ((delimiter-pos (position *table-v-delimiter* str
                                       :start current-pos
                                       :end line-end)))
          (unless delimiter-pos (return))
          (when (> delimiter-pos current-pos)
            (let* ((cell-begin current-pos)
                   (cell-end delimiter-pos))
              (multiple-value-bind (content-begin content-end)
                  (find-content-bounds str cell-begin cell-end)
                (let* ((content-node
                         ;; only create a content node if there is actual, non-whitespace text.
                         (when (< content-begin content-end)
                           (let* ((inline-children
                                    (when inline-rules
                                      (cltpt/combinator:scan-all-rules
                                       ctx
                                       str
                                       inline-rules
                                       content-begin
                                       content-end)))
                                  (cell-content-match (cltpt/combinator:make-match
                                                       :id 'table-cell-content
                                                       :begin content-begin
                                                       :end content-end
                                                       :str str
                                                       :children inline-children)))
                             cell-content-match)))
                       (cell-children (if content-node
                                          (list content-node)
                                          nil))
                       (cell-match (cltpt/combinator:make-match
                                    :id 'table-cell
                                    :begin cell-begin
                                    :end cell-end
                                    :str str
                                    :children cell-children)))
                  (push cell-match row-children)))))
          (let ((delimiter-node (cltpt/combinator:make-match
                                 :id 'table-cell-delimiter
                                 :begin delimiter-pos
                                 :end (1+ delimiter-pos)
                                 :str str)))
            (push delimiter-node row-children))
          (setf current-pos (1+ delimiter-pos))))
      (let ((row-match (cltpt/combinator:make-match
                        :id 'table-row
                        :begin trimmed-line-start
                        :end line-end
                        :str str
                        :children (nreverse row-children))))
        (values row-match next-line-start)))))

(defun org-table-matcher (ctx str pos &optional inline-rules)
  "parses an org-mode table starting at pos. returns (values match-node, new-pos)."
  (multiple-value-bind (first-line-start) (get-line-bounds str pos)
    (unless (and (= pos first-line-start)
                 (is-table-line-at-pos-p str pos))
      (return-from org-table-matcher (values nil pos))))
  (let ((row-nodes)
        (current-pos pos)
        (last-successful-pos pos))
    (loop
      (when (>= current-pos (length str))
        (return))
      (multiple-value-bind
            (line-start line-end next-start)
          (get-line-bounds str current-pos)
        (unless (and (= current-pos line-start)
                     (is-table-line-at-pos-p str line-start))
          (return))
        (if (is-hrule-line-at-pos-p str line-start)
            (let* ((trimmed-start
                     (position-if-not
                      (lambda (c) (member c '(#\space #\tab)))
                      str
                      :start line-start
                      :end line-end))
                   (hrule-match
                     (cltpt/combinator:make-match :id 'table-hrule
                                                  :begin trimmed-start
                                                  :end line-end
                                                  :str str)))
              (push hrule-match row-nodes))
            (multiple-value-bind (row-node)
                (parse-table-row ctx str line-start inline-rules)
              (push row-node row-nodes)))
        (setf current-pos next-start)
        (setf last-successful-pos current-pos)))
    (if row-nodes
        (let ((table-match (cltpt/combinator:make-match
                            :id 'org-table
                            :begin pos
                            :end last-successful-pos
                            :str str
                            :children (nreverse row-nodes))))
          (values table-match last-successful-pos))
        (values nil pos))))

(defun reformat-table (parse-tree)
  "takes a table parse tree and returns a new string with all columns
neatly aligned based on the widest cell in each column."
  (let ((col-widths (make-array 0 :adjustable t :fill-pointer t))
        (table-data))
    ;; pass 1: check column widths
    (dolist (row-node (cltpt/combinator/match:match-children parse-tree))
      (case (cltpt/combinator/match:match-id row-node)
        ('table-row
         (let ((current-row-cells)
               (cell-nodes (remove-if-not
                            (lambda (node)
                              (eq (cltpt/combinator/match::match-id node) 'table-cell))
                            (cltpt/combinator/match::match-children row-node))))
           (loop for cell-node in cell-nodes
                 for col-idx from 0
                 do
                    ;; look for the 'table-cell-content' child to get the text for width calculation.
                    (let* ((content-node (car (cltpt/combinator/match:match-children cell-node)))
                           (cell-text (if content-node
                                          (cltpt/combinator:match-text content-node)
                                          ""))
                           (cell-width (length cell-text)))
                      (when (>= col-idx (length col-widths))
                        (vector-push-extend 0 col-widths))
                      (setf (aref col-widths col-idx)
                            (max (aref col-widths col-idx) cell-width))
                      (push cell-text current-row-cells)))
           (push (nreverse current-row-cells) table-data)))
        ('table-hrule
         (push '(:separator) table-data))))
    (setf table-data (nreverse table-data))
    ;; pass 2: build the formatted string
    (with-output-to-string (s)
      (loop for row-data in table-data
            do
               (if (eq (car row-data) :separator)
                   (progn
                     (write-char *table-v-delimiter* s)
                     (loop for width across col-widths
                           for is-first-col = t then nil
                           do
                              (unless is-first-col
                                (write-char *table-intersection-delimiter* s))
                              (dotimes (i (+ width 2))
                                (write-char *table-h-delimiter* s)))
                     (write-char *table-v-delimiter* s)
                     (write-char #\newline s))
                   (progn
                     (write-char *table-v-delimiter* s)
                     (loop for cell-text in row-data
                           for col-idx from 0
                           do (let ((width (aref col-widths col-idx)))
                                (format s
                                        " ~vA ~c"
                                        width
                                        cell-text
                                        *table-v-delimiter*)))
                     (write-char #\newline s)))))))