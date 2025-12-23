(in-package :cltpt/org-mode)

(defparameter *table-v-delimiter* #\|
  "the character for vertical cell delimiters.")
(defparameter *table-h-delimiter* #\-
  "the character for horizontal rule lines.")
(defparameter *table-intersection-delimiter* #\+
  "the character for intersections in horizontal rules.")

;; TODO: get-line-bounds is defined in org-list.lisp, move it to a better location that is
;; available to both files.

(defun is-table-line-at-pos-p (reader pos)
  "checks if the line at pos starts with a delimiter after trimming whitespace."
  (multiple-value-bind (line-start line-end) (get-line-bounds reader pos)
    (let ((trimmed-start
            (position-if-not
             (lambda (c)
               (member c '(#\space #\tab)))
             reader
             :start line-start
             :end line-end)))
      (and trimmed-start
           (member (elt reader trimmed-start)
                   (list *table-v-delimiter* *table-intersection-delimiter*))))))

(defun is-hrule-line-at-pos-p (reader pos)
  "checks if the line at pos is a horizontal rule."
  (multiple-value-bind (line-start line-end) (get-line-bounds reader pos)
    (let ((trimmed-start
            (position-if-not (lambda (c) (member c '(#\space #\tab)))
                             reader :start line-start :end line-end)))
      (and trimmed-start
           (member (elt reader trimmed-start)
                   (list *table-v-delimiter* *table-intersection-delimiter*))
           ;; the line must contain at least one h-delimiter
           (find *table-h-delimiter* reader :start line-start :end line-end)
           (loop for i from (1+ trimmed-start) below line-end
                 for char = (elt reader i)
                 always (member char (list #\space #\tab
                                           *table-h-delimiter*
                                           *table-intersection-delimiter*
                                           *table-v-delimiter*)))))))

(defun find-content-bounds (reader start end)
  "finds the start and end of non-whitespace content within the slice [start, end)."
  (let ((content-start
          (position-if-not
           (lambda (c)
             (member c '(#\space #\tab)))
           reader
           :start start
           :end end))
        ;; use loop-based backwards search (cant use :from-end with reader)
        (content-end
          (loop for i from (1- end) downto start
                when (not (member (elt reader i) '(#\space #\tab)))
                  return i)))
    (if content-start
        (values content-start (1+ content-end))
        (values start start))))

(defun parse-table-row (ctx reader row-start-offset inline-rules)
  "parses a single table row, creating a structured tree for cells and their content."
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds reader row-start-offset)
    (let* ((trimmed-line-start
             (position-if-not
              (lambda (c) (member c '(#\space #\tab)))
              reader
              :start line-start
              :end line-end))
           (row-match (cltpt/combinator:make-match
                       :id 'table-row
                       :ctx ctx
                       :parent (cltpt/combinator:context-parent-match ctx)
                       :begin (- trimmed-line-start (cltpt/combinator:context-parent-begin ctx))))
           (row-ctx (cltpt/combinator:context-copy ctx row-match))
           (row-children)
           (current-pos trimmed-line-start))
      (loop
        (let ((delimiter-pos (position *table-v-delimiter*
                                       reader
                                       :start current-pos
                                       :end line-end)))
          (unless delimiter-pos (return)) ;; exit when no more '|' are found
          (when (> delimiter-pos current-pos)
            (let* ((cell-begin current-pos)
                   (cell-end delimiter-pos))
              (multiple-value-bind (content-begin content-end)
                  (find-content-bounds reader cell-begin cell-end)
                (let* ((cell-match (cltpt/combinator:make-match
                                    :id 'table-cell
                                    :ctx row-ctx
                                    :parent (cltpt/combinator:context-parent-match row-ctx)
                                    :begin (- cell-begin (cltpt/combinator:context-parent-begin row-ctx))
                                    :end (- cell-end (cltpt/combinator:context-parent-begin row-ctx))))
                       (cell-ctx (cltpt/combinator:context-copy row-ctx cell-match))
                       (content-node
                         (when (< content-begin content-end)
                           (let* ((cell-content-match (cltpt/combinator:make-match
                                                       :id 'table-cell-content
                                                       :ctx cell-ctx
                                                       :parent (cltpt/combinator:context-parent-match cell-ctx)
                                                       :begin (- content-begin cell-begin)
                                                       :end (- content-end cell-begin)))
                                  (cell-content-ctx (cltpt/combinator:context-copy cell-ctx cell-content-match))
                                  (inline-children
                                    (when inline-rules
                                      (cltpt/combinator:scan-all-rules
                                       cell-content-ctx
                                       reader
                                       inline-rules
                                       content-begin
                                       content-end))))
                             (setf (cltpt/combinator:match-children cell-content-match)
                                   inline-children)
                             (cltpt/combinator:match-set-children-parent cell-content-match)
                             cell-content-match)))
                       (cell-children (when content-node
                                        (list content-node))))
                  (setf (cltpt/combinator:match-children cell-match) cell-children)
                  (when content-node
                    (setf (cltpt/combinator/match:match-parent content-node) cell-match))
                  (push cell-match row-children)))))
          (let ((delimiter-node (cltpt/combinator:make-match
                                 :id 'table-cell-delimiter
                                 :ctx row-ctx
                                 :parent (cltpt/combinator:context-parent-match row-ctx)
                                 :begin (- delimiter-pos (cltpt/combinator:context-parent-begin row-ctx))
                                 :end (- (1+ delimiter-pos) (cltpt/combinator:context-parent-begin row-ctx)))))
            (push delimiter-node row-children))
          (setf current-pos (1+ delimiter-pos))))
      ;; after the loop, check for a final cell after the last delimiter
      ;; TODO: DRY this
      (multiple-value-bind (content-begin content-end)
          (find-content-bounds reader current-pos line-end)
        (when (< content-begin content-end)
          ;; there is content here, so create one last cell.
          (let* ((cell-begin current-pos)
                 (cell-end line-end)
                 (cell-match (cltpt/combinator:make-match
                              :id 'table-cell
                              :ctx row-ctx
                              :parent (cltpt/combinator:context-parent-match row-ctx)
                              :begin (- cell-begin (cltpt/combinator:context-parent-begin row-ctx))
                              :end (- cell-end (cltpt/combinator:context-parent-begin row-ctx))))
                 (cell-ctx (cltpt/combinator:context-copy row-ctx cell-match))
                 (content-node
                   (when (< content-begin content-end)
                     (let* ((cell-content-match (cltpt/combinator:make-match
                                                 :id 'table-cell-content
                                                 :ctx cell-ctx
                                                 :parent (cltpt/combinator:context-parent-match cell-ctx)
                                                 :begin (- content-begin cell-begin)
                                                 :end (- content-end cell-begin)))
                            (cell-content-ctx (cltpt/combinator:context-copy cell-ctx cell-content-match))
                            (inline-children
                              (when inline-rules
                                (cltpt/combinator:scan-all-rules
                                 cell-content-ctx
                                 reader
                                 inline-rules
                                 content-begin
                                 content-end))))
                       (setf (cltpt/combinator:match-children cell-content-match)
                             inline-children)
                       (cltpt/combinator:match-set-children-parent cell-content-match)
                       cell-content-match)))
                 (cell-children (when content-node
                                  (list content-node))))
            (setf (cltpt/combinator:match-children cell-match) cell-children)
            (when content-node
              (setf (cltpt/combinator/match:match-parent content-node) cell-match))
            (push cell-match row-children))))
      ;; finalize row match
      (let ((reversed-children (nreverse row-children)))
        (setf (cltpt/combinator:match-end row-match)
              (- line-end (cltpt/combinator:context-parent-begin ctx)))
        (setf (cltpt/combinator:match-children row-match) reversed-children)
        (dolist (child reversed-children)
          (setf (cltpt/combinator/match:match-parent child) row-match))
        (values row-match next-line-start)))))

(defun org-table-matcher (ctx reader pos &optional inline-rules)
  "parses an org-mode table starting at pos. returns (values match-node, new-pos)."
  (multiple-value-bind (first-line-start) (get-line-bounds reader pos)
    (unless (and (= pos first-line-start)
                 (is-table-line-at-pos-p reader pos))
      (return-from org-table-matcher (values nil pos))))
  (let* ((table-match (cltpt/combinator:make-match
                       :id 'org-table
                       :ctx ctx
                       :parent (cltpt/combinator:context-parent-match ctx)
                       :begin (- pos (cltpt/combinator:context-parent-begin ctx))))
         (table-ctx (cltpt/combinator:context-copy ctx table-match))
         (row-nodes) ;; built in reverse order
         (current-pos pos)
         (last-successful-pos pos))
    (loop
      (when (cltpt/reader:is-after-eof reader current-pos)
        (return))
      (multiple-value-bind (line-start line-end next-start)
          (get-line-bounds reader current-pos)
        (unless (and (= current-pos line-start)
                     (is-table-line-at-pos-p reader line-start))
          (return))
        ;; push the row/hrule
        (if (is-hrule-line-at-pos-p reader line-start)
            (let* ((trimmed-start (position-if-not
                                   (lambda (c) (member c '(#\space #\tab)))
                                   reader
                                   :start line-start
                                   :end line-end))
                   (hrule-match (cltpt/combinator:make-match
                                 :id 'table-hrule
                                 :ctx table-ctx
                                 :parent (cltpt/combinator:context-parent-match table-ctx)
                                 :begin (- trimmed-start
                                           (cltpt/combinator:context-parent-begin table-ctx))
                                 :end (- line-end
                                         (cltpt/combinator:context-parent-begin table-ctx)))))
              (push hrule-match row-nodes))
            (multiple-value-bind (row-node)
                (parse-table-row table-ctx reader line-start inline-rules)
              (push row-node row-nodes)))
        ;; push the newline separator if it exists
        (when (< line-end next-start)
          (let ((separator-match (cltpt/combinator:make-match
                                  :id 'table-row-separator
                                  :ctx table-ctx
                                  :parent (cltpt/combinator:context-parent-match table-ctx)
                                  :begin (- line-end
                                            (cltpt/combinator:context-parent-begin table-ctx))
                                  :end (- next-start
                                          (cltpt/combinator:context-parent-begin table-ctx)))))
            (push separator-match row-nodes)))
        (setf current-pos next-start)
        (setf last-successful-pos current-pos)))
    (if row-nodes
        (let* ((final-nodes-rev (if (and (car row-nodes)
                                         (eq (cltpt/combinator:match-id (car row-nodes))
                                             'table-row-separator))
                                    ;; if the last thing found was a separator, discard it.
                                    (cdr row-nodes)
                                    row-nodes))
               (last-child (car final-nodes-rev))
               (reversed-nodes (nreverse final-nodes-rev)))
          (if (not last-child)
              (values nil pos) ;; table was empty or only had a separator, invalid.
              (progn
                ;; finalize table match
                (setf (cltpt/combinator:match-end table-match)
                      (+ (cltpt/combinator:match-begin table-match)
                         (cltpt/combinator:match-end last-child)))
                (setf (cltpt/combinator:match-children table-match) reversed-nodes)
                (cltpt/combinator:match-set-children-parent table-match)
                ;; the match is tight, but the next parse position is after the trailing newline.
                (values table-match last-successful-pos))))
        (values nil pos))))

(defun reformat-table (str parse-tree)
  "takes a table parse tree and returns a new string with all columns neatly aligned."
  (let ((col-widths (make-array 0 :adjustable t :fill-pointer t)))
    ;; calculate all column widths.
    (dolist (row-node (cltpt/combinator/match:match-children parse-tree))
      (when (eq (cltpt/combinator/match:match-id row-node) 'table-row)
        (let ((cell-nodes (remove-if-not
                           (lambda (node)
                             (eq (cltpt/combinator/match:match-id node) 'table-cell))
                           (cltpt/combinator/match:match-children row-node))))
          (loop for cell-node in cell-nodes
                for col-idx from 0
                do (let* ((content-node (car (cltpt/combinator/match:match-children cell-node)))
                          (cell-text (if content-node
                                         (cltpt/combinator:match-text content-node str)
                                         ""))
                          (cell-width (length cell-text)))
                     (when (>= col-idx (length col-widths))
                       (vector-push-extend 0 col-widths))
                     (setf (aref col-widths col-idx)
                           (max (aref col-widths col-idx) cell-width)))))))
    ;; build the formatted string directly from the parse tree.
    (let ((num-columns (length col-widths)))
      (with-output-to-string (s)
        (loop for child-node in (cltpt/combinator/match:match-children parse-tree)
              do
                 (case (cltpt/combinator/match:match-id child-node)
                   (table-row
                    (write-char *table-v-delimiter* s)
                    (let ((cell-nodes (remove-if-not
                                       (lambda (node)
                                         (eq (cltpt/combinator/match:match-id node) 'table-cell))
                                       (cltpt/combinator/match:match-children child-node))))
                      (loop for col-idx from 0 below num-columns
                            do (let ((width (aref col-widths col-idx))
                                     (cell-node (nth col-idx cell-nodes))
                                     (cell-text ""))
                                 (when cell-node
                                   (let ((content-node
                                           (car (cltpt/combinator/match:match-children
                                                 cell-node))))
                                     (when content-node
                                        (setf cell-text (cltpt/combinator:match-text
                                                         content-node
                                                         str)))))
                                 (format s " ~vA ~c" width cell-text *table-v-delimiter*)))))
                   (table-hrule
                    (write-char *table-v-delimiter* s)
                    (loop for width across col-widths for is-first-col = t then nil
                          do (unless is-first-col
                               (write-char *table-intersection-delimiter* s))
                             (dotimes (i (+ width 2))
                               (write-char *table-h-delimiter* s)))
                    (write-char *table-v-delimiter* s))
                   (table-row-separator
                    (write-char #\newline s))))))))

(defun get-cell-coordinates (cell-match)
  "takes a table-cell match and returns its (column . row) coordinates.
both column and row are 0-indexed."
  (let* ((row-node (cltpt/combinator/match:match-parent cell-match))
         (table-node (cltpt/combinator/match:match-parent row-node))
         (row-idx
           (loop for child in (cltpt/combinator/match:match-children table-node)
                 for i from 0
                 when (eq (cltpt/combinator/match:match-id child) 'table-row)
                   count child into row-count
                 when (eq child row-node)
                   return (1- row-count)))
         (col-idx
           (loop for child in (cltpt/combinator/match:match-children row-node)
                 for i from 0
                 when (eq (cltpt/combinator/match:match-id child) 'table-cell)
                   count child into cell-count
                 when (eq child cell-match)
                   return (1- cell-count))))
    (cons col-idx row-idx)))

(defun get-cell-at-coordinates (table-match coordinates)
  "takes an org-table match and a cons of (column . row) and returns the
cell at that location. both column and row are 0-indexed."
  (let* ((row-nodes (remove-if-not
                     (lambda (node)
                       (eq (cltpt/combinator/match:match-id node) 'table-row))
                     (cltpt/combinator/match:match-children table-match)))
         (target-row (nth (cdr coordinates) row-nodes)))
    (when target-row
      (let* ((cell-nodes (remove-if-not
                          (lambda (node)
                            (eq (cltpt/combinator/match:match-id node) 'table-cell))
                          (cltpt/combinator/match:match-children target-row))))
        (nth (car coordinates) cell-nodes)))))

(defun table-match-to-nested-list (str table-match &optional (include-hrules-p t))
  "takes an org-table match and returns a nested list representing the
table's data. ignores the new table-row-separator nodes."
  (let ((table-data))
    (dolist (row-node (cltpt/combinator/match:match-children table-match))
      (case (cltpt/combinator/match:match-id row-node)
        ('table-row
         (let ((current-row-cells)
               (cell-nodes (remove-if-not
                            (lambda (node)
                              (eq (cltpt/combinator/match:match-id node) 'table-cell))
                            (cltpt/combinator/match:match-children row-node))))
           (dolist (cell-node cell-nodes)
             (let* ((content-node (first (cltpt/combinator/match:match-children cell-node)))
                    (cell-text (if content-node
                                    (cltpt/combinator:match-text content-node str)
                                   "")))
               (push cell-text current-row-cells)))
           (push (nreverse current-row-cells) table-data)))
        ('table-hrule
         (when include-hrules-p
           (push :hrule table-data)))
        ('table-row-separator
         ;; do nothing
         )))
    (nreverse table-data)))

(defun nested-list-to-table-string (table-data)
  "takes a nested list (as generated by table-match-to-nested-list)
and returns a formatted org-mode table string."
  (let ((col-widths (make-array 0 :adjustable t :fill-pointer t)))
    ;; pass 1: calculate column widths
    (dolist (row-data table-data)
      (when (listp row-data) ;; ignore :hrule markers
        (loop for cell-text in row-data
              for col-idx from 0
              do
                 (let ((cell-width (length cell-text)))
                   (when (>= col-idx (length col-widths))
                     (vector-push-extend 0 col-widths))
                   (setf (aref col-widths col-idx)
                         (max (aref col-widths col-idx) cell-width))))))
    ;; pass 2: build the formatted string
    (with-output-to-string (s)
      (loop for row-data in table-data
            do
               (if (eq row-data :hrule)
                   (progn
                     (write-char *table-v-delimiter* s)
                     (loop for width across col-widths
                           for is-first-col = t then nil
                           do
                              (unless is-first-col
                                (write-char *table-intersection-delimiter* s))
                              (dotimes (i (+ width 2)) ;; +2 for padding
                                (write-char *table-h-delimiter* s)))
                     (write-char *table-v-delimiter* s)
                     (write-char #\newline s))
                   ;; it's a data row
                   (progn
                     (write-char *table-v-delimiter* s)
                     (loop for cell-text in row-data
                           for col-idx from 0
                           do (let ((width (aref col-widths col-idx)))
                                (format s " ~vA ~c"
                                        width cell-text *table-v-delimiter*)))
                     (write-char #\newline s)))))))

(defun get-table-height (table-match)
  "takes an org-table match and returns its height (number of data rows)."
  (count-if (lambda (node)
              (eq (cltpt/combinator/match:match-id node) 'table-row))
            (cltpt/combinator/match:match-children table-match)))

(defun get-table-width (table-match)
  "takes an org-table match and returns its width (number of columns)."
  (let ((first-row (find-if (lambda (node)
                              (eq (cltpt/combinator/match:match-id node) 'table-row))
                            (cltpt/combinator/match:match-children table-match))))
    (if first-row
        (count-if (lambda (node)
                    (eq (cltpt/combinator/match:match-id node) 'table-cell))
                  (cltpt/combinator/match:match-children first-row))
        0)))

(defun get-next-data-cell-coords (table-match coords)
  "takes a table-match and a coordinate pair (column . row). returns the
coordinates of the next data cell, or nil if at the end of the table.
handles wrapping to the next line automatically."
  (let ((width (get-table-width table-match))
        (height (get-table-height table-match))
        (current-x (car coords))
        (current-y (cdr coords)))
    (when (or (= width 0) (= height 0))
      (return-from get-next-data-cell-coords nil))
    ;; calculate the next linear position
    (let ((next-x (1+ current-x))
          (next-y current-y))
      ;; handle wrapping to the next row
      (when (>= next-x width)
        (setf next-x 0)
        (setf next-y (1+ next-y)))
      ;; check if we are past the end of the table
      (if (>= next-y height)
          nil ; we've iterated past the last cell
          (cons next-x next-y)))))