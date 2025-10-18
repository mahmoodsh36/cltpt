(in-package :cltpt/org-mode)

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
  "checks if the line at pos starts with '|' or '+' after trimming whitespace."
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
                   '(#\| #\+))))))

(defun is-hrule-line-at-pos-p (str pos)
  "checks if the line at pos is a horizontal rule (e.g., |---+---| or +---+)."
  (multiple-value-bind (line-start line-end) (get-line-bounds str pos)
    (let ((trimmed-start
            (position-if-not (lambda (c) (member c '(#\space #\tab)))
                             str :start line-start :end line-end)))
      (and trimmed-start
           ;; an hrule can start with '|' or '+'.
           (member (char str trimmed-start) '(#\| #\+))
           ;; and the rest of the line must only contain valid rule characters.
           (loop for i from (1+ trimmed-start) below line-end
                 for char = (char str i)
                 always (member char '(#\space #\tab #\- #\+ #\|)))))))

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
        (values start start)))) ;; all whitespace, return zero-length slice

(defun parse-table-row (ctx str row-start-offset inline-rules)
  "parses a single table row, creating nodes for cells and their content.

returns (values row-node, next-line-start-offset)."
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds str row-start-offset)
    (let* ((trimmed-line-start
             (position-if-not
              (lambda (c) (member c '(#\space #\tab)))
              str
              :start line-start
              :end line-end))
           (cell-nodes)
           (current-cell-start (1+ trimmed-line-start))) ;; start after the first '|'
      (loop for pipe-pos = (position #\|
                                     str
                                     :start current-cell-start
                                     :end line-end)
            while pipe-pos
            do
               (multiple-value-bind (content-begin content-end)
                   (find-content-bounds str current-cell-start pipe-pos)
                 (let ((cell-children
                         (when (and inline-rules (< content-begin content-end))
                           (cltpt/combinator:scan-all-rules ctx
                                                            str
                                                            inline-rules
                                                            content-begin
                                                            content-end))))
                   (let ((cell-parent-info (list :id 'table-cell
                                                 :begin content-begin
                                                 :end content-end
                                                 :str str)))
                     (push (cons cell-parent-info cell-children) cell-nodes))))
               (setf current-cell-start (1+ pipe-pos)))
      (let ((row-parent-info (list :id 'table-row
                                   :begin trimmed-line-start
                                   :end line-end
                                   :str str)))
        (values (cons row-parent-info (nreverse cell-nodes))
                next-line-start)))))

(defun org-table-matcher (ctx str pos &optional inline-rules)
  "parses an org-mode table starting at pos. returns (values match-node, new-pos)."
  (multiple-value-bind (first-line-start) (get-line-bounds str pos)
    (unless (and (= pos first-line-start) (is-table-line-at-pos-p str pos))
      (return-from org-table-matcher (values nil pos))))
  (let ((row-nodes)
        (current-pos pos)
        (last-successful-pos pos))
    (loop
      (when (>= current-pos (length str))
        (return))
      (multiple-value-bind (line-start line-end next-start) (get-line-bounds str current-pos)
        (unless (and (= current-pos line-start)
                     (is-table-line-at-pos-p str line-start))
          (return))
        (if (is-hrule-line-at-pos-p str line-start)
            (let* ((trimmed-start
                     (position-if-not (lambda (c) (member c '(#\space #\tab)))
                                      str
                                      :start line-start
                                      :end line-end))
                   (hrule-parent-info (list :id 'table-hrule
                                            :begin trimmed-start
                                            :end line-end
                                            :str str)))
              (push (cons hrule-parent-info nil) row-nodes))
            (multiple-value-bind (row-node)
                (parse-table-row ctx str line-start inline-rules)
              (push row-node row-nodes)))
        (setf current-pos next-start)
        (setf last-successful-pos current-pos)))
    (if row-nodes
        (let ((table-parent-info (list :id 'org-table
                                       :begin pos
                                       :end last-successful-pos
                                       :str str)))
          (values (cons table-parent-info (nreverse row-nodes))
                  last-successful-pos))
        (values nil pos))))

(defun to-html-table (parse-tree)
  "converts a table parse tree to an equivalent HTML table."
  (when (and parse-tree (eq (getf (car parse-tree) :id) 'org-table))
    (let* ((children (cdr parse-tree))
           (header-p (and (> (length children) 1)
                          (eq (getf (car (second children)) :id)
                              'table-hrule))))
      (with-output-to-string (s)
        (write-string "<table>" s)
        (loop for row-node in children
              for is-first = t then nil
              do (case (getf (car row-node) :id)
                   ('table-row
                    (write-string "<tr>" s)
                    (loop for cell-node in (cdr row-node)
                          do (format s
                                     "<~a>~a</~a>"
                                     (if (and is-first header-p) "th" "td")
                                     (cltpt/combinator:match-text
                                      (car cell-node))
                                     (if (and is-first header-p) "th" "td")))
                    (write-string "</tr>" s))
                   ('table-hrule (when is-first (write-string "" s)))))
        (write-string "</table>" s)))))

(defun to-latex-table (parse-tree)
  "converts a table parse tree to a latex table."
  (when (and parse-tree (eq (getf (car parse-tree) :id) 'org-table))
    (let* ((children (cdr parse-tree))
           (first-data-row
             (find 'table-row
                   children
                   :key (lambda (n) (getf (car n) :id))))
           (num-cols (if first-data-row
                         (length (cdr first-data-row))
                         1))
           (col-spec (format nil "{ |~{~a~^|~}| }"
                             (loop repeat num-cols collect "l"))))
      (with-output-to-string (s)
        (format s "\\begin{tabular}~a~%" col-spec)
        (format s "\\hline~%")
        (loop for row-node in children
              do (case (getf (car row-node) :id)
                   ('table-row
                    (format s "~{~a~^ & ~} \\\\~%"
                            (mapcar (lambda (cell)
                                      (cltpt/combinator:match-text (car cell)))
                                    (cdr row-node))))
                   ('table-hrule
                    (format s "\\hline~%"))))
        (format s "\\hline~%")
        (format s "\\end{tabular}")))))

(defun reformat-table (parse-tree)
  "takes a table parse tree and returns a new string with all columns

neatly aligned based on the widest cell in each column."
  ;; pass 1: check column widths
  (let ((col-widths (make-array 0 :adjustable t :fill-pointer t))
        (table-data))
    (dolist (row-node (cdr parse-tree))
      (case (getf (car row-node) :id)
        ('table-row
         (let ((current-row-cells))
           (loop for cell-node in (cdr row-node)
                 for col-idx from 0
                 do
                    (let* ((cell-text
                             (cltpt/combinator:match-text (car cell-node)))
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
                   ;; this is a horizontal separator row
                   (progn
                     (write-char #\| s)
                     (loop for width across col-widths
                           for is-first-col = t then nil
                           do
                              (unless is-first-col
                                (write-char #\+ s))
                              (dotimes (i (+ width 2)) (write-char #\- s)))
                     (write-line "|" s))
                   ;; this is a regular data row
                   (progn
                     (write-char #\| s)
                     (loop for cell-text in row-data
                           for col-idx from 0
                           do (let ((width (aref col-widths col-idx)))
                                (format s " ~vA |" width cell-text)))
                     (write-char #\newline s)))))))