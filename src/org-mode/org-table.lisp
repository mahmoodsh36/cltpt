(in-package :cltpt/org-mode)

(defun whitespacep (char)
  "checks if a character is a standard whitespace character."
  (member char '(#\space #\tab #\newline #\return #\linefeed)))

(defun find-trim-indices (str start end)
  "returns a (start . end) cons pair for the given slice of a string."
  (let ((s (position-if-not #'whitespacep str :start start :end end))
        (e (position-if-not #'whitespacep str :start start :end end :from-end t)))
    (if s
        (cons s (1+ e))
        ;; if the slice is all whitespace, return a zero-length slice at the start.
        (cons start start))))

(defun split-line-by-indices (char str start end)
  "splits a line (defined by start/end indices) by a character.
returns a list of (start . end) cons pairs for each segment."
  ;; assumes the line format is like `|cell1|cell2|cell3|`
  (when (and (< start end) (char= (char str start) #\|))
    (loop with current = (1+ start)
          while (< current end)
          collect (let ((next-pipe (or (position char str :start current :end end)
                                       (1- end))))
                    (prog1 (cons current next-pipe)
                      (setf current (1+ next-pipe)))))))

(defun org-table-get-line-info (str pos)
  "returns (values line-start-index line-end-index next-line-start-index is-last-line-p)."
  (when (>= pos (length str))
    (return-from org-table-get-line-info
      (values (length str) (length str) (length str) t)))
  (let* ((line-start (or (position #\newline str :end pos :from-end t) -1))
         (actual-line-start (1+ line-start))
         (line-end (or (position #\newline str :start actual-line-start)
                       (length str))))
    (values actual-line-start
            line-end
            (if (< line-end (length str))
                (1+ line-end)
                line-end)
            (>= line-end (length str)))))

(defun parse-table-row (str line-start line-end inline-rules)
  (let* ((trimmed-indices (find-trim-indices str line-start line-end))
         (start (car trimmed-indices))
         (end (cdr trimmed-indices)))
    (when (>= start end)
      (return-from parse-table-row (values nil line-start)))
    (let ((first-char (char str start)))
      (cond
        ;; case 1: horizontal separator line
        ((or (and (char= first-char #\|)
                  (> end (1+ start))
                  (member (char str (1+ start)) '(#\- #\+)))
             (and (char= first-char #\+)
                  (> end (1+ start))
                  (char= (char str (1+ start)) #\-)))
         (values
          (cons (list :id 'h-separator
                      :begin start
                      :end end
                      :match (subseq str start end))
                nil)
          end))
        ;; case 2: regular data row
        ((char= first-char #\|)
         (let ((cell-nodes)
               (cell-indices (split-line-by-indices #\| str start end)))
           (loop for (cell-start . cell-end) in cell-indices
                 do
                    (let* ((trimmed-cell (find-trim-indices str cell-start cell-end))
                           (cell-content-start (car trimmed-cell))
                           (cell-content-end (cdr trimmed-cell))
                           (cell-text (subseq str cell-content-start cell-content-end))
                           (inline-children
                             (when (and inline-rules (plusp (length cell-text)))
                               (adjust-match-offsets
                                (cltpt/combinator::scan-all-rules cell-text inline-rules)
                                cell-content-start))))
                      (push (cons (list :id 'table-cell
                                        :begin cell-content-start
                                        :end cell-content-end
                                        :match cell-text)
                                  inline-children)
                            cell-nodes)))
           (values
            (cons (list :id 'table-row
                        :begin start
                        :end end)
                  (nreverse cell-nodes))
            end)))
        ;; not a valid table row
        (t (values nil line-start))))))

(defun org-table-matcher (str pos &optional inline-rules)
  "main function to find and parse an entire org-mode table."
  (multiple-value-bind (first-line-start first-line-end)
      (org-table-get-line-info str pos)
    (unless (= pos first-line-start)
      (return-from org-table-matcher (values nil pos)))
    (let* ((trimmed (find-trim-indices str first-line-start first-line-end))
           (start (car trimmed)))
      (unless (and (< start (cdr trimmed)) (char= (char str start) #\|))
        (return-from org-table-matcher (values nil pos)))
    ;; it's a table, so start parsing line by line
    (let ((row-nodes)
          (current-pos pos)
          (table-end-pos pos))
      (loop
        (when (>= current-pos (length str)) (return))
        (multiple-value-bind (line-start line-end next-pos is-last)
            (org-table-get-line-info str current-pos)
          (setf table-end-pos line-start)
          (multiple-value-bind (row-node row-end-pos)
              (parse-table-row str line-start line-end inline-rules)
            (if row-node
                (progn
                  (push row-node row-nodes)
                  (setf table-end-pos (if is-last row-end-pos next-pos))
                  (setf current-pos next-pos))
                (return)))))
      (if row-nodes
          (let ((table-begin-offset pos)
                (table-end-offset (string-end-pos str table-end-pos)))
            (values
             (cons (list :id 'org-table
                         :begin table-begin-offset
                         :end table-end-offset
                         :match (subseq str table-begin-offset table-end-offset))
                   (nreverse row-nodes))
             table-end-offset))
          (values nil pos))))))

(defun string-end-pos (str pos)
  (if (and (> pos 0) (char= (char str (1- pos)) #\newline))
    (1- pos)
    pos))

(defun to-html-table (parse-tree)
  (when (and parse-tree (eq (getf (car parse-tree) :id) 'org-table))
    (let ((children (cdr parse-tree))
          (header-p (and (> (length children) 1)
                         (eq (getf (caar (second children)) :id) 'h-separator))))
      (with-output-to-string (s)
        (write-string "<table>" s)
        (loop for row-node in children
              for is-first = t then nil
              do (case (getf (car row-node) :id)
                   ('table-row
                    (write-string "<tr>" s)
                    (loop for cell-node in (cdr row-node)
                          do (format s "<~a>~a</~a>"
                                     (if (and is-first header-p) "th" "td")
                                     (getf (car cell-node) :match)
                                     (if (and is-first header-p) "th" "td")))
                    (write-string "</tr>" s))
                   ('h-separator (when is-first (write-string "" s)))))
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
                                      (getf (car cell) :match))
                                    (cdr row-node))))
                   ('h-separator
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
                    (let* ((cell-text (getf (car cell-node) :match))
                           (cell-width (length cell-text)))
                      (when (>= col-idx (length col-widths))
                        (vector-push-extend 0 col-widths))
                      (setf (aref col-widths col-idx)
                            (max (aref col-widths col-idx) cell-width))
                      (push cell-text current-row-cells)))
           (push (nreverse current-row-cells) table-data)))
        ('h-separator
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
                              ;; if it's not the first column, print the '+' joiner.
                              (unless is-first-col
                                (write-char #\+ s))
                              ;; print the dashes for the current column, plus padding.
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