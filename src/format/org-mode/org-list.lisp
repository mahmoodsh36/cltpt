(in-package :cltpt/org-mode)

(defun get-line-bounds (str pos)
  "returns (values line-start, line-end, next-line-start) for the line at pos."
  (when (>= pos (length str))
    (return-from get-line-bounds
      (values (length str)
              (length str)
              (length str))))
  (let* ((line-start (or (position #\newline str :end pos :from-end t)
                         -1))
         (actual-line-start (1+ line-start))
         (line-end (or (position #\newline str :start actual-line-start)
                       (length str)))
         (next-line-start (if (< line-end (length str))
                              (1+ line-end)
                              line-end)))
    (values actual-line-start line-end next-line-start)))

(defun count-leading-spaces (str start end)
  "counts leading spaces in a segment of str defined by [start, end)."
  (loop for i from start below end
        while (char= (char str i) #\space)
        count 1))

(defun parse-bullet (str line-start line-end expected-indent)
  "parses a bullet from a line segment.

returns (values is-bullet-p, marker-string, length-of-bullet-structure)."
  (unless (and (>= (- line-end line-start) expected-indent)
               (= (count-leading-spaces str line-start line-end) expected-indent))
    (return-from parse-bullet (values nil nil 0)))
  (let ((bullet-start (+ line-start expected-indent)))
    (cond
      ;; dash bullet: "- "
      ((and (<= (+ bullet-start 2) line-end)
            (char= (char str bullet-start) #\-)
            (char= (char str (1+ bullet-start)) #\space))
       (values t "-" 2))
      ;; number/letter bullet: "1.", "a." etc.
      ((and (< bullet-start line-end)
            (alphanumericp (char str bullet-start)))
       (let ((marker-dot-pos (position #\.
                                       str
                                       :start (1+ bullet-start)
                                       :end line-end)))
         (when (and marker-dot-pos
                    ;; ensure all characters between bullet-start and dot are alphanumeric
                    (loop for i from bullet-start below marker-dot-pos
                          always (alphanumericp (char str i))))
           (let* ((has-space-after
                    (and (< (1+ marker-dot-pos) line-end)
                         (char= (char str (1+ marker-dot-pos))
                                #\space)))
                  (bullet-struct-len
                    (+ (- (1+ marker-dot-pos) bullet-start)
                       (if has-space-after 1 0))))
             (values t
                     (subseq str bullet-start (1+ marker-dot-pos)) ;; subseq ok for small, local value
                     bullet-struct-len)))))
      (t (values nil nil 0)))))

(defun parse-single-list-item (ctx str item-start-pos item-indent inline-rules)
  "parses one list item, including its content, extra lines, and nested child lists.
returns (values item-node, pos-after-item)."
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds str item-start-pos)
    (multiple-value-bind (is-bullet marker bullet-len)
        (parse-bullet str line-start line-end item-indent)
      (unless is-bullet
        (return-from parse-single-list-item
          (values nil item-start-pos)))
      (let* ((children-of-item)
             (bullet-begin (+ line-start item-indent))
             (bullet-end (+ bullet-begin (length marker)))
             (content-segments)
             (current-scan-pos next-line-start)
             (end-of-content-lines next-line-start))
        (push (cons
               (list :id 'list-item-bullet
                     :begin bullet-begin
                     :end bullet-end
                     :str str)
               nil)
              children-of-item)
        (let ((first-line-content-begin (+ bullet-begin bullet-len)))
          (when (< first-line-content-begin line-end)
            (push (cons first-line-content-begin line-end) content-segments)))
        (loop
          (when (>= current-scan-pos (length str)) (return))
          (multiple-value-bind (next-l-start next-l-end next-l-next)
              (get-line-bounds str current-scan-pos)
            (let ((next-indent (count-leading-spaces str next-l-start next-l-end)))
              (if (and (> next-indent item-indent)
                       (not (nth-value 0
                                       (parse-bullet str
                                                     next-l-start
                                                     next-l-end
                                                     next-indent))))
                  (progn
                    (let ((extra-line-content-start (+ next-l-start next-indent)))
                      (when (< extra-line-content-start next-l-end)
                        (push (cons extra-line-content-start next-l-end)
                              content-segments)))
                    (setf current-scan-pos next-l-next)
                    (setf end-of-content-lines next-l-next))
                  (return)))))
        (setf content-segments (nreverse content-segments))
        (let* ((children-of-content)
               (pos-after-children current-scan-pos)
               (final-end-pos 0) ;; calculated later
               (content-node-begin
                 (if content-segments
                     (caar content-segments)
                     (+ bullet-begin bullet-len))))
          (when (< current-scan-pos (length str))
            (multiple-value-bind (child-list-match pos-after-child)
                (org-list-matcher ctx
                                  str
                                  current-scan-pos
                                  inline-rules
                                  (1+ item-indent))
              (when child-list-match
                (push child-list-match children-of-content)
                (setf pos-after-children pos-after-child))))
          (when inline-rules
            (dolist (segment content-segments)
              (let ((matches-in-segment
                      (cltpt/combinator::scan-all-rules
                       ctx
                       str
                       inline-rules
                       (car segment)
                       (cdr segment))))
                (when matches-in-segment
                  (setf children-of-content
                        (nconc children-of-content
                               matches-in-segment))))))
          ;; calculate the final end position for the *entire item* first.
          (setf final-end-pos (max end-of-content-lines pos-after-children))
          ;; now create the content node, using this correct, encompassing end position.
          ;; the content node's range now correctly includes its text AND its children's ranges.
          (push (cons (list :id 'list-item-content
                            :begin content-node-begin
                            :end final-end-pos
                            :str str)
                      (sort children-of-content
                            #'<
                            :key (lambda (n)
                                   (getf (car n) :begin))))
                children-of-item)
          (let* ((item-parent-info (list :id 'list-item
                                         :indent item-indent
                                         :begin item-start-pos
                                         :end final-end-pos
                                         :str str)))
            (values (cons item-parent-info (nreverse children-of-item))
                    final-end-pos)))))))

(defun parse-list-items-at-indent (ctx str initial-pos expected-indent inline-rules)
  "parses a sequence of sibling list items.
returns (values list-of-item-nodes, pos-after-last-item)."
  (let ((item-nodes)
        (current-pos initial-pos)
        (last-successful-pos initial-pos))
    (loop
      (when (>= current-pos (length str))
        (return))
      (multiple-value-bind (line-start line-end) (get-line-bounds str current-pos)
        (unless (= current-pos line-start)
          (return))
        (when (>= line-start (length str))
          (return))
        (let ((indent-on-this-line (count-leading-spaces str line-start line-end)))
          (unless (= indent-on-this-line expected-indent) (return))

          (if (nth-value
               0
               (parse-bullet str
                             line-start
                             line-end
                             indent-on-this-line))
              (multiple-value-bind (item-cons-cell new-item-pos)
                  (parse-single-list-item
                   ctx str current-pos expected-indent inline-rules)
                (if (and item-cons-cell (> new-item-pos current-pos))
                    (progn
                      (push item-cons-cell item-nodes)
                      (setf current-pos new-item-pos)
                      (setf last-successful-pos new-item-pos))
                    (return)))
              (return)))))
    (values (nreverse item-nodes) last-successful-pos)))

(defun org-list-matcher (ctx str pos &optional inline-rules (minimum-indent 0))
  "parses an org-mode list starting at pos, ensuring its indent is >= MINIMUM-INDENT.
returns (values match-node, new-pos)."
  (multiple-value-bind (first-line-start first-line-end) (get-line-bounds str pos)
    (unless (= pos first-line-start)
      (return-from org-list-matcher (values nil pos)))
    (let ((initial-indent (count-leading-spaces
                           str
                           first-line-start
                           first-line-end)))
      (unless (and (>= initial-indent minimum-indent)
                   (nth-value
                    0
                    (parse-bullet str
                                  first-line-start
                                  first-line-end
                                  initial-indent)))
        (return-from org-list-matcher (values nil pos)))
      (multiple-value-bind (top-level-item-nodes final-pos-after-list)
          (parse-list-items-at-indent ctx str pos initial-indent inline-rules)
        (if top-level-item-nodes
            (let ((list-parent-info (list :id 'org-list
                                          :begin pos
                                          :end final-pos-after-list
                                          :str str
                                          :indent initial-indent)))
              (values (cons list-parent-info top-level-item-nodes)
                      final-pos-after-list))
            (values nil pos))))))

(defun get-list-type (list-node)
  (let* ((children (cdr list-node))
         (first-item (when children (first children)))
         (bullet-node (when first-item
                        (find 'list-item-bullet
                              (cdr first-item)
                              :key (lambda (n) (getf (car n) :id)))))
         (marker (when bullet-node (cltpt/combinator:match-text (car bullet-node)))))
    (if (and marker (string= marker "-")) :ul :ol)))

(defun get-html-ol-type (bullet-marker)
  (when (and bullet-marker (> (length bullet-marker) 0))
    (let ((char (char bullet-marker 0)))
      (cond ((digit-char-p char) "1")
            ((char-equal char #\a) "a")
            ((char-equal char #\i) "i")
            ((char-equal char #\I) "I")
            (t "1")))))

(defun get-latex-label-command (bullet-marker depth)
  (let ((counter (case depth
                   (0 "enumi")
                   (1 "enumii")
                   (2 "enumiii")
                   (t "enumiv")))
        (command (when (and bullet-marker (> (length bullet-marker) 0))
                   (let ((char (char bullet-marker 0)))
                     (cond ((digit-char-p char) "\\arabic")
                           ((char-equal char #\a) "\\alph")
                           ((char-equal char #\i) "\\roman")
                           ((char-equal char #\I) "\\Roman")
                           (t "\\arabic"))))))
    (when command
      (format nil "\\renewcommand{\\label~a}{~a{~a.}}" counter command counter))))