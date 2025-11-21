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

(defun count-leading-spaces (str start end)
  (loop for i from start below end
        while (char= (char str i) #\space)
        count 1))

(defun find-direct-child-by-id (node target-id)
  (when node
    (find target-id
          (cltpt/combinator:match-children node)
          :key #'cltpt/combinator:match-id)))

(defun split-string-lines (str)
  (with-input-from-string (s str)
    (loop for line = (read-line s nil nil)
          while line collect line)))

(defun clean-list-content (str)
  (let ((lines (split-string-lines str)))
    (if (null lines)
        ""
        (let* ((first-line (string-trim '(#\space #\tab #\newline)
                                        (first lines)))
               (other-lines (rest lines)))
          (if (null other-lines)
              first-line
              (let* ((non-empty-others
                       (remove-if
                        (lambda (l)
                          (string= "" (string-trim '(#\space #\tab) l)))
                        other-lines))
                     (min-indent
                       (if non-empty-others
                           (reduce #'min
                                   non-empty-others
                                   :key (lambda (l)
                                          (count-leading-spaces l 0 (length l))))
                           0)))
                (with-output-to-string (out)
                  (write-string first-line out)
                  (dolist (l other-lines)
                    (write-char #\newline out)
                    (if (string= "" (string-trim '(#\space #\tab) l))
                        (write-string "" out)
                        (write-string (subseq l (min (length l) min-indent))
                                      out))))))))))

(defun get-match-clean-content (str begin end)
  (if (and (< begin end)
           (<= end (length str)))
      (clean-list-content (subseq str begin end))
      ""))

(defun parse-bullet (str line-start line-end expected-indent)
  (unless (and (>= (- line-end line-start) expected-indent)
               (= (count-leading-spaces str line-start line-end) expected-indent))
    (return-from parse-bullet (values nil nil 0)))
  (let ((bullet-start (+ line-start expected-indent)))
    (cond
      ((and (<= (+ bullet-start 2) line-end)
            (char= (char str bullet-start) #\-)
            (char= (char str (1+ bullet-start)) #\space))
       (values t "-" 2))
      ((and (< bullet-start line-end)
            (alphanumericp (char str bullet-start)))
       (let ((marker-dot-pos
               (position #\.
                         str
                         :start (1+ bullet-start)
                         :end line-end)))
         (when (and marker-dot-pos
                    (loop for i from bullet-start below marker-dot-pos
                          always (alphanumericp (char str i))))
           (let* ((has-space-after
                    (and (< (1+ marker-dot-pos) line-end)
                         (char= (char str (1+ marker-dot-pos)) #\space)))
                  (bullet-struct-len
                    (+ (- (1+ marker-dot-pos) bullet-start)
                       (if has-space-after 1 0))))
             (values t
                     (subseq str bullet-start (1+ marker-dot-pos))
                     bullet-struct-len)))))
      (t (values nil nil 0)))))

(defun parse-single-list-item (ctx str item-start-pos item-indent inline-rules)
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds str item-start-pos)
    (multiple-value-bind (is-bullet marker bullet-len)
        (parse-bullet str line-start line-end item-indent)
      (unless is-bullet
        (return-from parse-single-list-item (values nil item-start-pos)))
      (let* ((children-of-item)
             (bullet-begin (+ line-start item-indent))
             (bullet-end (+ bullet-begin (length marker)))
             (content-segments)
             (current-scan-pos next-line-start)
             (end-of-content-text line-end))
        (push (cltpt/combinator:make-match
               :id 'list-item-bullet
               :begin bullet-begin
               :end bullet-end
               :str str)
              children-of-item)
        (let ((first-line-content-begin (+ bullet-begin bullet-len)))
          (when (< first-line-content-begin line-end)
            (push (cons first-line-content-begin line-end) content-segments)))
        (loop
          (when (>= current-scan-pos (length str))
            (return))
          (multiple-value-bind (next-l-start next-l-end next-l-next)
              (get-line-bounds str current-scan-pos)
            (let ((next-indent (count-leading-spaces str next-l-start next-l-end)))
              (if (and (> next-indent item-indent)
                       (not (nth-value 0 (parse-bullet str next-l-start next-l-end next-indent))))
                  (progn (let ((extra-start (+ next-l-start next-indent)))
                           (when (< extra-start next-l-end)
                             (push (cons extra-start next-l-end) content-segments)))
                         (setf current-scan-pos next-l-next)
                         (setf end-of-content-text next-l-end))
                  (return)))))
        (setf content-segments (nreverse content-segments))
        (let* ((children-of-content)
               (pos-after-children current-scan-pos)
               (end-of-children 0)
               (content-node-begin
                 (if content-segments
                     (caar content-segments)
                     (+ bullet-begin bullet-len))))
          (when (< current-scan-pos (length str))
            (multiple-value-bind (child-list-match pos-after-child)
                (org-list-matcher ctx str current-scan-pos inline-rules (1+ item-indent))
              (when child-list-match
                (push child-list-match children-of-content)
                (setf pos-after-children pos-after-child)
                (setf end-of-children (cltpt/combinator:match-end child-list-match)))))
          (when inline-rules
            (dolist (segment content-segments)
              (let ((matches-in-segment (cltpt/combinator::scan-all-rules
                                         ctx
                                         str
                                         inline-rules
                                         (car segment)
                                         (cdr segment))))
                (when matches-in-segment
                  (setf children-of-content (nconc children-of-content matches-in-segment))))))
          (let* ((final-match-end (max end-of-content-text end-of-children))
                 (final-next-pos (max current-scan-pos pos-after-children)))
            (push (cltpt/combinator:make-match
                   :id 'list-item-content
                   :begin content-node-begin :end final-match-end :str str
                   :children (sort children-of-content
                                   #'<
                                   :key #'cltpt/combinator:match-begin))
                  children-of-item)
            (values (cltpt/combinator:make-match
                     :id 'list-item
                     :begin item-start-pos
                     :end final-match-end
                     :str str
                     :props (list :indent item-indent)
                     :children (nreverse children-of-item))
                    final-next-pos)))))))

(defun parse-list-items-at-indent (ctx str initial-pos expected-indent inline-rules)
  (let ((item-nodes)
        (current-pos initial-pos)
        (last-successful-pos initial-pos))
    (loop
      (when (>= current-pos (length str))
        (return))
      (multiple-value-bind (ls le) (get-line-bounds str current-pos)
        (unless (= current-pos ls)
          (return))
        (when (>= ls (length str))
          (return))
        (let ((indent (count-leading-spaces str ls le)))
          (unless (= indent expected-indent)
            (return))
          (if (nth-value 0 (parse-bullet str ls le indent))
              (multiple-value-bind (item-cons-cell new-item-pos)
                  (parse-single-list-item ctx str current-pos expected-indent inline-rules)
                (if (and item-cons-cell (> new-item-pos current-pos))
                    (progn
                      (push item-cons-cell item-nodes)
                      (setf current-pos new-item-pos)
                      (setf last-successful-pos new-item-pos))
                    (return)))
              (return)))))
    (values (nreverse item-nodes) last-successful-pos)))

(defun org-list-matcher (ctx str pos &optional inline-rules (minimum-indent 0))
  (multiple-value-bind (ls le) (get-line-bounds str pos)
    (unless (= pos ls)
      (return-from org-list-matcher (values nil pos)))
    (let ((indent (count-leading-spaces str ls le)))
      (unless (and (>= indent minimum-indent)
                   (nth-value 0 (parse-bullet str ls le indent)))
        (return-from org-list-matcher (values nil pos)))
      (multiple-value-bind (nodes final-pos)
          (parse-list-items-at-indent ctx str pos indent inline-rules)
        (if nodes
            (values (cltpt/combinator:make-match
                     :id 'org-list
                     :begin pos
                     :end (cltpt/combinator:match-end (car (last nodes)))
                     :str str
                     :children nodes
                     :props (list :indent indent))
                    final-pos)
            (values nil pos))))))

(defun list-match-to-nested-list (list-match)
  "converts an org-list match tree into a nested lisp list structure."
  (unless (and list-match (eq (cltpt/combinator:match-id list-match) 'org-list))
    (return-from list-match-to-nested-list nil))
  (let ((items (cltpt/combinator:match-children list-match))
        (result-list))
    (dolist (item items)
      (when (eq (cltpt/combinator:match-id item) 'list-item)
        (let* ((bullet-node (find-direct-child-by-id item 'list-item-bullet))
               (content-node (find-direct-child-by-id item 'list-item-content))
               (bullet-text (if bullet-node
                                (cltpt/combinator:match-text bullet-node)
                                "-"))
               (item-data (list :bullet bullet-text
                                :content ""
                                :children nil)))
          (when content-node
            (let* ((str (cltpt/combinator:match-str content-node))
                   (content-begin (cltpt/combinator:match-begin content-node))
                   (sub-list-node (find-direct-child-by-id content-node 'org-list))
                   (text-end (if sub-list-node
                                 (cltpt/combinator:match-begin sub-list-node)
                                 (cltpt/combinator:match-end content-node))))
              (setf (getf item-data :content)
                    (get-match-clean-content str content-begin text-end))
              (when sub-list-node
                (setf (getf item-data :children) (list-match-to-nested-list sub-list-node)))))
          (push item-data result-list))))
    (nreverse result-list)))

(defun nested-list-to-list-string (nested-list &optional (indent-level 0))
  "converts nested list structure to an org-list string.
returns the list string *without* a trailing newline."
  (with-output-to-string (s)
    ;; loop used to handle separators between items correctly
    (loop for (item . rest) on nested-list
          do (let ((bullet (getf item :bullet))
                   (content (getf item :content))
                   (children (getf item :children))
                   (indent-str (make-string indent-level :initial-element #\space)))
               ;; write bullet and indent
               (format s "~a~a " indent-str bullet)
               ;; write content
               (let ((lines (split-string-lines content)))
                 (when lines
                   (write-string (first lines) s)
                   (dolist (line (cdr lines))
                     ;; 2 spaces indent matches "- " length
                     (format s "~%~a  ~a" indent-str line))))
               ;; write children (if any)
               (when children
                 ;; need newline before entering sub-list
                 (write-char #\newline s)
                 (write-string (nested-list-to-list-string children (+ indent-level 2)) s))
               ;; write separator (only if there is another item following)
               (when rest
                 (write-char #\newline s))))))

(defun nested-list-to-list-match (ctx nested-list &optional inline-rules)
  (let ((generated-text (nested-list-to-list-string nested-list)))
    (org-list-matcher ctx generated-text 0 inline-rules)))

(defun reformat-list (parse-tree)
  "normalizes indentation and spacing.
returns the string representation of the list structure (no trailing newline)."
  (let ((data (list-match-to-nested-list parse-tree))
        (indent (getf (cltpt/combinator:match-props parse-tree) :indent 0)))
    (nested-list-to-list-string data indent)))

(defun get-list-item-indices (root-list-node target-pos-or-node)
  (let ((target-pos (if (typep target-pos-or-node 'cltpt/combinator:match)
                        (cltpt/combinator:match-begin target-pos-or-node)
                        target-pos-or-node)))
    (unless (and (>= target-pos (cltpt/combinator:match-begin root-list-node))
                 (< target-pos (cltpt/combinator:match-end root-list-node)))
      (return-from get-list-item-indices nil))
    (let ((children (cltpt/combinator:match-children root-list-node)))
      (loop for child in children
            for i from 0
            when (eq (cltpt/combinator:match-id child) 'list-item)
              do (when (and (>= target-pos (cltpt/combinator:match-begin child))
                            (< target-pos (cltpt/combinator:match-end child)))
                   (let* ((content-node (find-direct-child-by-id child 'list-item-content))
                          (sub-list-node (and content-node
                                              (find-direct-child-by-id content-node 'org-list))))
                     (if (and sub-list-node
                              (>= target-pos (cltpt/combinator:match-begin sub-list-node))
                              (< target-pos (cltpt/combinator:match-end sub-list-node)))
                         (return-from get-list-item-indices
                           (cons i (get-list-item-indices sub-list-node target-pos)))
                         (return-from get-list-item-indices (list i)))))))))

(defun get-item-at-indices (root-list-node indices)
  (let ((current-list root-list-node)
        (current-item))
    (dolist (idx indices)
      (unless (and current-list (eq (cltpt/combinator:match-id current-list) 'org-list))
        (return-from get-item-at-indices nil))
      (let ((items (remove-if-not
                    (lambda (n)
                      (eq (cltpt/combinator:match-id n) 'list-item))
                    (cltpt/combinator:match-children current-list))))
        (if (< idx (length items))
            (setf current-item (nth idx items))
            (return-from get-item-at-indices nil)))
      (let* ((content-node (find-direct-child-by-id current-item 'list-item-content)))
        (setf current-list (and content-node (find-direct-child-by-id content-node 'org-list)))))
    current-item))

(defun get-list-item-text (item-node)
  (let ((content-node (find-direct-child-by-id item-node 'list-item-content)))
    (if content-node
        (let* ((str (cltpt/combinator:match-str content-node))
               (begin (cltpt/combinator:match-begin content-node))
               (sub-list (find-direct-child-by-id content-node 'org-list))
               (end (if sub-list
                        (cltpt/combinator:match-begin sub-list)
                        (cltpt/combinator:match-end content-node))))
          (get-match-clean-content str begin end))
        "")))

(defun get-list-depth (root-list-node)
  (let ((depth 1)
        (children (cltpt/combinator:match-children root-list-node)))
    (dolist (child children)
      (when (eq (cltpt/combinator:match-id child) 'list-item)
        (let* ((content (find-direct-child-by-id child 'list-item-content))
               (sub-list (when content
                           (find-direct-child-by-id content 'org-list))))
          (when sub-list
            (setf depth (max depth (1+ (get-list-depth sub-list))))))))
    depth))

(defun get-list-type (list-node)
  (let* ((children (cltpt/combinator:match-children list-node))
         (first-item (when children
                       (first children)))
         (bullet-node (when first-item
                        (find-direct-child-by-id first-item 'list-item-bullet)))
         (marker (when bullet-node
                   (cltpt/combinator:match-text bullet-node))))
    (if (and marker (string= marker "-"))
        :ul
        :ol)))

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

(defun to-html-list (parse-tree)
  (to-html-list-recursive parse-tree))

(defun to-html-list-recursive (node)
  (when node
    (case (cltpt/combinator:match-id node)
      ('org-list
       (let* ((list-type (get-list-type node))
              (tag (if (eq list-type :ul)
                       "ul"
                       "ol"))
              (type-attr (when (eq list-type :ol)
                           (let* ((first-item (car (cltpt/combinator:match-children node)))
                                  (bullet-node
                                    (when first-item
                                      (find-direct-child-by-id first-item
                                                               'list-item-bullet)))
                                  (html-type
                                    (when bullet-node
                                      (get-html-ol-type
                                       (cltpt/combinator:match-text bullet-node)))))
                             (when html-type (format nil " type=\"~a\"" html-type))))))
         (format nil
                 "<~a~a>~%~{~a~}</~a>~%"
                 tag
                 (or type-attr "")
                 (mapcar 'to-html-list-recursive (cltpt/combinator:match-children node))
                 tag)))
      ('list-item
       (format nil
               "<li>~{~a~}</li>~%"
               (mapcar 'to-html-list-recursive (cltpt/combinator:match-children node))))
      ('list-item-content
       (with-output-to-string (s)
         (let ((current-pos (cltpt/combinator:match-begin node))
               (original-str (cltpt/combinator:match-str node)))
           (dolist (child (cltpt/combinator:match-children node))
             (let ((child-begin (cltpt/combinator:match-begin child)))
               (write-string (subseq original-str current-pos child-begin) s)
               (write-string (to-html-list-recursive child) s)
               (setf current-pos (cltpt/combinator:match-end child))))
           (write-string (subseq original-str current-pos (cltpt/combinator:match-end node))
                         s))))
      ('list-item-bullet "")
      (t (cltpt/combinator:match-text node)))))

(defun to-latex-list (parse-tree)
  (to-latex-list-recursive parse-tree))

(defun to-latex-list-recursive (node &optional (depth 0))
  (when node
    (case (cltpt/combinator:match-id node)
      ('org-list
       (let* ((list-type (get-list-type node))
              (env (if (eq list-type :ul)
                       "itemize"
                       "enumerate"))
              (label-command
                (when (eq list-type :ol)
                  (let* ((first-item (first (cltpt/combinator:match-children node)))
                         (bullet-node (when first-item
                                        (find-direct-child-by-id first-item 'list-item-bullet))))
                    (when bullet-node
                      (get-latex-label-command (cltpt/combinator:match-text bullet-node)
                                               depth))))))
         (format nil
                 "\\begin{~a}~@[~%~a~]~%~{~a~}\\end{~a}~%"
                 env
                 label-command
                 (mapcar
                  (lambda (child)
                    (to-latex-list-recursive child (1+ depth)))
                  (cltpt/combinator:match-children node))
                 env)))
      ('list-item
       (format nil
               "\\item ~{~a~}~%"
               (mapcar (lambda (child)
                         (to-latex-list-recursive child depth))
                       (cltpt/combinator:match-children node))))
      ('list-item-content
       (with-output-to-string (s)
         (let ((current-pos (cltpt/combinator:match-begin node))
               (original-str (cltpt/combinator:match-str node)))
           (dolist (child (cltpt/combinator:match-children node))
             (let ((child-begin (cltpt/combinator:match-begin child)))
               (write-string (subseq original-str current-pos child-begin) s)
               (write-string (to-latex-list-recursive child depth) s)
               (setf current-pos (cltpt/combinator:match-end child))))
           (write-string (subseq original-str current-pos (cltpt/combinator:match-end node))
                         s))))
      ('list-item-bullet "")
      (t (cltpt/combinator:match-text node)))))