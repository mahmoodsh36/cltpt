(in-package :cltpt/org-mode)

;; (defun get-line-bounds (reader pos)
;;   "returns (values line-start, line-end, next-line-start) for the line at pos."
;;   (when (cltpt/reader:is-after-eof reader pos)
;;     (return-from get-line-bounds
;;       (values (cltpt/reader:reader-buffer-fill reader)
;;               (cltpt/reader:reader-buffer-fill reader)
;;               (cltpt/reader:reader-buffer-fill reader))))
;;   ;; find previous newline by iterating backwards (can't use :from-end with reader)
;;   (let* ((line-start (or (loop for i from (1- pos) downto 0
;;                                when (char= (cltpt/reader:reader-char reader i) #\newline)
;;                                  return i)
;;                          -1))
;;          (actual-line-start (1+ line-start))
;;          (line-end (or (position #\newline reader :start pos)
;;                        (cltpt/reader:reader-buffer-fill reader)))
;;          (next-line-start (if (< line-end (cltpt/reader:reader-buffer-fill reader))
;;                               (1+ line-end)
;;                               line-end)))
;;     (values actual-line-start line-end next-line-start)))

;; this version is much faster than the one above.
(defun get-line-bounds (reader pos)
  "returns (values line-start, line-end, next-line-start) for the line at pos."
  (let* ((buf (cltpt/reader:reader-buffer reader))
         (buf-len (fill-pointer buf)))
    ;; check EOF using cached buffer length
    (when (>= pos buf-len)
      (when (cltpt/reader:reader-eof-reached reader)
        (return-from get-line-bounds (values buf-len buf-len buf-len)))
      (when (cltpt/reader:is-after-eof reader pos)
        (let ((new-len (fill-pointer buf)))
          (return-from get-line-bounds (values new-len new-len new-len)))))
    ;; find previous newline by direct buffer access
    (let* ((line-start (or (loop for i fixnum from (1- pos) downto 0
                                 when (char= (char buf i) #\newline)
                                   return i)
                           -1))
           (actual-line-start (the fixnum (1+ line-start)))
           (line-end (or (position #\newline buf :start actual-line-start)
                         (fill-pointer buf)))
           (cur-len (fill-pointer buf))
           (next-line-start (if (< line-end cur-len)
                                (1+ line-end)
                                line-end)))
      (values actual-line-start line-end next-line-start))))

(defun count-leading-spaces (reader start end)
  (loop for i from start below end
        while (char= (elt reader i) #\space)
        count 1))

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
                           (reduce 'min
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

(defun parse-bullet (reader line-start line-end expected-indent)
  (unless (and (>= (- line-end line-start) expected-indent)
               (= (count-leading-spaces reader line-start line-end) expected-indent))
    (return-from parse-bullet (values nil nil 0)))
  (let ((bullet-start (+ line-start expected-indent)))
    (cond
      ((and (<= (+ bullet-start 2) line-end)
            (char= (elt reader bullet-start) #\-)
            (char= (elt reader (1+ bullet-start)) #\space))
       (values t "-" 2))
      ((and (< bullet-start line-end)
            (alphanumericp (elt reader bullet-start)))
       (let ((marker-dot-pos
               (position #\.
                         reader
                         :start (1+ bullet-start)
                         :end line-end)))
         (when (and marker-dot-pos
                    (loop for i from bullet-start below marker-dot-pos
                          always (alphanumericp (elt reader i))))
           (let* ((has-space-after
                    (and (< (1+ marker-dot-pos) line-end)
                         (char= (elt reader (1+ marker-dot-pos)) #\space)))
                  (bullet-struct-len
                    (+ (- (1+ marker-dot-pos) bullet-start)
                       (if has-space-after 1 0))))
             (values t
                     (subseq reader bullet-start (1+ marker-dot-pos))
                     bullet-struct-len)))))
      (t (values nil nil 0)))))

(defun parse-single-list-item (ctx reader item-start-pos item-indent inline-rules)
  "parse a single list item. returns (values item-match next-pos)."
  (multiple-value-bind (line-start line-end next-line-start)
      (get-line-bounds reader item-start-pos)
    (multiple-value-bind (is-bullet marker bullet-len)
        (parse-bullet reader line-start line-end item-indent)
      (unless is-bullet
        (return-from parse-single-list-item (values nil item-start-pos)))
      (let* ((bullet-begin (+ line-start item-indent))
             (bullet-end (+ bullet-begin (length marker)))
             (first-line-content-begin (+ bullet-begin bullet-len))
             (item-match (cltpt/combinator:make-match
                          :id 'list-item
                          :ctx ctx
                          :parent (cltpt/combinator:context-parent-match ctx)
                          :begin (- item-start-pos (cltpt/combinator:context-parent-begin ctx))
                          :props (list :indent item-indent)))
             (item-ctx (cltpt/combinator:context-copy ctx item-match))
             (item-children)
             (content-segments)
             (current-scan-pos next-line-start)
             (end-of-content-text line-end))
        (let ((bullet-match (cltpt/combinator:make-match
                             :id 'list-item-bullet
                             :ctx item-ctx
                             :parent (cltpt/combinator:context-parent-match item-ctx)
                             :begin (- bullet-begin (cltpt/combinator:context-parent-begin item-ctx))
                             :end (- bullet-end (cltpt/combinator:context-parent-begin item-ctx)))))
          (push bullet-match item-children))
        ;; collect content segments from first line
        (when (< first-line-content-begin line-end)
          (push (cons first-line-content-begin line-end) content-segments))
        ;; scan continuation lines
        (loop
          (when (cltpt/reader:is-after-eof reader current-scan-pos)
            (return))
          (multiple-value-bind (next-l-start next-l-end next-l-next)
              (get-line-bounds reader current-scan-pos)
            (let ((next-indent (count-leading-spaces reader next-l-start next-l-end)))
              (if (and (> next-indent item-indent)
                       (not (nth-value 0 (parse-bullet reader next-l-start next-l-end next-indent))))
                  (progn
                    (let ((extra-start (+ next-l-start next-indent)))
                      (when (< extra-start next-l-end)
                        (push (cons extra-start next-l-end) content-segments)))
                    (setf current-scan-pos next-l-next)
                    (setf end-of-content-text next-l-end))
                  (return)))))
        (setf content-segments (nreverse content-segments))
        (let* ((content-node-begin (if content-segments
                                       (caar content-segments)
                                       first-line-content-begin))
               (pos-after-children current-scan-pos)
               (end-of-children 0)
               (content-children))
          ;; check for nested list
          (unless (cltpt/reader:is-after-eof reader current-scan-pos)
            (multiple-value-bind (child-list-match pos-after-child)
                (org-list-matcher ctx reader current-scan-pos inline-rules (1+ item-indent))
              (when child-list-match
                (let ((child-abs-begin (+ (cltpt/combinator:match-begin child-list-match)
                                          (cltpt/combinator:context-parent-begin ctx)))
                      (child-abs-end (+ (cltpt/combinator:match-end child-list-match)
                                        (cltpt/combinator:context-parent-begin ctx))))
                  ;; store for now, will be adjusted when content-node is created.
                  ;; TODO: do this better, we should be setting those to relative positions
                  ;; initially rather than adjusting them later.
                  (setf (cltpt/combinator:match-begin child-list-match)
                        (- child-abs-begin content-node-begin))
                  (setf (cltpt/combinator:match-end child-list-match)
                        (- child-abs-end content-node-begin)))
                (push child-list-match content-children)
                (setf pos-after-children pos-after-child)
                (setf end-of-children (+ (cltpt/combinator:match-begin child-list-match)
                                         content-node-begin
                                         (- (cltpt/combinator:match-end child-list-match)
                                            (cltpt/combinator:match-begin child-list-match)))))))
          ;; scan inline rules in content segments
          (when inline-rules
            (dolist (segment content-segments)
              (let ((matches-in-segment (cltpt/combinator:scan-all-rules
                                         ctx
                                         reader
                                         inline-rules
                                         (car segment)
                                         (cdr segment))))
                (when matches-in-segment
                  (dolist (m matches-in-segment)
                    ;; convert from ctx-relative to content-node-relative
                    (let ((abs-begin (+ (cltpt/combinator:match-begin m)
                                        (cltpt/combinator:context-parent-begin ctx)))
                          (abs-end (+ (cltpt/combinator:match-end m)
                                      (cltpt/combinator:context-parent-begin ctx))))
                      (setf (cltpt/combinator:match-begin m) (- abs-begin content-node-begin))
                      (setf (cltpt/combinator:match-end m) (- abs-end content-node-begin))))
                  (setf content-children (nconc content-children matches-in-segment))))))
          (let* ((final-match-end (max end-of-content-text end-of-children))
                 (final-next-pos (max current-scan-pos pos-after-children))
                 (content-match (cltpt/combinator:make-match
                                 :id 'list-item-content
                                 :ctx item-ctx
                                 :parent (cltpt/combinator:context-parent-match item-ctx)
                                 :begin (- content-node-begin (cltpt/combinator:context-parent-begin item-ctx))
                                 :end (- final-match-end (cltpt/combinator:context-parent-begin item-ctx))
                                 :children (sort content-children '< :key 'cltpt/combinator:match-begin))))
            ;; set parent pointers for content children
            ;; TODO: make sure the matches are constructed with parent pointers in the first place. this is "incorrect" behavior because the children lose the context of the parent.
            (cltpt/combinator:match-set-children-parent content-match)
            (push content-match item-children)
            ;; finalize item match
            (setf (cltpt/combinator:match-end item-match)
                  (- final-match-end (cltpt/combinator:context-parent-begin ctx)))
            (setf (cltpt/combinator:match-children item-match) (nreverse item-children))
            (cltpt/combinator:match-set-children-parent item-match)
            (values item-match final-next-pos)))))))

(defun parse-list-items-at-indent (ctx reader initial-pos expected-indent inline-rules)
  (let ((item-nodes)
        (current-pos initial-pos)
        (last-successful-pos initial-pos))
    (loop
      (when (cltpt/reader:is-after-eof reader current-pos)
        (return))
      (multiple-value-bind (ls le) (get-line-bounds reader current-pos)
        (unless (= current-pos ls)
          (return))
        (when (cltpt/reader:is-after-eof reader ls)
          (return))
        (let ((indent (count-leading-spaces reader ls le)))
          (unless (= indent expected-indent)
            (return))
          (if (nth-value 0 (parse-bullet reader ls le indent))
              (multiple-value-bind (item-cons-cell new-item-pos)
                  (parse-single-list-item ctx reader current-pos expected-indent inline-rules)
                (if (and item-cons-cell (> new-item-pos current-pos))
                    (progn
                      (push item-cons-cell item-nodes)
                      (setf current-pos new-item-pos)
                      (setf last-successful-pos new-item-pos))
                    (return)))
              (return)))))
    (values (nreverse item-nodes) last-successful-pos)))

(defun org-list-matcher (ctx reader pos &optional inline-rules (minimum-indent 0))
  "parse an org-mode list starting at pos. returns (values list-match next-pos)."
  (multiple-value-bind (ls le) (get-line-bounds reader pos)
    (unless (= pos ls)
      (return-from org-list-matcher (values nil pos)))
    (let ((indent (count-leading-spaces reader ls le)))
      (unless (and (>= indent minimum-indent)
                   (nth-value 0 (parse-bullet reader ls le indent)))
        (return-from org-list-matcher (values nil pos)))
      (let* ((list-match (cltpt/combinator:make-match
                          :id 'org-list
                          :ctx ctx
                          :parent (cltpt/combinator:context-parent-match ctx)
                          :begin (- pos (cltpt/combinator:context-parent-begin ctx))
                          :props (list :indent indent)))
             (list-ctx (cltpt/combinator:context-copy ctx list-match)))
        (multiple-value-bind (nodes final-pos)
            (parse-list-items-at-indent list-ctx reader pos indent inline-rules)
          (if nodes
              (progn
                ;; final-pos points after the trailing newline for next parsing,
                ;; but the list match should end before it.
                (let ((list-end (- final-pos (cltpt/combinator:context-parent-begin ctx))))
                  ;; if the character before final-pos is a newline, don't include it
                  (when (and (> final-pos 0)
                             (< (1- final-pos) (cltpt/reader:reader-buffer-fill reader))
                             (char= (elt reader (1- final-pos)) #\newline))
                    (decf list-end))
                  (setf (cltpt/combinator:match-end list-match) list-end))
                (setf (cltpt/combinator:match-children list-match) nodes)
                (cltpt/combinator:match-set-children-parent list-match)
                (values list-match final-pos))
              (values nil pos)))))))

(defun list-match-to-nested-list (str list-match)
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
                                (cltpt/combinator:match-text bullet-node str)
                                "-"))
               (item-data (list :bullet bullet-text
                                :content ""
                                :children nil)))
          (when content-node
            (let* ((content-begin (cltpt/combinator:match-begin content-node))
                   (sub-list-node (find-direct-child-by-id content-node 'org-list))
                   (text-end (if sub-list-node
                                 (cltpt/combinator:match-begin sub-list-node)
                                 (cltpt/combinator:match-end content-node))))
              (setf (getf item-data :content)
                    (get-match-clean-content str content-begin text-end))
              (when sub-list-node
                (setf (getf item-data :children) (list-match-to-nested-list str sub-list-node)))))
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

(defun reformat-list (str parse-tree)
  "normalizes indentation and spacing.
returns the string representation of the list structure (no trailing newline)."
  (let ((data (list-match-to-nested-list str parse-tree))
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

(defun get-list-item-text (str item-node)
  (let ((content-node (find-direct-child-by-id item-node 'list-item-content)))
    (if content-node
        (let* ((begin (cltpt/combinator:match-begin content-node))
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

(defun get-list-type (str list-node)
  (let* ((children (cltpt/combinator:match-children list-node))
         (first-item (when children
                       (first children)))
         (bullet-node (when first-item
                        (find-direct-child-by-id first-item 'list-item-bullet)))
         (marker (when bullet-node
                   (cltpt/combinator:match-text bullet-node str))))
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
