(in-package :cltpt/org-mode)

(defun get-line-info (str pos)
  (when (>= pos (length str))
    (return-from get-line-info (values "" (length str) (length str) t)))
  (let* ((line-start (or (position #\newline str :end pos :from-end t) -1))
         (actual-line-start (1+ line-start))
         (line-end (or (position #\newline str :start actual-line-start)
                       (length str)))
         (next-pos (if (< line-end (length str))
                       (1+ line-end) line-end)))
    (values (subseq str actual-line-start line-end)
            actual-line-start
            next-pos
            (>= line-end (length str)))))

(defun count-leading-spaces (line-text)
  (loop for char across line-text
        while (char= char #\space)
        count 1))

(defun parse-bullet-from-line-text (line-text expected-indent)
  (cond
    ((and (>= (length line-text) (+ expected-indent 2))
          (= (count-leading-spaces (subseq line-text 0 expected-indent))
             expected-indent)
          (char= (char line-text expected-indent) #\-)
          (char= (char line-text (+ expected-indent 1)) #\space))
     (values t "-" (subseq line-text (+ expected-indent 2)) (+ expected-indent 2)))
    ((and (>= (length line-text) expected-indent)
          (= (count-leading-spaces (subseq line-text 0 expected-indent))
             expected-indent)
          (< expected-indent (length line-text))
          (let ((char-after-indent (char line-text expected-indent)))
            (or (digit-char-p char-after-indent)
                (alpha-char-p char-after-indent))))
     (let* ((text-after-indent (subseq line-text expected-indent))
            (i 0)
            (len-text-after-indent (length text-after-indent)))
       (loop while (and (< i len-text-after-indent)
                        (alphanumericp (char text-after-indent i)))
             do (incf i))
       (if (and (> i 0)
                (< i len-text-after-indent)
                (char= (char text-after-indent i) #\.))
           (let* ((marker-end-idx-in-text-after-indent (1+ i))
                  (marker (subseq text-after-indent 0 marker-end-idx-in-text-after-indent))
                  (text-start-idx-in-text-after-indent marker-end-idx-in-text-after-indent)
                  (bullet-struct-len-after-indent marker-end-idx-in-text-after-indent))
             (when (and (< text-start-idx-in-text-after-indent len-text-after-indent)
                        (char= (char text-after-indent
                                     text-start-idx-in-text-after-indent)
                               #\space))
               (incf text-start-idx-in-text-after-indent)
               (incf bullet-struct-len-after-indent))
             (values t
                     marker
                     (subseq text-after-indent text-start-idx-in-text-after-indent)
                     (+ expected-indent bullet-struct-len-after-indent)))
           (values nil nil nil nil))))
    (t (values nil nil nil nil))))

(defun adjust-match-offsets (matches offset)
  (labels ((adjust-single-match (match-item)
             (when match-item
               (let* ((parent-info (car match-item))
                      (children (cdr match-item))
                      (new-parent-info
                        (list* :begin (+ (getf parent-info :begin 0) offset)
                               :end (+ (getf parent-info :end 0) offset)
                               (loop for (key val) on parent-info by #'cddr
                                     unless (or (eq key :begin) (eq key :end))
                                       append (list key val)))))
                 (cons new-parent-info
                       (mapcar #'adjust-single-match children))))))
    (mapcar #'adjust-single-match matches)))

(defun org-list-matcher (str pos &optional inline-rules)
  (multiple-value-bind (first-line-text first-line-start-offset _next-ln-pos _is-last)
      (get-line-info str pos)
    (unless (= pos first-line-start-offset)
      (return-from org-list-matcher (values nil pos)))
    (when (or (null first-line-text) (>= pos (length str)))
      (return-from org-list-matcher (values nil pos)))
    (let ((initial-indent (count-leading-spaces first-line-text)))
      (multiple-value-bind (is-bullet) (parse-bullet-from-line-text first-line-text initial-indent)
        (unless is-bullet (return-from org-list-matcher (values nil pos)))
        (multiple-value-bind (top-level-item-nodes final-pos-after-list)
            (parse-list-items-at-indent str first-line-start-offset initial-indent inline-rules)
          (if top-level-item-nodes
              (let ((list-begin-offset first-line-start-offset)
                    (list-end-offset final-pos-after-list))
                (values
                 (cons (list :id 'org-list
                             :begin list-begin-offset
                             :end list-end-offset
                             :match (subseq str list-begin-offset list-end-offset)
                             :indent initial-indent)
                       top-level-item-nodes)
                 final-pos-after-list))
              (values nil pos)))))))

(defun parse-single-list-item (str item-line-start-offset current-item-indent inline-rules initial-bullet-marker initial-text-on-bullet-line)
  (let* ((children-of-list-item)
         (bullet-node)
         (bullet-string-actual initial-bullet-marker)
         (is-bullet-p)
         (parsed-marker)
         (text-after-marker)
         (bullet-struct-char-length)
         (current-line-text (nth-value 0 (get-line-info str item-line-start-offset)))
         (text-content-absolute-start-offset item-line-start-offset))
    (multiple-value-bind (p-is-bullet p-marker p-text-after p-bullet-len)
        (parse-bullet-from-line-text current-line-text current-item-indent)
      (setf is-bullet-p p-is-bullet)
      (setf parsed-marker p-marker)
      (setf text-after-marker p-text-after)
      (setf bullet-struct-char-length p-bullet-len))
    (if bullet-struct-char-length
        (progn
          (setf text-content-absolute-start-offset
                (+ item-line-start-offset bullet-struct-char-length))
          (setf bullet-node
                (cons (list :id 'list-item-bullet
                            :begin (+ item-line-start-offset current-item-indent)
                            :end text-content-absolute-start-offset
                            :match bullet-string-actual)
                      nil))
          (push bullet-node children-of-list-item))
        (return-from parse-single-list-item (values nil item-line-start-offset)))
    (let* ((collected-text-lines (list initial-text-on-bullet-line))
           (pos-after-initial-bullet-line
             (nth-value 2 (get-line-info str item-line-start-offset)))
           (current-scan-pos pos-after-initial-bullet-line)
           (end-of-this-item-text-block pos-after-initial-bullet-line))
      (loop
        (when (>= current-scan-pos (length str))
          (setf end-of-this-item-text-block current-scan-pos)
          (return))
        (multiple-value-bind (next-line-text _nextlnst nxtlnparse _islast) (get-line-info str current-scan-pos)
          (let ((indent-on-next-line (count-leading-spaces next-line-text)))
            (cond ((<= indent-on-next-line current-item-indent)
                   (setf end-of-this-item-text-block current-scan-pos)
                   (return))
                  (t (multiple-value-bind (is-bullet-for-child) (parse-bullet-from-line-text next-line-text indent-on-next-line)
                       (if is-bullet-for-child
                           (progn
                             (setf end-of-this-item-text-block current-scan-pos)
                             (return))
                           (progn
                             (push (string-left-trim " " next-line-text)
                                   collected-text-lines)
                             (setf current-scan-pos nxtlnparse)
                             (setf end-of-this-item-text-block
                                   current-scan-pos)))))))))
      (let* ((reversed-collected-lines (nreverse collected-text-lines))
             (full-item-text-for-match
               (string-right-trim
                '(#\newline #\space #\tab)
                (format nil "~{~A~^~%~}" reversed-collected-lines)))
             (children-of-content-node)
             (pos-after-item-processing current-scan-pos))
        (when (and inline-rules (plusp (length full-item-text-for-match)))
          (let ((raw-inline-matches
                  (cltpt/combinator::scan-all-rules
                   full-item-text-for-match
                   inline-rules
                   0
                   (length full-item-text-for-match))))
            (setf children-of-content-node
                  (nconc children-of-content-node
                         (adjust-match-offsets raw-inline-matches
                                               text-content-absolute-start-offset)))))
        (when (< current-scan-pos (length str))
          (multiple-value-bind (line-at-children-start _l _nl _is) (get-line-info str current-scan-pos)
            (when line-at-children-start
              (let ((child-indent (count-leading-spaces line-at-children-start)))
                (when (> child-indent current-item-indent)
                  (multiple-value-bind (is-child-bullet) (parse-bullet-from-line-text line-at-children-start child-indent)
                    (when is-child-bullet
                      (multiple-value-bind (parsed-child-list-match new-pos-from-child-matcher)
                          (org-list-matcher str current-scan-pos inline-rules)
                        (when parsed-child-list-match
                          (setf children-of-content-node
                                (nconc children-of-content-node
                                       (list parsed-child-list-match)))
                          (when (numberp new-pos-from-child-matcher)
                            (setf pos-after-item-processing
                                  new-pos-from-child-matcher)))))))))))
        (let ((content-node-begin text-content-absolute-start-offset)
              (content-node-end end-of-this-item-text-block))
          (when (or (plusp (length full-item-text-for-match))l
                    children-of-content-node)
            (let ((content-node-parent-info
                    (list :id 'list-item-content
                          :begin content-node-begin
                          :end content-node-end
                          :match full-item-text-for-match)))
              (push (cons content-node-parent-info children-of-content-node)
                    children-of-list-item))))
        (let* ((item-node-begin item-line-start-offset)
               (item-node-end pos-after-item-processing)
               (item-parent-info
                 (list :id 'list-item
                       :indent current-item-indent
                       :begin item-node-begin
                       :end item-node-end
                       :match (subseq str item-node-begin item-node-end))))
          (values (cons item-parent-info (nreverse children-of-list-item))
                  item-node-end))))))

(defun parse-list-items-at-indent (str initial-pos expected-indent inline-rules)
  (let ((item-nodes)
        (current-pos initial-pos)
        (last-successful-item-end-pos initial-pos))
    (loop
      (unless (numberp current-pos)
        (return-from parse-list-items-at-indent
          (values (nreverse item-nodes) last-successful-item-end-pos)))
      (when (>= current-pos (length str))
        (return))
      (multiple-value-bind (line-text line-start _nextlnpos _islastln) (get-line-info str current-pos)
        (unless line-text (return))
        (setf current-pos line-start)
        (let ((indent-on-this-line (count-leading-spaces line-text)))
          (cond ((< indent-on-this-line expected-indent) (return))
                ((> indent-on-this-line expected-indent) (return))
                (t (multiple-value-bind (is-bullet bullet-marker text-on-bullet-line _bulletlen)
                       (parse-bullet-from-line-text line-text expected-indent)
                     (if is-bullet
                         (multiple-value-bind (item-cons-cell new-item-pos)
                             (parse-single-list-item
                              str
                              current-pos
                              expected-indent
                              inline-rules
                              bullet-marker
                              text-on-bullet-line)
                           (if (and item-cons-cell
                                    (numberp new-item-pos)
                                    (> new-item-pos current-pos))
                               (progn
                                 (push item-cons-cell item-nodes)
                                 (setf current-pos new-item-pos)
                                 (setf last-successful-item-end-pos new-item-pos))
                               (return)))
                         (return))))))))
    (values (nreverse item-nodes) last-successful-item-end-pos)))

(defun test-org-list-parsing ()
  (let ((text "- item one
  extra text for one
- we have \\(x=y\\)
  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
- item three"))
    (cltpt/combinator::parse
     text
     (list
      '(cltpt/org-mode::org-list-matcher
        ((:pattern (cltpt/combinator::literal "item") :id item-keyword)
         (:pattern (cltpt/combinator::literal "nested") :id nested-keyword)
         (:pattern (cltpt/combinator::word-matcher) :id generic-word)))))))