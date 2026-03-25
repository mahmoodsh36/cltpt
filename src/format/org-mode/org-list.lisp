(in-package :cltpt/org-mode)

;; this version is much faster than the one above.
(defun get-line-bounds (reader pos)
  "returns (values line-start, line-end) for the line at pos."
  (if (cltpt/reader:is-after-eof reader pos)
      (let ((len (cltpt/reader:reader-buffer-fill reader)))
        (values len len))
      (let* ((line-start (or (loop for i fixnum from (1- pos) downto 0
                                   when (char= (cltpt/reader:reader-char reader i) #\newline)
                                     return i)
                             -1))
             (actual-line-start (the fixnum (1+ line-start)))
             (line-end (or (cltpt/reader:reader-position
                            reader
                            #\newline
                            actual-line-start
                            (cltpt/reader:reader-buffer-fill reader))
                           (cltpt/reader:reader-buffer-fill reader))))
        (values actual-line-start line-end))))

(defun count-leading-spaces (reader start end)
  (loop for i fixnum from start below end
        while (char= (cltpt/reader:reader-char reader i) #\space)
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
                                          (loop for i from 0 below (length l)
                                                while (char= (char l i) #\space)
                                                count 1)))
                           0)))
                (with-output-to-string (out)
                  (write-string first-line out)
                  (dolist (l other-lines)
                    (write-char #\newline out)
                    (unless (string= "" (string-trim '(#\space #\tab) l))
                      (write-string (subseq l (min (length l) min-indent))
                                    out))))))))))

(defun get-match-clean-content (str begin end)
  (if (and (< begin end)
           (<= end (length str)))
      (clean-list-content (subseq str begin end))
      ""))

(defvar *bullet-types*
  nil
  "registered bullet types, tried in order by `parse-bullet'.
each entry is a plist with :name, :rule, :ordered-p, and :first-bullet.")

(defun register-bullet-type (&key name rule ordered-p first-bullet sequence-func on-char)
  (setf *bullet-types*
        (append (remove name
                        *bullet-types*
                        :key (lambda (bt) (getf bt :name)))
                (list (list :name name
                            :rule rule
                            :ordered-p ordered-p
                            :first-bullet first-bullet
                            :sequence-func sequence-func
                            :on-char on-char)))))

(defvar *roman-values*
  '((1000 "m") (900 "cm") (500 "d") (400 "cd")
    (100 "c") (90 "xc") (50 "l") (40 "xl")
    (10 "x") (9 "ix") (5 "v") (4 "iv") (1 "i")))

(defun roman-char-value (ch)
  "return the integer value of a single roman numeral character, or nil."
  (loop for (val str) in *roman-values*
        when (and (= (length str) 1) (char-equal ch (char str 0)))
          return val))

(defun roman-to-int (str)
  "parse a roman numeral string. returns the integer value or nil."
  (when (zerop (length str))
    (return-from roman-to-int nil))
  (let ((total 0)
        (prev 0))
    (loop for i from (1- (length str)) downto 0
          for val = (roman-char-value (char str i))
          do (unless val
               (return-from roman-to-int nil))
             (if (< val prev)
                 (decf total val)
                 (incf total val))
             (setf prev val))
    total))

(defun int-to-roman (n &optional uppercase)
  "convert integer to a roman numeral string."
  (let ((result (with-output-to-string (s)
                  (dolist (pair *roman-values*)
                    (loop while (>= n (first pair))
                          do (write-string (second pair) s)
                             (decf n (first pair)))))))
    (if uppercase
        (string-upcase result)
        result)))

(defun make-numeric-dot-bullet (lst idx)
  (let ((marker (getf (first (getf lst :children)) :marker)))
    (format nil "~D." (+ (parse-integer (or marker "1")) idx))))

(defun make-numeric-paren-bullet (lst idx)
  (let ((marker (getf (first (getf lst :children)) :marker)))
    (format nil "~D)" (+ (parse-integer (or marker "1")) idx))))

(defun make-roman-dot-bullet (lst idx)
  (let* ((m (or (getf (first (getf lst :children)) :marker) "i"))
         (start (or (roman-to-int m) 1))
         (uppercase (upper-case-p (if (> (length m) 0) (char m 0) #\i))))
    (format nil "~A." (int-to-roman (+ start idx) uppercase))))

(defun make-roman-paren-bullet (lst idx)
  (let* ((m (or (getf (first (getf lst :children)) :marker) "i"))
         (start (or (roman-to-int m) 1))
         (uppercase (upper-case-p (if (> (length m) 0) (char m 0) #\i))))
    (format nil "~A)" (int-to-roman (+ start idx) uppercase))))

(defun make-alpha-dot-bullet (lst idx)
  (let* ((m (or (getf (first (getf lst :children)) :marker) "a"))
         (start (if (> (length m) 0) (char-code (char m 0)) 97)))
    (format nil "~C." (code-char (+ start idx)))))

(defun make-alpha-paren-bullet (lst idx)
  (let* ((m (or (getf (first (getf lst :children)) :marker) "a"))
         (start (if (> (length m) 0) (char-code (char m 0)) 97)))
    (format nil "~C)" (code-char (+ start idx)))))

(register-bullet-type
  :name :dash
  :on-char #\-
  :rule '(cltpt/combinator:consec (cltpt/combinator:literal "-") (cltpt/combinator:literal " "))
  :first-bullet "-")

(register-bullet-type
  :name :plus
  :on-char #\+
  :rule '(cltpt/combinator:consec (cltpt/combinator:literal "+") (cltpt/combinator:literal " "))
  :first-bullet "+")

(register-bullet-type
  :name :numeric-dot
  :on-char #'digit-char-p
  :rule '(cltpt/combinator:consec
          (:id marker :pattern (cltpt/combinator:atleast-one-discard (cltpt/combinator:digit-p)))
          (:id suffix :pattern (cltpt/combinator:literal "."))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "1."
  :sequence-func #'make-numeric-dot-bullet)

(register-bullet-type
  :name :numeric-paren
  :on-char #'digit-char-p
  :rule '(cltpt/combinator:consec
          (:id marker :pattern (cltpt/combinator:atleast-one-discard (cltpt/combinator:digit-p)))
          (:id suffix :pattern (cltpt/combinator:literal ")"))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "1)"
  :sequence-func #'make-numeric-paren-bullet)

;; order matters. types are tried in order by parse-bullet.
;; roman has to be registered before alphabetic.
(register-bullet-type
  :name :roman-dot
  :on-char #'alpha-char-p
  :rule '(cltpt/combinator:consec
          (:id marker
           :pattern (cltpt/combinator:atleast-one-discard (cltpt/combinator:roman-char-p)))
          (:id suffix :pattern (cltpt/combinator:literal "."))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "i."
  :sequence-func #'make-roman-dot-bullet)

(register-bullet-type
  :name :roman-paren
  :on-char #'alpha-char-p
  :rule '(cltpt/combinator:consec
          (:id marker
           :pattern (cltpt/combinator:atleast-one-discard (cltpt/combinator:roman-char-p)))
          (:id suffix :pattern (cltpt/combinator:literal ")"))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "i)"
  :sequence-func #'make-roman-paren-bullet)

(register-bullet-type
  :name :alpha-dot
  :on-char #'alpha-char-p
  :rule '(cltpt/combinator:consec
          (:id marker :pattern (cltpt/combinator:eng-char-p))
          (:id suffix :pattern (cltpt/combinator:literal "."))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "a."
  :sequence-func #'make-alpha-dot-bullet)

(register-bullet-type
  :name :alpha-paren
  :on-char #'alpha-char-p
  :rule '(cltpt/combinator:consec
          (:id marker :pattern (cltpt/combinator:eng-char-p))
          (:id suffix :pattern (cltpt/combinator:literal ")"))
          (cltpt/combinator:consec-atleast-one (cltpt/combinator:literal " ")))
  :ordered-p t
  :first-bullet "a)"
  :sequence-func #'make-alpha-paren-bullet)

(defun parse-bullet (reader line-start line-end expected-indent)
  "try each registered bullet type at (line-start + expected-indent). return match or nil."
  (unless (and (>= (- line-end line-start) expected-indent)
               (= (count-leading-spaces reader line-start line-end) expected-indent))
    (return-from parse-bullet nil))
  (let* ((bullet-start (+ line-start expected-indent))
         (ch (cltpt/reader:reader-char reader bullet-start)))
    (dolist (bt *bullet-types*)
      (let ((on-char (getf bt :on-char)))
        (when (or (null on-char)
                  (if (characterp on-char)
                      (char= ch on-char)
                      (funcall on-char ch)))
          (let ((match (cltpt/combinator:apply-rule nil (getf bt :rule) reader bullet-start)))
            (when match
              (let* ((bullet-len (- (cltpt/combinator:match-end match)
                                    (cltpt/combinator:match-begin match)))
                     (end-pos (+ bullet-start bullet-len)))
                (when (<= end-pos line-end)
                  (setf (cltpt/combinator:match-props match)
                        (list :type bt))
                  (return match))))))))))

(defun parse-single-list-item (ctx reader item-start-pos item-indent inline-rules)
  "parse a single list item. returns (values item-match next-pos)."
  (multiple-value-bind (line-start line-end)
      (get-line-bounds reader item-start-pos)
    (let ((bullet-match (parse-bullet reader line-start line-end item-indent)))
      (unless bullet-match
        (return-from parse-single-list-item (values nil item-start-pos)))
      (let* ((bprops (cltpt/combinator:match-props bullet-match))
             (bt (getf bprops :type))
             (bullet-len (- (cltpt/combinator:match-end bullet-match)
                            (cltpt/combinator:match-begin bullet-match)))
             (marker-match (cltpt/combinator:find-submatch bullet-match 'marker))
             (marker-text (when marker-match
                            (cltpt/combinator:match-text marker-match reader)))
             (suffix-match (cltpt/combinator:find-submatch bullet-match 'suffix))
             (suffix-text (when suffix-match
                            (cltpt/combinator:match-text suffix-match reader)))
             (bullet-begin (+ line-start item-indent))
             (bullet-end (+ bullet-begin (max 0 (1- bullet-len))))
             (first-line-content-begin (+ bullet-begin bullet-len))
             (item-match (cltpt/combinator:make-match
                          :id 'list-item
                          :ctx ctx
                          :parent (cltpt/combinator:context-parent-match ctx)
                          :begin (- item-start-pos (cltpt/combinator:context-parent-begin ctx))
                          :props (list :indent item-indent
                                       :marker marker-text
                                       :suffix suffix-text)))
             (item-ctx (cltpt/combinator:context-copy ctx item-match))
             (item-children)
             (content-segments)
             (current-scan-pos (if (< line-end (cltpt/reader:reader-buffer-fill reader))
                                   (1+ line-end)
                                   line-end))
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
          (multiple-value-bind (next-l-start next-l-end)
              (get-line-bounds reader current-scan-pos)
            (let ((next-indent (count-leading-spaces reader next-l-start next-l-end)))
              (if (and (> next-indent item-indent)
                       (not (parse-bullet reader next-l-start next-l-end next-indent)))
                  (progn
                    (let ((extra-start (+ next-l-start next-indent)))
                      (when (< extra-start next-l-end)
                        (push (cons extra-start next-l-end) content-segments)))
                    (setf current-scan-pos (if (< next-l-end (cltpt/reader:reader-buffer-fill reader))
                                               (1+ next-l-end)
                                               next-l-end))
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
          (if (parse-bullet reader ls le indent)
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
  ;; use a heuristic: if not at line-start, return immediately without an expensive scan
  (unless (or (= pos 0)
              (and (> pos 0)
                   (char= (cltpt/reader:reader-char reader (1- pos)) #\newline)))
    (return-from org-list-matcher (values nil pos)))
  (multiple-value-bind (ls le) (get-line-bounds reader pos)
    (let ((indent (count-leading-spaces reader ls le)))
        (let ((first-bullet-match (parse-bullet reader ls le indent)))
          (unless (and (>= indent minimum-indent) first-bullet-match)
            (return-from org-list-matcher (values nil pos)))
          (let* ((fbprops (cltpt/combinator:match-props first-bullet-match))
                 (first-bt (getf fbprops :type))
                 (list-match (cltpt/combinator:make-match
                               :id 'org-list
                               :ctx ctx
                               :parent (cltpt/combinator:context-parent-match ctx)
                               :begin (- pos (cltpt/combinator:context-parent-begin ctx))
                               :props (list :indent indent
                                            :type first-bt)))
                 (list-ctx (cltpt/combinator:context-copy ctx list-match)))
          (multiple-value-bind (nodes final-pos)
              (parse-list-items-at-indent list-ctx reader pos indent inline-rules)
            (if nodes
                (progn
                  (let ((list-end (- final-pos (cltpt/combinator:context-parent-begin ctx))))
                    (when (and (> final-pos 0)
                               (< (1- final-pos) (cltpt/reader:reader-buffer-fill reader))
                               (char= (cltpt/reader:reader-char reader (1- final-pos))
                                      #\newline))
                      (decf list-end))
                    (setf (cltpt/combinator:match-end list-match) list-end))
                  (setf (cltpt/combinator:match-children list-match) nodes)
                  (cltpt/combinator:match-set-children-parent list-match)
                  (values list-match final-pos))
                (values nil pos))))))))

(defun list-match-to-list (str list-match)
  "converts an org-list match tree into a nested list."
  (unless (and list-match (eq (cltpt/combinator:match-id list-match) 'org-list))
    (return-from list-match-to-list nil))
  (let ((items (cltpt/combinator:match-children list-match))
        (list-type (getf (cltpt/combinator:match-props list-match) :type))
        (result-list))
    (dolist (item items)
      (when (eq (cltpt/combinator:match-id item) 'list-item)
        (let* ((bullet-node
                 (cltpt/combinator/match:find-direct-match-child-by-id
                  item
                  'list-item-bullet))
               (content-node
                 (cltpt/combinator/match:find-direct-match-child-by-id
                  item
                  'list-item-content))
               (bullet-text (if bullet-node
                                (cltpt/combinator:match-text bullet-node str)
                                "-"))
               (props (cltpt/combinator:match-props item))
               (item-data (list* :bullet bullet-text
                                 :content ""
                                 :children nil
                                 props)))
          (when content-node
            (let* ((content-begin (cltpt/combinator:match-begin-absolute content-node))
                   (sub-list-node (cltpt/combinator/match:find-direct-match-child-by-id
                                   content-node
                                   'org-list))
                   (text-end (if sub-list-node
                                 (cltpt/combinator:match-begin-absolute sub-list-node)
                                 (cltpt/combinator:match-end-absolute content-node))))
              (setf (getf item-data :content)
                    (get-match-clean-content str content-begin text-end))
              (when sub-list-node
                (setf (getf item-data :children) (list-match-to-list str sub-list-node)))))
          (push item-data result-list))))
    (list :type list-type :children (nreverse result-list))))

(defun list-to-list-string (lst &optional (indent-level 0))
  "converts a nested list to an org-list string."
  (with-output-to-string (s)
    (loop for (item . rest) on (getf lst :children)
          do (let ((bullet (getf item :bullet))
                   (content (getf item :content))
                   (children (getf item :children))
                   (indent-str (make-string indent-level :initial-element #\space))
                   (own-content-offset (1+ (length (getf item :bullet)))))
               ;; write bullet and indent
               (format s "~a~a " indent-str bullet)
               ;; write content
               (let ((lines (split-string-lines content))
                     (own-content-indent
                       (make-string own-content-offset :initial-element #\space)))
                 (when lines
                   (write-string (first lines) s)
                   (dolist (line (cdr lines))
                     (format s "~%~a~a~a" indent-str own-content-indent line))))
               ;; write children (if any)
               (when children
                 ;; need newline before entering sub-list
                 (write-char #\newline s)
                 (write-string
                  (list-to-list-string children (+ indent-level own-content-offset))
                  s))
               ;; write separator (only if there is another item following)
               (when rest
                 (write-char #\newline s))))))

(defun reformat-list (str parse-tree)
  "normalizes indentation and spacing."
  (let ((data (list-match-to-list str parse-tree))
        (indent (getf (cltpt/combinator:match-props parse-tree) :indent 0)))
    (list-to-list-string data indent)))

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
                   (let* ((content-node (cltpt/combinator/match:find-direct-match-child-by-id
                                         child
                                         'list-item-content))
                          (sub-list-node
                            (and content-node
                                 (cltpt/combinator/match:find-direct-match-child-by-id
                                  content-node
                                  'org-list))))
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
      (let* ((content-node (cltpt/combinator/match:find-direct-match-child-by-id
                            current-item
                            'list-item-content)))
        (setf current-list
              (and content-node
                   (cltpt/combinator/match:find-direct-match-child-by-id
                    content-node
                    'org-list)))))
    current-item))

(defun get-list-item-text (str item-node)
  (let ((content-node (cltpt/combinator/match:find-direct-match-child-by-id item-node 'list-item-content)))
    (if content-node
        (let* ((begin (cltpt/combinator:match-begin content-node))
               (sub-list (cltpt/combinator/match:find-direct-match-child-by-id
                          content-node
                          'org-list))
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
        (let* ((content (cltpt/combinator/match:find-direct-match-child-by-id
                         child
                         'list-item-content))
               (sub-list (when content
                           (cltpt/combinator/match:find-direct-match-child-by-id
                            content
                            'org-list))))
          (when sub-list
            (setf depth (max depth (1+ (get-list-depth sub-list))))))))
    depth))

(defun get-list-type (str list-node)
  (if (getf (getf (cltpt/combinator:match-props list-node) :type) :ordered-p)
      :ol
      :ul))

(defun bullet-split (bullet-string)
  "from a BULLET-STRING, parse and return (values marker suffix)."
  (let* ((string-with-space (concatenate 'string bullet-string " "))
         (reader (cltpt/reader:reader-from-string string-with-space))
         (match (parse-bullet reader 0 (length string-with-space) 0)))
    (when match
      (let* ((marker-match (cltpt/combinator:find-submatch match 'marker))
             (marker-text (when marker-match
                            (cltpt/combinator:match-text marker-match reader)))
             (suffix-match (cltpt/combinator:find-submatch match 'suffix))
             (suffix-text (when suffix-match
                            (cltpt/combinator:match-text suffix-match reader))))
        (values marker-text suffix-text)))))

(defun cycle-next-bullet (bt)
  "return the next bullet type entry in the cycle after BT."
  (let ((pos (when bt
               (position
                (getf bt :name)
                *bullet-types*
                :key (lambda (entry)
                       (getf entry :name))))))
    (if pos
        (nth (mod (1+ pos) (length *bullet-types*)) *bullet-types*)
        (first *bullet-types*))))

(defun bullet-at-index (lst idx)
  "return the bullet string at IDX for a list plist."
  (let* ((bt (getf lst :type))
         (func (when bt (getf bt :sequence-func))))
    (if func
        (funcall func lst idx)
        (if bt
            (getf bt :first-bullet "-")
            (getf (first (getf lst :children)) :bullet "-")))))

(defun renumber-list-items (lst &optional (start-idx 0))
  "renumber :bullet of items in list LST from START-IDX onward.
resets the first item's marker to the type's base before generating bullets."
  (let* ((items (getf lst :children))
         (bt (getf lst :type))
         (first-item (first items)))
    (when first-item
      (when (and bt (getf bt :first-bullet))
        (multiple-value-bind (marker suffix) (bullet-split (getf bt :first-bullet))
          (setf (getf first-item :marker) marker)))
      (loop for item in (nthcdr start-idx items)
            for i from start-idx
            do (setf (getf item :bullet) (bullet-at-index lst i))))))

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