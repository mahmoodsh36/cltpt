(in-package :cltpt/org-mode)

(defun org-list-count-leading-spaces (line)
  (let ((count 0))
    (loop for ch across line
          while (char= ch #\space)
          do (incf count))
    count))

(defun org-list-parse-bullet-line (line expected-indent)
  "if LINE (after EXPECTED-INDENT spaces) begins with a valid bullet,
return three values: T, the bullet marker (a string), and the remaining text.
a valid bullet is either a dash followed by a space, or a sequence of digits/letters
followed by a dot (and optionally a following space)."
  (if (< (length line) expected-indent)
      (values nil nil nil)
      (if (/= (org-list-count-leading-spaces line) expected-indent)
          (values nil nil nil)
          (let ((trimmed (subseq line expected-indent)))
            (cond
              ;; dash bullet.
              ((and (>= (length trimmed) 2)
                    (char= (char trimmed 0) #\-)
                    (char= (char trimmed 1) #\space))
               (values t "-" (subseq trimmed 2)))
              ;; number or letter bullet (e.g., "1." or "a." or "IV.")
              ((and (plusp (length trimmed))
                    (or (digit-char-p (char trimmed 0))
                        (alpha-char-p (char trimmed 0))))
               (let ((i 0)
                     (only-digits t))
                 (loop while (and (< i (length trimmed))
                                  (let ((char (char trimmed i)))
                                    (cond
                                      ((digit-char-p char)
                                       t)
                                      ((alpha-char-p char)
                                       (setf only-digits nil)
                                       t)
                                      (t nil))))
                       do (incf i))
                 (if (and (> i 0)
                          (< i (length trimmed))
                          (char= (char trimmed i) #\.)
                          (or (<= i 3) only-digits))
                     (let* ((marker-end-idx (1+ i))
                            (marker (subseq trimmed 0 marker-end-idx))
                            (text-start-idx marker-end-idx))
                       (when (and (< text-start-idx (length trimmed))
                                  (char= (char trimmed text-start-idx) #\space))
                         (incf text-start-idx))
                       (values t marker (subseq trimmed text-start-idx)))
                     (values nil nil nil))))
              (t (values nil nil nil)))))))

(defun org-list-collect-extra-lines (lines current-indent)
  (if (or (null lines)
          (<= (org-list-count-leading-spaces (first lines)) current-indent))
      (values nil lines)
      (let* ((line (first lines))
             (line-indent (org-list-count-leading-spaces line)))
        (multiple-value-bind (is-bullet)
            (org-list-parse-bullet-line line line-indent)
          (if is-bullet
              (values nil lines)
              (multiple-value-bind (collected rem)
                  (org-list-collect-extra-lines (rest lines) current-indent)
                (values (cons (string-trim " " line) collected) rem)))))))

(defun org-list-parse-one-item (lines current-indent)
  "parses a single item and its direct children.
returns two values:
1. a cons cell: (item-data . list-of-child-nodes)
   where item-data is a plist like (:marker M :text T)
   and list-of-child-nodes is a list of nodes, each also (child-data . list-of-grandchild-nodes)
2. remaining lines after this item and its children."
  (multiple-value-bind (valid marker text-content)
      (org-list-parse-bullet-line (first lines) current-indent)
    (declare (ignore valid))
    (let ((remaining (rest lines)))
      (multiple-value-bind (extra-lines rem)
          (org-list-collect-extra-lines remaining current-indent)
        (setf remaining rem)
        (let ((children-nodes)) ;; this will be a list of (child-data . grandchild-nodes)
          (when (and remaining
                     (> (org-list-count-leading-spaces (first remaining)) current-indent))
            (let* ((child-first-line (first remaining))
                   (child-indent (org-list-count-leading-spaces child-first-line)))
              (multiple-value-bind (is-child-bullet)
                  (org-list-parse-bullet-line child-first-line child-indent)
                (when is-child-bullet
                  (multiple-value-bind (child-items-parsed rem2)
                      (org-list-parse-items remaining child-indent)
                    (setf children-nodes child-items-parsed)
                    (setf remaining rem2))))))
          (let* ((clean-text (string-trim " " text-content))
                 (combined-text (if extra-lines
                                    (concatenate 'string
                                                 clean-text
                                                 (string #\newline)
                                                 (str:join (string #\newline) extra-lines))
                                    clean-text))
                 (item-data (list :marker marker :text combined-text)))
            (values (cons item-data children-nodes) remaining)))))))

(defun org-list-parse-items (lines current-indent)
  "parses a list of items at the CURRENT-INDENT.
   returns two values:
   1. a list of nodes, where each node is (item-data . list-of-child-nodes)
   2. remaining lines."
  (if (null lines)
      (values nil lines) ;; no items, no remaining lines from this level
      (let ((first-line (first lines))
            (line-indent (org-list-count-leading-spaces (first lines))))
        (if (< line-indent current-indent)
            (values nil lines) ;; this line is for a parent or unrelated, return no items and all lines
            (multiple-value-bind (is-bullet-at-this-level)
                (org-list-parse-bullet-line first-line current-indent)
              (if (not is-bullet-at-this-level)
                  (values nil lines) ;; not a bullet at this level, return no items and all lines
                  (multiple-value-bind (current-item-node rem-lines-after-item) ;; current-item-node is (item-data . children-nodes)
                      (org-list-parse-one-item lines current-indent)
                    (multiple-value-bind (sibling-nodes final-rem-lines) ;; sibling-nodes is a list of (sibling-data . their-children)
                        (org-list-parse-items rem-lines-after-item current-indent)
                      (values (cons current-item-node sibling-nodes)
                              final-rem-lines)))))))))

(defun get-lines-with-metadata (text)
  "returns a list of (line-text start-offset line-index) for each line in TEXT.
offsets include newline characters for inter-line spacing."
  (let* ((all-raw-lines (str:split (string #\newline) text :omit-nulls nil))
         (num-all-lines (length all-raw-lines)))
    (loop with current-offset = 0
          for line-text in all-raw-lines
          for line-idx from 0
          collect (list line-text current-offset line-idx)
          do (incf current-offset (+ (length line-text)
                                     ;; add 1 for newline, unless it's the very last line of the text
                                     (if (< (1+ line-idx) num-all-lines) 1 0))))))

(defun find-list-start-at-target-offset (target-offset lines-metadata text-length)
  "attempts to find and validate a list start at TARGET-OFFSET.
LINES-METADATA is a list of (line-text line-start-char-offset line-idx).
TEXT-LENGTH is the total length of the original text.
returns (values effective-list-start-char-offset
                first-list-line-actual-start-char-offset
                initial-indent-for-parser
                lines-to-feed-parser ;; list of raw line strings
                success-p)"
  (let ((found-line-details))
    ;; find the line containing target-offset
    (setf found-line-details
          (find-if (lambda (ld)
                     (let ((l-text (first ld)) (l-start (second ld)))
                       (and (>= target-offset l-start)
                            (< target-offset (+ l-start (length l-text)
                                                ;; if target is at start of line, allow it even if line is empty
                                                (if (and (= target-offset l-start) (string= l-text "")) 1 0))))))
                   lines-metadata))
    ;; check for target-offset at the very end of text,
    ;; potentially an empty line if the text ends with a newline.
    (when (and (null found-line-details)
               (= target-offset text-length)
               (> text-length 0)
               lines-metadata)
      (let* ((last-entry-in-metadata (car (last lines-metadata))) ;; this is a list like (line-text offset idx)
             (last-line-text (first last-entry-in-metadata))
             (last-line-start-offset (second last-entry-in-metadata)))
        ;; condition: text ends with a newline implies the last "line" from str:split might be empty,
        ;; OR the last actual content line's offset + length + 1 (for newline) == text-length.
        ;; the (get-lines-with-metadata) function already handles offsets correctly.
        ;; if target-offset is text-length, it means it's after all characters.
        ;; this could be the start of an empty line if the text ended with a newline.
        (when (and (string= last-line-text "") ;; the last "line" produced by split is empty
                   (= last-line-start-offset target-offset)) ;; and it starts exactly at target-offset
          (setf found-line-details last-entry-in-metadata))))
    (unless found-line-details
      (return-from find-list-start-at-target-offset (values nil nil nil nil nil)))
    (destructuring-bind (target-line-text line-start target-line-idx) found-line-details
      (let ((indent-on-line (- target-offset line-start)))
        (when (< indent-on-line 0)
          (return-from find-list-start-at-target-offset (values nil nil nil nil nil)))
        (multiple-value-bind (is-bullet)
            (org-list-parse-bullet-line target-line-text indent-on-line)
          (if is-bullet
              (values target-offset
                      line-start
                      indent-on-line
                      (mapcar #'car (nthcdr target-line-idx lines-metadata))
                      t)
              (values nil nil nil nil nil)))))))

(defun org-list-get-bounds (text &optional target-start-offset)
  "determines the character start and end bounds of an org-mode list in TEXT.
returns (CONS START-OFFSET END-OFFSET) or nil if no list is found."
  (let (effective-list-start-char-offset
        first-list-line-actual-start-char-offset
        initial-indent-for-parser
        lines-to-feed-parser
        items
        remaining-lines-after-parse)
    (if target-start-offset
        (progn
          (multiple-value-bind (quick-check-success q-line-start q-init-indent)
              (quick-validate-list-start-at-offset text target-start-offset)
            (unless quick-check-success
              (return-from org-list-get-bounds nil))
            (setf effective-list-start-char-offset target-start-offset)
            (setf first-list-line-actual-start-char-offset q-line-start)
            (setf initial-indent-for-parser q-init-indent)
            ;; process only the suffix of the text starting from the identified line
            ;; pass :omit-nulls nil because org-list-parse-items might need empty lines.
            (let ((text-suffix (subseq text first-list-line-actual-start-char-offset)))
              (setf lines-to-feed-parser (str:split (string #\newline) text-suffix :omit-nulls nil)))))
        ;; else: no target-start-offset, find the first list item (uses get-lines-with-metadata)
        (let ((lines-metadata (get-lines-with-metadata text)) ;; get-lines-with-metadata is assumed to exist
              (found-list-start nil))
          (loop for (line-text line-start line-idx) in lines-metadata
                do
                   (let ((actual-indent (org-list-count-leading-spaces line-text)))
                     (multiple-value-bind (is-bullet-p)
                         (org-list-parse-bullet-line line-text actual-indent)
                       (when is-bullet-p
                         (setf effective-list-start-char-offset (+ line-start actual-indent))
                         (setf first-list-line-actual-start-char-offset line-start)
                         (setf initial-indent-for-parser actual-indent)
                         (setf lines-to-feed-parser (mapcar #'first (nthcdr line-idx lines-metadata)))
                         (setf found-list-start t)
                         (return))))
                finally (unless found-list-start
                          (return-from org-list-get-bounds nil)))))
    (when (null lines-to-feed-parser)
      (return-from org-list-get-bounds nil))
    (multiple-value-setq (items remaining-lines-after-parse)
      (org-list-parse-items lines-to-feed-parser initial-indent-for-parser))
    (unless items
      (return-from org-list-get-bounds nil))
    (let* ((num-lines-fed (length lines-to-feed-parser))
           (num-remaining-lines (length remaining-lines-after-parse))
           (num-consumed-lines (- num-lines-fed num-remaining-lines)))
      (when (<= num-consumed-lines 0)
        (return-from org-list-get-bounds nil))
      (let* ((consumed-lines-list (subseq lines-to-feed-parser 0 num-consumed-lines))
             (consumed-block-text-length
               (if consumed-lines-list
                   (loop for i from 0 below (length consumed-lines-list)
                         for line-text in consumed-lines-list
                         sum (+ (length line-text)
                                (if (< i (1- (length consumed-lines-list))) 1 0))) ;; add 1 for newline between lines
                   0)))
        (cons effective-list-start-char-offset
              (+ first-list-line-actual-start-char-offset consumed-block-text-length))))))

(defun org-list-parse (text &optional target-start-offset)
  "parses org-mode list TEXT.
returns a list of nodes (item-data . children-nodes) or NIL."
  (let (effective-lines-to-parse
        (initial-indent 0))
    (if target-start-offset
        (progn
          (unless (and (integerp target-start-offset) (>= target-start-offset 0))
            (error "TARGET-START-OFFSET must be a non-negative integer."))
          (multiple-value-bind (quick-check-success q-line-start q-init-indent)
              (quick-validate-list-start-at-offset text target-start-offset)
            (unless quick-check-success
              (return-from org-list-parse nil))
            (setf initial-indent q-init-indent)
            ;; process only the suffix of the text starting from the identified line
            ;; pass :omit-nulls nil because org-list-parse-items might need empty lines.
            (let ((text-suffix (subseq text q-line-start)))
              (setf effective-lines-to-parse (str:split (string #\newline) text-suffix :omit-nulls nil)))))
        ;; else: no target-start-offset (original behavior, trims text first)
        (let* ((trimmed-text (str:trim text))
               (raw-lines (if (string= trimmed-text "")
                              nil
                              ;; note: :omit-nulls t here is the original behavior for this branch
                              (str:split (string #\newline) trimmed-text :omit-nulls t))))
          (unless raw-lines (return-from org-list-parse nil))
          (loop for line in raw-lines
                for i from 0
                do (let ((indent (org-list-count-leading-spaces line)))
                     (multiple-value-bind (is-bullet) (org-list-parse-bullet-line line indent)
                       (when is-bullet
                         (setf initial-indent indent)
                         (setf effective-lines-to-parse (nthcdr i raw-lines))
                         (return))))
                finally (when (null effective-lines-to-parse)
                          (return-from org-list-parse nil)))))

    (if (null effective-lines-to-parse)
        nil
        (multiple-value-bind (items rem)
            (org-list-parse-items effective-lines-to-parse initial-indent)
          (declare (ignore rem))
          items))))

(defun quick-validate-list-start-at-offset (text target-offset)
  "quickly checks if a list bullet starts at TARGET-OFFSET in TEXT.
returns multiple values:
1. success-p (boolean): true if a valid bullet is found.
2. line-start-char-offset: the character offset of the start of the line containing TARGET-OFFSET.
3. initial-indent-for-parser: the indent calculated from line-start to TARGET-OFFSET.
returns (values nil nil nil) on failure."
  (when (or (< target-offset 0) (> target-offset (length text))) ;; allow target-offset to be length of text for potential empty line start
    (return-from quick-validate-list-start-at-offset (values nil nil nil)))
  ;; if target_offset is exactly at the end of the text
  (if (= target-offset (length text))
      (if (and (> (length text) 0) (char/= (char text (1- (length text))) #\Newline))
          ;; if text doesn't end with newline, target-offset at end is not start of a new line's content.
          (values nil nil nil)
          ;; else, text is empty, or ends with newline. treat as an empty line starting at target-offset.
          ;; the "indent" on this conceptual empty line, relative to itself, is 0.
          (multiple-value-bind (is-bullet)
              (org-list-parse-bullet-line "" 0)
            (if is-bullet
                (values t target-offset 0)
                (values nil nil nil))))
      ;; standard case: target-offset is within the text's characters
      (let* ((line-start-search-end (if (zerop target-offset) 0 target-offset))
             ;; find start of the line containing target-offset
             (prev-newline-pos (position #\newline text :end line-start-search-end :from-end t))
             (line-start-char-offset (if prev-newline-pos (1+ prev-newline-pos) 0))
             ;; find end of the line containing target-offset
             (next-newline-pos (position #\newline text :start target-offset))
             (line-end-char-offset (if next-newline-pos next-newline-pos (length text)))
             (target-line-text (subseq text line-start-char-offset line-end-char-offset))
             (indent-on-line (- target-offset line-start-char-offset)))
        ;; indent-on-line should not be negative if target-offset is on or after line-start-char-offset
        (when (< indent-on-line 0)
          (return-from quick-validate-list-start-at-offset (values nil nil nil)))
        (multiple-value-bind (is-bullet)
            (org-list-parse-bullet-line target-line-text indent-on-line)
          (if is-bullet
              (values t line-start-char-offset indent-on-line)
              (values nil nil nil))))))