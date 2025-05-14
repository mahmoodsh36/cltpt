(in-package :cltpt/org-mode)

(defun org-list-parse (text)
  (multiple-value-bind (items rem)
      (org-list-parse-items
       (str:split (string #\newline) (str:trim text) :omit-nulls t)
       0)
    (declare (ignore rem))
    items))

(defun org-list-parse-items (lines current-indent)
  (if (null lines)
      (values nil lines)
      (let ((first-line (first lines))
            (line-indent (org-list-count-leading-spaces (first lines))))
        (if (< line-indent current-indent)
            (values nil lines)
            (multiple-value-bind (is-bullet-at-this-level)
                (org-list-parse-bullet-line first-line current-indent)
              (if (not is-bullet-at-this-level)
                  (values nil lines)
                  (multiple-value-bind (item-cons rem-lines)
                      (org-list-parse-one-item lines current-indent)
                    (let ((node (car item-cons))
                          (children (cdr item-cons)))
                      (multiple-value-bind (siblings final-rem)
                          (org-list-parse-items rem-lines current-indent)
                        (values
                         (cons (append node (list :children children)) siblings)
                         final-rem))))))))))

(defun org-list-parse-one-item (lines current-indent)
  (multiple-value-bind (valid marker text-content)
      (org-list-parse-bullet-line (first lines) current-indent)
    (declare (ignore valid))
    (let ((remaining (rest lines)))
      (multiple-value-bind (extra-lines rem)
          (org-list-collect-extra-lines remaining current-indent)
        (setf remaining rem)
        (let ((children-forest nil))
          (when (and remaining
                     (> (org-list-count-leading-spaces (first remaining)) current-indent))
            (let* ((child-first-line (first remaining))
                   (child-indent (org-list-count-leading-spaces child-first-line)))
              (multiple-value-bind (is-child-bullet)
                  (org-list-parse-bullet-line child-first-line child-indent)
                (when is-child-bullet
                  (multiple-value-bind (child-items rem2)
                      (org-list-parse-items remaining child-indent)
                    (setf children-forest child-items)
                    (setf remaining rem2))))))
          (let* ((clean-text (string-trim " " text-content))
                 (combined-text (if extra-lines
                                    (concatenate 'string
                                                 clean-text
                                                 (string #\newline)
                                                 (str:join (string #\newline) extra-lines))
                                    clean-text))
                 (node-plist (list :marker marker :text combined-text)))
            (values (cons node-plist children-forest) remaining)))))))

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
                     (only-digits t)
                     (only-alphas t))
                 ;; find the end of the initial letter/digit sequence
                 ;; also track if it's purely digits or purely alphas
                 (loop while (and (< i (length trimmed))
                                  (let ((char (char trimmed i)))
                                    (cond
                                      ((digit-char-p char)
                                       (setf only-alphas nil) ;; not purely alphas
                                       t) 
                                      ((alpha-char-p char)
                                       (setf only-digits nil) ;; not purely digits
                                       t)
                                      (t nil)))) ;; stop if not letter or digit
                       do (incf i))
                 ;; now check conditions:
                 ;; 1. found at least one char for marker (i > 0)
                 ;; 2. there's a char after this sequence (< i (length trimmed)))
                 ;; 3. that char is a dot (char= (char trimmed i) #\.)
                 ;; 4. either the marker part was short (e.g. <= 3 chars, like "1.", "a.", "IV.")
                 ;;    OR it was purely digits (like "123.")
                 ;;    (this is to avoid matching "preamble.")
                 (if (and (> i 0)
                          (< i (length trimmed))
                          (char= (char trimmed i) #\.)
                          (or (<= i 3) ;; allow short mixed markers like "A1." if i becomes 2
                              only-digits))
                     (let* ((marker-end-idx (1+ i))
                            (marker (subseq trimmed 0 marker-end-idx))
                            (text-start-idx marker-end-idx))
                       (when (and (< text-start-idx (length trimmed))
                                  (char= (char trimmed text-start-idx) #\space))
                         (incf text-start-idx))
                       (values t marker (subseq trimmed text-start-idx)))
                     (values nil nil nil))))
              (t (values nil nil nil)))))))

(defun get-first-non-whitespace-char-index (text)
  (loop for i from 0 below (length text)
        for char = (char text i)
        unless (member char '(#\space #\newline #\tab #\return) :test #'char=)
          return i
        finally (return (length text))))

(defun org-list-parse-internal (text)
  (let* ((trimmed-text (str:trim text))
         (input-lines (if (string= trimmed-text "")
                          '()
                          (str:split (string #\newline) trimmed-text :omit-nulls t))))
    (if (null input-lines)
        (values nil nil)
        (org-list-parse-items input-lines 0))))

(defun org-list-get-bounds (text)
  (let ((list-block-start-char-offset nil)
        (current-char-offset 0)
        (lines (str:split (string #\newline) text :omit-nulls nil)))

    ;; 1. find the starting character offset of the first valid list item line
    (loop for line in lines
          for line-idx from 0
          for line-start-offset = current-char-offset
          do
             (let ((actual-indent (org-list-count-leading-spaces line)))
               (multiple-value-bind (is-bullet-p) 
                   (org-list-parse-bullet-line line actual-indent)
                 (when is-bullet-p
                   (setf list-block-start-char-offset line-start-offset)
                   (return))))
             (if (< (1+ line-idx) (length lines))
                 (incf current-char-offset (1+ (length line)))
                 (incf current-char-offset (length line))))

    (unless list-block-start-char-offset
      (return-from org-list-get-bounds (cons nil nil)))

    ;; 2. determine the actual start of list content
    (let* (;; Get the first line where a bullet was found
           (first-potential-list-line-text
             (subseq text list-block-start-char-offset
                     (or (position #\Newline text :start list-block-start-char-offset)
                         (length text))))
           ;; Get its true indentation
           (indent-on-first-list-line (org-list-count-leading-spaces first-potential-list-line-text))
           ;; The actual list content starts after these leading spaces on that line
           (actual-list-content-global-start-offset (+ list-block-start-char-offset indent-on-first-list-line))

           ;; The block to feed the parser starts from this precise point
           (block-for-parser (subseq text actual-list-content-global-start-offset))
           items
           remaining-lines-after-parse)

      (when (or (string= block-for-parser "")
                (>= actual-list-content-global-start-offset (length text)))
        (return-from org-list-get-bounds (cons nil nil)))

      (multiple-value-setq (items remaining-lines-after-parse)
        (org-list-parse-internal block-for-parser))

      (unless items
        (return-from org-list-get-bounds (cons nil nil)))

      (let* ((text-effectively-parsed (str:trim block-for-parser))
             (lines-fed-to-parser
               (if (string= text-effectively-parsed "")
                   nil
                   (str:split (string #\newline) text-effectively-parsed :omit-nulls t)))
             (num-lines-fed (length lines-fed-to-parser))
             (num-remaining-lines (length remaining-lines-after-parse))
             (num-consumed-lines (- num-lines-fed num-remaining-lines))
             (consumed-block-text-length 0))

        (cond
          ((<= num-consumed-lines 0)
           (return-from org-list-get-bounds (cons nil nil)))
          ((or (null remaining-lines-after-parse) (zerop num-remaining-lines))
           (setf consumed-block-text-length (length text-effectively-parsed)))
          (t
           (let ((consumed-lines-list (subseq lines-fed-to-parser 0 num-consumed-lines)))
             (setf consumed-block-text-length (length (str:join (string #\newline) consumed-lines-list))))))

        (let ((final-start-index actual-list-content-global-start-offset)
              (final-end-index (+ actual-list-content-global-start-offset consumed-block-text-length)))
          (cons final-start-index final-end-index))))))

(defun test-original-plist-output ()
  (let ((text "- item one
   extra text for one
- item two
   a. nested item one
      more nested text
   b. nested item two
   c. hi
- item three"))
    (format t "~%testing org-list-parse (plist output):~%")
    (let ((parsed-list (org-list-parse text)))
      (if parsed-list
          (print parsed-list)
          (format t "NIL (no list parsed or input was invalid for list structure)~%"))))

  (let ((text "not a list"))
    (format t "~%testing org-list-parse with non-list text:~%")
    (let ((parsed-list (org-list-parse text)))
      (if parsed-list
          (print parsed-list)
          (format t "NIL (no list parsed or input was invalid for list structure)~%")))))

(defun test-get-bounds ()
  (flet ((show-bounds (text)
           (let* ((bounds (org-list-get-bounds text))
                  (start (car bounds))
                  (end (cdr bounds)))
             (format t "~%text:~%~S~%" text)
             (if (and start end)
                 (format t "bounds: ~A, ~A~%substring: ~S~%" start end (subseq text start end))
                 (format t "bounds: ~S~%" bounds)))))

    (show-bounds "- item one
   extra text for one
- item two
   a. nested item one
      more nested text
   b. nested item two
   c. hi
- item three")

    (show-bounds "preamble text.
  - list item 1 starts here
  - list item 2
    - nested
after list.")

    (show-bounds "no list here.")

    (show-bounds (format nil "  ~C~C  - properly indented list~C    More stuff for it." #\newline #\newline #\newline))

    (show-bounds "- just one item")

    (show-bounds "  - indented item")

    (show-bounds " leading text
- item A
- item B
  1. sub item B1
trailing text")

    (show-bounds "- list
  - sublist
    - subsublist
- another top")

    (show-bounds (format nil "~C- item one~C- item two~C" #\newline #\newline #\newline))

    (show-bounds "  ")
    (show-bounds "")))