(defpackage :cltpt/combinator
  (:use :cl :str)
  (:export
   :literal :literal-casein :consec :parse :word-matcher :upcase-word-matcher
   :consec-atleast-one
   :symbol-matcher :scan-all-rules :any :all-but
   :all-but-newline :atleast-one :atleast-one-discard :lisp-sexp :pair
   :unescaped :natural-number-matcher :when-match
   :at-line-start-p :at-line-end-p :followed-by :match-rule
   :separated-atleast-one :all-but-whitespace))

(in-package :cltpt/combinator)

(defun normalize-match (match _rule str pos)
  "ensures a match result is in the standard plist cons structure."
  (declare (ignore _rule))
  (if (numberp match)
      (cons
       (list :begin pos
             :end (+ pos match)
             :match (subseq str pos (+ pos match)))
       nil)
      match))

(defun match-rule-normalized (rule str pos)
  "calls match-rule and ensures its result is normalized."
  (let ((raw-match (match-rule rule str pos)))
    (when raw-match
      (normalize-match raw-match rule str pos))))

(defun any (str pos &rest all)
  "match any of the given combinator rules."
  (loop for one in all
        for match = (match-rule-normalized one str pos)
        do (when match
             (return-from any match))))

(defun literal (str pos substr)
  "match a literal string."
  (let ((sublen (length substr)))
    (when (and (<= (+ pos sublen) (length str))
               (string= str substr :start1 pos :end1 (+ pos sublen) :end2 sublen))
      sublen)))

(defun literal-casein (str pos substr)
  "match a literal string, case-insensitive."
  (let ((sublen (length substr)))
    (when (<= (+ pos sublen) (length str))
      (when (string-equal str substr :start1 pos :end1 (+ pos sublen) :end2 sublen)
        sublen))))

(defun eng-char-p (str pos)
  "match an english character."
  (when (< pos (length str))
    (when (alpha-char-p (char str pos))
      1)))

(defun eng-alphanump (str pos)
  "match an english character or a digit."
  (when (or (alpha-char-p (char str pos))
            (digit-char-p (char str pos)))
    1))

(defun symbol-char (str pos)
  "helper for `symbol-matcher'."
  (when (or (eng-alphanump str pos)
            (char= (char str pos) #\-)
            (char= (char str pos) #\_)
            (char= (char str pos) #\*)
            (char= (char str pos) #\$))
    1))

(defun symbol-matcher (str pos)
  "matches a 'symbol', which may include some special characters."
  (match-rule '(atleast-one-discard (symbol-char)) str pos))

(defun all-but (str pos exceptions)
  "match everything but the characters in EXCEPTIONS."
  (let ((start pos))
    (loop while (< pos (length str))
          for c = (char str pos)
          while (not (find c exceptions :test #'char=))
          do (incf pos))
    (when (> pos start)
      (- pos start))))

(defun only (str pos allowed)
  (let ((start pos))
    (when (or (>= pos (length str))
              (not (find (char str pos) allowed :test #'char=)))
      (return-from only nil))
    (loop while (< pos (length str))
          for c = (char str pos)
          while (find c allowed :test #'char=)
          do (incf pos))
    (- pos start)))

(defun all-but-newline (str pos)
  "match all characters but a newline."
  (all-but str pos (string #\newline)))

;; we should add more chars that quality as whitespace
(defun all-but-whitespace (str pos)
  "match all characters but whitespaces."
  (all-but str pos (concatenate 'string " " (string #\newline))))

(defun consec (str pos &rest all)
  "match a consecutive set of rules, each one has to be present."
  (let ((start pos)
        (matches))
    (loop for one in all
          for match = (match-rule-normalized one str pos)
          for len = (when match
                      (- (getf (car match) :end)
                         (getf (car match) :begin)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (getf (car match) :end))
                   (push match matches))
                 (return-from consec nil)))
    (cons (list :begin start
                :end pos
                :match (subseq str start pos))
          (nreverse matches))))

(defun consec-atleast-one (str pos &rest all)
  "match a consecutive set of rules, atleast the first has to be present.

the matcher stops once it encounters a rule that hasnt been matched, and returns
the consecutive matches up to that point."
  (let ((start pos)
        (matches))
    (loop for one in all
          for match = (match-rule-normalized one str pos)
          for len = (when match
                      (- (getf (car match) :end)
                         (getf (car match) :begin)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (getf (car match) :end))
                   (push match matches))
                 (return)))
    (when matches
      (cons (list :begin start
                  :end pos
                  :match (subseq str start pos))
            (nreverse matches)))))

;; a consecutive set of matchers, separated by a specific matcher. atleast one
;; note that this also detects a separator at the end, should be an easy fix
(defun separated-atleast-one (str pos sep-matcher matcher)
  "apply the sequence of matcher `ALL' separated by `SEP-MATCHER'."
  (let ((start pos)
        (matches)
        (first t)
        (sep-match))
    (loop
      do (unless first
           ;; match separator
           (let ((match (match-rule-normalized sep-matcher str pos)))
             (unless match
               (return))
             (setf pos (getf (car match) :end))
             (setf sep-match match)))
         (let ((match (match-rule-normalized matcher str pos)))
           (unless match
             (return))
           (setf pos (getf (car match) :end))
           (when first
             (push sep-match matches))
           (push match matches))
         (setf first nil))
    (when matches
      (cons (list :begin start
                  :end pos
                  :match (subseq str start pos))
            (nreverse matches)))))

(defun atleast-one (str pos matcher)
  "match the rule MATCHER atleast once."
  (let ((start pos)
        (matches))
    (loop while (< pos (length str))
          for match = (match-rule-normalized matcher str pos)
          while match
          for len = (when match
                      (- (getf (car match) :end)
                         (getf (car match) :begin)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (getf (car match) :end))
                   (push match matches))
                 (return)))
    (when matches
      (cons (list :begin start
                  :end pos
                  :match (subseq str start pos))
            (nreverse matches)))))

(defun atleast-one-discard (str pos matcher)
  "like `atleast-one', but doesnt collect submatches. better performance."
  (let ((start pos))
    (loop while (< pos (length str))
          for match = (match-rule-normalized matcher str pos)
          while match
          for len = (when match
                      (- (getf (car match) :end)
                         (getf (car match) :begin)))
          do (if (and match len (plusp len))
                 (setf pos (getf (car match) :end))
                 (return)))
    (when (> pos start)
      (cons (list :begin start
                  :end pos
                  :match (subseq str start pos))
            nil))))

;; shouldnt we be matching words from any language?
(defun word-matcher (str pos)
  "match an english word."
  (match-rule `(atleast-one-discard (eng-char-p)) str pos))

(defun digit-p (str pos)
  "returns 1 if char at POS of STR is a digit, NIL otherwise."
  (and (< pos (length str))
       (digit-char-p (char str pos))
       1))

(defun natural-number-matcher (str pos)
  "matches a natural number."
  (match-rule `(atleast-one-discard (digit-p)) str pos))

(defun pair (str pos opening-rule closing-rule
             &optional rules-for-content pair-id (allow-multiline t))
  "matches an opening-rule, then content parsed by rules-for-content, then a closing-rule.
handles nesting of the same pair structure.
if ALLOW-MULTILINE is NIL, the match will fail if a newline is encountered
before the final closing rule is found."
  (let ((opening-match (match-rule-normalized opening-rule str pos)))
    (when opening-match
      (let* ((open-parent-info (car opening-match))
             (open-end-pos (getf open-parent-info :end))
             (current-search-pos open-end-pos)
             (nesting-level 1)
             (content-start-pos open-end-pos)
             (final-closing-match)
             (content-end-pos -1))
        (loop while (< current-search-pos (length str))
              do
                 ;; if multiline is not allowed and we are at the base nesting level,
                 ;; fail the match if we encounter a newline character.
                 (when (and (not allow-multiline)
                            (= nesting-level 1)
                            (char= (char str current-search-pos) #\newline))
                   (return-from pair nil))
                 (let ((potential-close
                         (match-rule-normalized closing-rule
                                                str
                                                current-search-pos)))
                   (if potential-close
                       (progn
                         (decf nesting-level)
                         (setf current-search-pos (getf (car potential-close) :end))
                         (when (= nesting-level 0)
                           (setf final-closing-match potential-close)
                           (setf content-end-pos (getf (car potential-close) :begin))
                           (return)))
                       (let ((potential-open (match-rule-normalized opening-rule str current-search-pos)))
                         (if potential-open
                             (progn
                               (incf nesting-level)
                               (setf current-search-pos (getf (car potential-open) :end)))
                             (incf current-search-pos))))))
        (when (and final-closing-match (= nesting-level 0))
          (let* ((closing-parent-info (car final-closing-match))
                 (overall-end-pos (getf closing-parent-info :end))
                 (content-matches
                   (if (> content-end-pos content-start-pos)
                       (scan-all-rules str rules-for-content content-start-pos content-end-pos)
                       nil))
                 (parent-node-info (list :begin pos
                                         :end overall-end-pos
                                         :match (subseq str pos overall-end-pos)))
                 (children-nodes (append (list opening-match)
                                         content-matches
                                         (list final-closing-match))))
            (when pair-id
              (setf (getf parent-node-info :id) pair-id))
            (cons parent-node-info children-nodes)))))))

(defun compile-rule-string-helper (spec bindings)
  (let ((result)
        (len (length spec))
        (segment-start 0)
        (i 0))
    (loop while (< i len)
          do (let ((char (char spec i)))
               (if (char= char #\%)
                   (if (< (1+ i) len)
                       (let ((key-char (char spec (1+ i))))
                         (when (> i segment-start)
                           (push (list 'literal (subseq spec segment-start i))
                                 result))
                         (cond
                           ((char= key-char #\%)
                            (push (list 'literal "%") result)
                            (incf i 2)
                            (setf segment-start i))
                           (t
                            (let ((pair (assoc key-char bindings)))
                              (if pair
                                  (push (cdr pair) result)
                                  (push (list 'literal (subseq spec i (+ i 2)))
                                        result))
                              (incf i 2)
                              (setf segment-start i)))))
                       (progn
                         (incf i)))
                   (incf i))))
    (when (> i segment-start)
      (push (list 'literal (subseq spec segment-start i)) result))
    (nreverse result)))

(defun compile-rule-string (str)
  (let ((my-replacements
          '((#\w . (word-matcher))
            (#\W . (symbol-matcher))
            (#\a . (all-but-newline))
            )))
    (compile-rule-string-helper str my-replacements)))

;; i forgot why we're matching dollar signs here, should probably remove that
;; and use symbol-matcher instead where this was needed.
(defun word-digits-hyphen (str pos)
  "matches a (non-empty) word with hyphens, underscores or dollar signs."
  (let ((start pos))
    (if (or (>= pos (length str))
            (not (or (alpha-char-p (char str pos))
                     (digit-char-p (char str pos))
                     (char= (char str pos) #\-)
                     (char= (char str pos) #\_)
                     (char= (char str pos) #\$))))
        (return-from word-digits-hyphen nil)
        (loop while (< pos (length str))
              for c = (char str pos)
              while (or (alpha-char-p c)
                        (digit-char-p (char str pos))
                        (char= c #\-)
                        (char= c #\_)
                        (char= c #\$))
              do (incf pos)))
    (- pos start)))

(defun word-digits (str pos)
  "matches a (non-empty) word (possibly) with digits."
  (let ((start pos))
    (if (or (>= pos (length str))
            (not (or (alpha-char-p (char str pos))
                     (digit-char-p (char str pos)))))
        (return-from word-digits nil)
        (loop while (< pos (length str))
              for c = (char str pos)
              while (or (alpha-char-p c)
                        (digit-char-p c))
              do (incf pos)))
    (- pos start)))

(defun lisp-sexp (str pos)
  "reads a single lisp S-expression from the current position."
  (let ((lisp-form)
        (read-error)
        (chars-consumed 0)
        (form-str))
    (when (and (< pos (length str))
               ;; dont read it if it starts with space (default behavior by `read')
               (not (char-equal (char str pos) #\space)))
      (with-input-from-string (s str :start pos)
        (handler-case
            (setf lisp-form (read s))
          (error (c)
            (setf read-error c)))
        (unless read-error
          (setf chars-consumed (file-position s))
          (when (> chars-consumed 0)
             (setf form-str (subseq str pos (+ pos chars-consumed)))))))
    (if (and form-str (> chars-consumed 0))
        (cons (list :begin pos
                    :end (+ pos chars-consumed)
                    :match form-str
                    :id 'lisp-form-content)
              nil)
        nil)))

(defun match-rule (rule str pos)
  "returns a 'raw' match: a number (length) for simple successful matches,
or a pre-formed plist cons cell for combinators/structured matches, or NIL."
  (cond
    ((and (listp rule) (symbolp (car rule)) (fboundp (car rule)))
     (apply (car rule) str pos (cdr rule)))
    ((and (listp rule) (getf rule :pattern))
     ;; exploit :on-char here too, for some (small?) gains
     (unless (and (getf rule :on-char)
                  (< pos (length str))
                  (not (char= (char str pos) (getf rule :on-char))))
       (let ((sub-pattern-match
               (match-rule-normalized
                (getf rule :pattern)
                str
                pos)))
         (when sub-pattern-match
           (let* ((parent-info (car sub-pattern-match))
                  (new-parent-info (list* :id (getf rule :id) parent-info)))
             (cons new-parent-info (cdr sub-pattern-match)))))))
    ((stringp rule)
     (let ((compiled (compile-rule-string rule)))
       (if (> (length compiled) 1)
           ;; if stringified pattern compiles to multiple rules we use `consec'.
           (match-rule (cons 'consec compiled) str pos)
           (match-rule (car compiled) str pos))))
    (t (error "invalid rule: ~A" rule))))

;; the hash table thing is a heuristic that makes things slightly faster
(defun hash-rules (rules)
  "construct a hash table by :on-char of the list of plists RULES."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for rule in rules
          do (when (and (keywordp (car rule)) (getf rule :on-char))
               (push rule (gethash (getf rule :on-char) hash))))
    hash))

(defun scan-all-rules (str rules &optional (start-idx 0) (end-idx (length str)))
  "iterate through STR, apply each matcher of RULES repeatedly at each position."
  (let* ((events)
         (i start-idx)
         (rule-hash (hash-rules rules))
         (hashed-rules (loop for value being the hash-values of rule-hash
                             append value))
         (unhashed-rules (set-difference rules hashed-rules :test #'equal)))
    (loop while (< i end-idx)
          do (let ((matched)
                   (current-char (char str i)))
               (labels ((handle-rule (rule)
                          (let ((raw-match (match-rule rule str i)))
                            (when raw-match
                              (let* ((match-result (normalize-match raw-match rule str i)))
                                ;; (when (<= (getf (car match-result) :end) end-idx)
                                (setf i (getf (car match-result) :end))
                                (push match-result events)
                                (setf matched t))))))
                 (loop for rule in (union unhashed-rules
                                          (gethash current-char rule-hash))
                       do (handle-rule rule)
                       finally (unless matched
                                 (incf i))))))
    (nreverse events)))

(defun parse (str rules)
  (scan-all-rules str rules 0 (length str)))

(defun is-preceded-by-odd-escape-p (str pos escape-char)
  "checks if the character at pos is preceded by an odd number of
contiguous escape-char characters."
  (if (zerop pos)
      nil
      (let ((i (1- pos))
            (escape-count 0))
        (loop while (and (>= i 0) (char= (char str i) escape-char))
              do (incf escape-count)
                 (decf i))
        (oddp escape-count))))

(defun unescaped (str pos rule &optional (escape-char #\\))
  "a combinator that fails if the current position is escaped.
if not escaped, it attempts to match the given 'rule'.
an optional escape-char can be provided, defaulting to backslash."
  ;; check if the current position is escaped by an odd number of escape chars.
  (if (is-preceded-by-odd-escape-p str pos escape-char)
      nil
      (match-rule rule str pos)))

(defun at-line-start-p (str pos)
  "predicate to check if the current position is at the start of a line."
  (or (zerop pos)
      (char= (char str (1- pos)) #\newline)))

(defun at-line-end-p (str pos)
  "predicate to check if the current position is at the end of a line
(i.e., at the end of the string or followed by a newline)."
  (or (>= pos (length str))
      (char= (char str pos) #\newline)))

(defun when-match (str pos rule condition-fn)
  "a combinator that attempts to match 'rule' only if 'condition-fn' returns true.
'condition-fn' is a function that takes str and pos and returns a boolean."
  (when (funcall condition-fn str pos)
    (match-rule rule str pos)))

(defun followed-by (str pos rule condition-fn)
  "a combinator that succeeds only if 'rule' matches and the position
immediately after the match satisfies 'condition-fn'."
  (let ((match (match-rule-normalized rule str pos)))
    (when match
      (let ((end-pos (getf (car match) :end)))
        (when (funcall condition-fn str end-pos)
          match)))))

(defun upcase-char-p (str pos)
  "returns 1 if the char at POS of STR is an uppercase character."
  (and (< pos (length str))
       (upper-case-p (char str pos))
       1))

(defun upcase-word-matcher (str pos)
  "matches an uppercase (non-empty) word."
  (match-rule `(atleast-one-discard (upcase-char-p)) str pos))