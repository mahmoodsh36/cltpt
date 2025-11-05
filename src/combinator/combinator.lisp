(defpackage :cltpt/combinator
  (:use :cl)
  (:import-from :cltpt/combinator/match
   :match-id :match-begin :match-end :match-ctx :match-children :match-str
   :make-match :match-clone :match-rule
   :find-submatch :find-submatch-last :find-submatch-all)
  (:export
   :literal :literal-casein :consec :parse :word-matcher :upcase-word-matcher
   :consec-atleast-one :symbol-matcher :scan-all-rules :any :all-but
   :all-but-newline :atleast-one :atleast-one-discard :lisp-sexp :pair
   :unescaped :natural-number-matcher :when-match :at-line-start-p
   :at-line-end-p :followed-by :separated-atleast-one
   :all-but-whitespace :handle-rule-string :all-upto
   :all-upto-included :succeeded-by :all-upto-without
   :context-rules :consec-with-optional :compile-rule-string
   :between-whitespace :when-match-after :flanked-by-whitespace
   :apply-rule-normalized

   :match-id :match-begin :match-end :match-ctx :match-children :match-str
   :make-match :match-clone :match-rule
   :find-submatch :find-submatch-last :find-submatch-all

   :find-submatch :find-submatch-all :find-submatch-last :match-text
   :copy-rule :copy-modify-rule))

(in-package :cltpt/combinator)

;; ways to improve this code:
;; 1. we always invoke :match on the boundaries we find, this is too slow. often we might be able to get away with just storing the boundaries we found, and storing a reference of the main string, then computing :match on demand. this can be ddone perhaps using a custom structure with a special access function `get-match' built exactly for this.
;; 2. the `pair' function should be able to receive a predicate that may discard "false positives". e.g. we dont want \begin{env1} \end{env2} to be matched, which currently happens.
;; 3. the `pair' function isnt optimal, it does redundant work.
;; 4. we need a function that may discard any custom "left padding" such as a sequence of characters used for comments, e.g. ";;" in this case. in this exact comment section we have ";;" at the start of each line, we need a function that takes a "list matcher", and allows it to work regardless of those "padding sequences".
;; 5. `getf' is executed ridiculously often. perhaps we should use structs instead of plists, or something that makes access actually O(1). (constant-size plists are O(1), but not really.)
;; 6. some rules may only want to attempt matching in very specific cases, such as the beginning of a line, ending of a line. currently we support the heuristic of :on-char, and it speeds things up because it reduces the number of matchers executed at each point in the string, but we can add other heuristics too (perhaps an :after-char which for example could be used for ':after-char #\newline').
;; 7. no need to store :str for every match (also :ctx isnt needed to be stored in matches at all).
;; 8. the usage of this code is currently done on buffers after they have been loaded from the files, which results in the buffer being processed atleast twice. a better approach would be to process the string while its being loaded into memory.

;; this is used to keep track of the rules being processed, so that a matcher
;; may be aware of other matches
(defstruct context
  rules
  ;; we hash rules by id, so we can grab them quickly during matching
  rule-hash)

(defun match-text (match)
  (when match
    (subseq (match-str match)
            (match-begin match)
            (match-end match))))

(defun plistp (list1)
  "check whether LIST1 is a plist."
  (and (consp list1)
       (keywordp (car list1))))

(defun copy-rule (rule id &key type)
  (if (plistp rule)
      (let ((copy (copy-tree rule)))
        (setf (getf copy :id) id)
        (when type
          (setf (getf copy :type) type))
        copy)
      (if type
          (list :pattern (copy-tree rule) :id id :type type)
          (list :pattern (copy-tree rule) :id id))))

(defun copy-modify-rule (rule modifications)
  "copy a RULE, apply MODIFICATIONS to it.

MODIFICATIONS is an alist of the form (id . new-rule) where id is the subrule
to replace and new-rule is the rule to replace it with."
  (let ((new-rule (copy-tree rule)))
    (cltpt/tree:tree-map
     new-rule
     (lambda (subrule)
       (when (plistp subrule)
         (loop for modification in modifications
               for modification-id = (car modification)
               for modification-rule = (cdr modification)
               do (if (equal (getf subrule :id) modification-id)
                      (setf (getf subrule :pattern) modification-rule))))))
    new-rule))

;; TODO: we are constructing this context on every call to the combinator
;; rules dont change often (if at all), so we should cache this, maybe not
;; in the combinator itself because that may introduce some unneeded overhead,
;; in cases where the combinator isnt called many times with the same rules
;; in other applications of this combinator library
;; but maybe introduce this caching in other parts of cltpt's code that can benefit form it.
(defun make-context-from-rules (rules)
  (let ((ctx (make-context))
        (rule-hash (make-hash-table :test 'equal)))
    (setf (context-rules ctx) rules)
    (setf (context-rule-hash ctx) rule-hash)
    (loop for rule in rules
          do (cltpt/tree:tree-map
              rule
              (lambda (node)
                ;; if its a plist it could be a rule with an id
                (when (and (consp node) (keywordp (car node)))
                  (let ((node-id (getf node :id)))
                    (when node-id
                      (setf (gethash node-id rule-hash) node)))))))
    ctx))

(defun whitespace-p (char)
  "Predicate to check if a character is whitespace (space, newline, or tab)."
  (member char '(#\space #\newline #\tab)))

(defmethod context-rule-by-id ((ctx context) rule-id)
  (gethash rule-id (context-rule-hash ctx)))

(defun normalize-match (match ctx rule str pos)
  "ensures a match result is in the standard plist cons structure."
  (if (numberp match)
      (make-match :begin pos
                  :end (+ pos match)
                  :str str
                  :ctx ctx
                  :children nil)
      match))

(defun apply-rule-normalized (ctx rule str pos)
  "calls match-rule and ensures its result is normalized."
  (let ((raw-match (apply-rule ctx rule str pos)))
    (when raw-match
      (normalize-match raw-match ctx rule str pos))))

(defun any (ctx str pos &rest all)
  "match any of the given combinator rules."
  (loop for one in all
        for match = (apply-rule-normalized ctx one str pos)
        do (when match
             (return-from any
               (make-match :begin (match-begin match)
                           :end (match-end match)
                           :str str
                           :ctx ctx
                           :children (list match))))))

(defun literal (ctx str pos substr)
  "match a literal string."
  (let ((sublen (length substr)))
    (when (and (<= (+ pos sublen) (length str))
               (string= str
                        substr
                        :start1 pos
                        :end1 (+ pos sublen)
                        :end2 sublen))
      sublen)))

(defun literal-casein (ctx str pos substr)
  "match a literal string, case-insensitive."
  (let ((sublen (length substr)))
    (when (<= (+ pos sublen) (length str))
      (when (string-equal str substr :start1 pos :end1 (+ pos sublen) :end2 sublen)
        sublen))))

(defun eng-char-p (ctx str pos)
  "match an english character."
  (when (< pos (length str))
    (when (alpha-char-p (char str pos))
      1)))

(defun eng-alphanump (ctx str pos)
  "match an english character or a digit."
  (when (or (alpha-char-p (char str pos))
            (digit-char-p (char str pos)))
    1))

(defun symbol-char (ctx str pos)
  "helper for `symbol-matcher'."
  (when (or (eng-alphanump ctx str pos)
            (char= (char str pos) #\-)
            (char= (char str pos) #\+)
            (char= (char str pos) #\_)
            (char= (char str pos) #\*)
            (char= (char str pos) #\$))
    1))

(defun symbol-matcher (ctx str pos)
  "matches a 'symbol', which may include some special characters."
  (apply-rule ctx '(atleast-one-discard (symbol-char)) str pos))

(defun all-but (ctx str pos exceptions)
  "match everything but the characters in EXCEPTIONS."
  (let ((start pos))
    (loop while (< pos (length str))
          for c = (char str pos)
          while (not (find c exceptions :test #'char=))
          do (incf pos))
    (when (> pos start)
      (- pos start))))

(defun only (ctx str pos allowed)
  (let ((start pos))
    (when (or (>= pos (length str))
              (not (find (char str pos) allowed :test #'char=)))
      (return-from only nil))
    (loop while (< pos (length str))
          for c = (char str pos)
          while (find c allowed :test #'char=)
          do (incf pos))
    (- pos start)))

(defun all-but-newline (ctx str pos)
  "match all characters but a newline."
  (all-but ctx str pos (string #\newline)))

;; we should add more chars that quality as whitespace
(defun all-but-whitespace (ctx str pos)
  "match all characters but whitespaces."
  (all-but ctx str pos (concatenate 'string " " (string #\newline))))

(defun consec (ctx str pos &rest all)
  "match a consecutive set of rules, each one has to be present."
  (let ((start pos)
        (matches))
    (loop for one in all
          for match = (apply-rule-normalized ctx one str pos)
          for len = (when match
                      (- (match-end match)
                         (match-begin match)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (match-end match))
                   (push match matches))
                 (return-from consec nil)))
    (make-match :begin start
                :end pos
                :str str
                :ctx ctx
                :children (nreverse matches))))

(defun consec-atleast-one (ctx str pos &rest all)
  "match a consecutive set of rules, atleast the first has to be present.

the matcher stops once it encounters a rule that hasnt been matched, and returns
the consecutive matches up to that point."
  (let ((start pos)
        (matches))
    (loop for one in all
          for match = (apply-rule-normalized ctx one str pos)
          for len = (when match
                      (- (match-end match)
                         (match-begin match)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (match-end match))
                   (push match matches))
                 (return)))
    (when matches
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children (nreverse matches)))))

;; a consecutive set of matchers, separated by a specific matcher. atleast one
(defun separated-atleast-one (ctx str pos sep-matcher matcher)
  "apply the sequence of matcher `MATCHER' separated by `SEP-MATCHER'."
  (let ((start pos)
        (matches)
        (first t)
        (sep-match)
        (pos-after-sep pos))
    (loop
      do (unless first
           ;; match separator
           (let ((match (apply-rule-normalized ctx sep-matcher str pos)))
             (unless match
               (return))
             (setf pos-after-sep (match-end match))
             (setf sep-match match)))
         (let ((match (apply-rule-normalized ctx matcher str pos-after-sep)))
           (unless match
             (return))
           (setf pos (match-end match))
           (unless first
             (push sep-match matches))
           (push match matches))
         (setf first nil))
    (when matches
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children (nreverse matches)))))

(defun atleast-one (ctx str pos matcher)
  "match the rule MATCHER atleast once."
  (let ((start pos)
        (matches))
    (loop while (< pos (length str))
          for match = (apply-rule-normalized ctx matcher str pos)
          while match
          for len = (when match
                      (- (match-end match)
                         (match-begin match)))
          do (if (and match len (plusp len))
                 (progn
                   (setf pos (match-end match))
                   (push match matches))
                 (return)))
    (when matches
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children (nreverse matches)))))

(defun atleast-one-discard (ctx str pos matcher)
  "like `atleast-one', but doesnt collect submatches. better performance."
  (let ((start pos))
    (loop while (< pos (length str))
          for match = (apply-rule-normalized ctx matcher str pos)
          while match
          for len = (when match
                      (- (match-end match)
                         (match-begin match)))
          do (if (and match len (plusp len))
                 (setf pos (match-end match))
                 (return)))
    (when (> pos start)
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children nil))))

;; shouldnt we be matching words from any language?
(defun word-matcher (ctx str pos)
  "match an english word."
  (apply-rule ctx `(atleast-one-discard (eng-char-p)) str pos))

(defun digit-p (ctx str pos)
  "returns 1 if char at POS of STR is a digit, NIL otherwise."
  (and (< pos (length str))
       (digit-char-p (char str pos))
       1))

(defun natural-number-matcher (ctx str pos)
  "matches a natural number."
  (apply-rule ctx `(atleast-one-discard (digit-p)) str pos))

(defun pair (ctx str pos opening-rule closing-rule
             &optional rules-for-content pair-id (allow-multiline t))
  "matches an opening-rule, then content parsed by rules-for-content, then a closing-rule.
handles nesting of the same pair structure.
if ALLOW-MULTILINE is NIL, the match will fail if a newline is encountered
before the final closing rule is found."
  (let ((opening-match (apply-rule-normalized ctx opening-rule str pos)))
    (when opening-match
      (let* ((open-end-pos (match-end opening-match))
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
                         (apply-rule-normalized ctx
                                                closing-rule
                                                str
                                                current-search-pos)))
                   (if potential-close
                       (progn
                         (decf nesting-level)
                         (setf current-search-pos (match-end potential-close))
                         (when (= nesting-level 0)
                           (setf final-closing-match potential-close)
                           (setf content-end-pos (match-begin potential-close))
                           (return)))
                       (let ((potential-open
                               (apply-rule-normalized ctx
                                                      opening-rule
                                                      str
                                                      current-search-pos)))
                         (if potential-open
                             (progn
                               (incf nesting-level)
                               (setf current-search-pos
                                     (match-end potential-open)))
                             (incf current-search-pos))))))
        (when (and final-closing-match (= nesting-level 0))
          (let* ((overall-end-pos (match-end final-closing-match))
                 (content-matches
                   (if (> content-end-pos content-start-pos)
                       (scan-all-rules ctx
                                       str
                                       rules-for-content
                                       content-start-pos
                                       content-end-pos)
                       nil))
                 (match (make-match
                         :begin pos
                         :end overall-end-pos
                         :str str
                         :ctx ctx
                         :children (append (list opening-match)
                                           content-matches
                                           (list final-closing-match))))
                 (children-nodes ))
            (when pair-id
              (setf (match-id match) pair-id))
            match))))))

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
                       (incf i))
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

(defun handle-rule-string (str)
  (let ((compiled (compile-rule-string str)))
    (if (> (length compiled) 1)
        ;; if stringified pattern compiles to multiple rules we use `consec'.
        (cons 'consec compiled)
        (car compiled))))

;; i forgot why we're matching dollar signs here, should probably remove that
;; and use symbol-matcher instead where this was needed.
(defun word-digits-hyphen (ctx str pos)
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

(defun word-digits (ctx str pos)
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

(defun lisp-sexp (ctx str pos)
  "reads a single lisp S-expression from the current position."
  (let ((lisp-form)
        (read-error)
        (chars-consumed 0)
        (form-str))
    (when (and (< pos (length str))
               ;; dont read it if it starts with space (default behavior by `read')
               (not (whitespace-p (char str pos))))
      (with-input-from-string (s str :start pos)
        (handler-case
            (setf lisp-form (read s))
          (error (c)
            (setf read-error c)))
        (unless read-error
          (setf chars-consumed (file-position s))
          (when (> chars-consumed 0)
            ;; TODO: no need to run subseq here or even assign "form-str"
            (setf form-str (subseq str pos (+ pos chars-consumed)))))))
    (if (and form-str (> chars-consumed 0))
        (make-match :begin pos
                    :end (+ pos (length form-str))
                    :str str
                    :ctx ctx
                    :id 'lisp-form-content
                    :children nil)
        nil)))

(defun apply-rule (ctx rule str pos)
  "returns a 'raw' match: a number (length) for simple successful matches,
or a pre-formed plist cons cell for combinators/structured matches, or NIL."
  (let ((result))
    (setf
     result
     (cond
       ;; if its a single symbol it must be the id of a rule, we use context
       ;; to retrieve the rule
       ((and (symbolp rule) (context-rule-by-id ctx rule))
        (apply-rule ctx (context-rule-by-id ctx rule) str pos))
       ((and (listp rule) (symbolp (car rule)) (fboundp (car rule)))
        (apply (car rule) ctx str pos (cdr rule)))
       ((and (listp rule) (getf rule :pattern))
        ;; exploit :on-char here too, for some (small?) gains
        (unless (and (getf rule :on-char)
                     (< pos (length str))
                     (not (char= (char str pos) (getf rule :on-char))))
          (let ((sub-pattern-match
                  (apply-rule-normalized
                   ctx
                   (getf rule :pattern)
                   str
                   pos)))
            (when sub-pattern-match
              (setf (match-id sub-pattern-match) (getf rule :id))
              (setf (match-rule sub-pattern-match) rule)
              sub-pattern-match))))
       ((stringp rule)
        (apply-rule ctx (list 'literal rule) str pos))
       (t (error "invalid rule: ~A" rule))))
    (when result
      (normalize-match result ctx rule str pos))))

;; the hash table thing is a heuristic that makes things slightly faster
(defun hash-rules (rules)
  "construct a hash table by :on-char of the list of plists RULES."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for rule in rules
          do (when (and (consp rule) (keywordp (car rule)) (getf rule :on-char))
               (push rule (gethash (getf rule :on-char) hash))))
    hash))

(defun scan-all-rules (ctx str rules &optional (start-idx 0) (end-idx (length str)))
  "iterate through STR, apply each matcher of RULES repeatedly at each position."
  (let* ((events)
         (i start-idx)
         (rule-hash (hash-rules rules))
         ;; hash rules by :on-char to speed up iteration, we only call hashed
         ;; rules when we see the :on-char they need. this way we avoid
         ;; trying all rules at all locations
         (hashed-rules (loop for value being the hash-values of rule-hash
                             append value))
         (unhashed-rules (set-difference rules hashed-rules :test #'equal))
         (ctx (or ctx (make-context-from-rules rules))))
    (loop while (< i end-idx)
          do (let ((matched)
                   (current-char (char str i)))
               (labels ((handle-rule (rule)
                          (let ((match-result (apply-rule ctx rule str i)))
                            (when match-result
                              ;; (when (<= (getf (car match-result) :end) end-idx)
                              (setf i (match-end match-result))
                              (push match-result events)
                              (setf matched t)))))
                 (loop for rule in (union unhashed-rules
                                          (gethash current-char rule-hash))
                       until matched
                       do (handle-rule rule)
                       finally (unless matched
                                 (incf i))))))
    (nreverse events)))

(defun parse (str rules)
  (scan-all-rules nil str rules 0 (length str)))

(defun is-preceded-by-odd-escape-p (ctx str pos escape-char)
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

(defun unescaped (ctx str pos rule &optional (escape-char #\\))
  "a combinator that fails if the current position is escaped.
if not escaped, it attempts to match the given 'rule'.
an optional escape-char can be provided, defaulting to backslash."
  ;; check if the current position is escaped by an odd number of escape chars.
  (if (is-preceded-by-odd-escape-p ctx str pos escape-char)
      nil
      (apply-rule ctx rule str pos)))

(defun at-line-start-p (ctx str pos)
  "predicate to check if the current position is at the start of a line."
  (or (zerop pos)
      (char= (char str (1- pos)) #\newline)))

(defun at-line-end-p (ctx str pos)
  "predicate to check if the current position is at the end of a line
(i.e., at the end of the string or followed by a newline)."
  (or (>= pos (length str))
      (char= (char str pos) #\newline)))

(defun when-match (ctx str pos rule condition-fn)
  "a combinator that attempts to match 'rule' only if 'condition-fn' returns true.
'condition-fn' is a function that takes str and pos and returns a boolean."
  (when (funcall condition-fn ctx str pos)
    (apply-rule ctx rule str pos)))

(defun followed-by (ctx str pos rule condition-fn)
  "a combinator that succeeds only if 'rule' matches and the position
immediately after the match satisfies 'condition-fn'."
  (let ((match (apply-rule-normalized ctx rule str pos)))
    (when match
      (let ((end-pos (match-end match)))
        (when (funcall condition-fn ctx str end-pos)
          match)))))

(defun succeeded-by (ctx str pos pattern successor-pattern)
  "match PATTERN only if it is immediately followed by SUCCESSOR-PATTERN.
the SUCCESSOR-PATTERN is not captured as part of the match."
  (let ((pattern-match (apply-rule-normalized ctx pattern str pos)))
    (when pattern-match
      (let* ((match-end (match-end pattern-match))
             (successor-match
               (apply-rule-normalized ctx successor-pattern str match-end)))
        (when successor-match
          pattern-match)))))

(defun unsucceeded-by (ctx str pos pattern successor-pattern)
  "match PATTERN only if it is not followed by SUCCESSOR-PATTERN."
  (let ((pattern-match (apply-rule-normalized ctx pattern str pos)))
    (when pattern-match
      (let* ((match-end (match-end pattern-match))
             (successor-match
               (when match-end
                 (apply-rule-normalized ctx
                                        successor-pattern
                                        str
                                        match-end))))
        (unless successor-match
          pattern-match)))))

(defun upcase-char-p (ctx str pos)
  "returns 1 if the char at POS of STR is an uppercase character."
  (and (< pos (length str))
       (upper-case-p (char str pos))
       1))

(defun upcase-word-matcher (ctx str pos)
  "matches an uppercase (non-empty) word."
  (apply-rule ctx `(atleast-one-discard (upcase-char-p)) str pos))

(defun all-upto (ctx str pos delimiter-rule)
  "match all characters up to but not including the pattern defined by DELIMITER-RULE.
returns the matched substring and its bounds."
  (let ((start pos))
    (loop while (< pos (length str))
          for match = (apply-rule-normalized ctx delimiter-rule str pos)
          while (not match)
          do (incf pos))
    (when (> pos start)
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children nil))))

(defun all-upto-without (ctx str pos delimiter-rule without-rule)
  "match all characters up to but not including the pattern defined by DELIMITER-RULE unless WITHOUT-RULE is matched.
returns the matched substring and its bounds."
  (let ((start pos))
    (loop while (< pos (length str))
          for delimiter-match = (apply-rule-normalized ctx delimiter-rule str pos)
          for without-match = (apply-rule-normalized ctx without-rule str pos)
          while (not delimiter-match)
          if without-match
            do (return-from all-upto-without nil)
          else
            do (incf pos))
    (when (> pos start)
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children nil))))

;; ended up not using this, but will keep it.
(defun upto-cond (ctx str pos cond-fn)
  "match all characters until COND-FN returns t."
  (let ((start pos))
    (loop while (< pos (length str))
          for result = (funcall condition-fn ctx str pos)
          while (not result)
          do (incf pos))
    (when (> pos start)
      (make-match :begin start
                  :end pos
                  :str str
                  :ctx ctx
                  :children nil))))

(defun consec-with-optional (ctx str pos &rest parsers)
  "match a consecutive set of rules, where some are optional and some are non-optional.

parsers can be marked as optional by wrapping them in an :optional keyword.
all non-optional parsers must match in order. optional parsers are attempted,
but if they don't match, parsing continues without them."
  (let ((start pos)
        (matches))
    (loop for parser in parsers
          do (cond
               ;; if the parser is marked as optional
               ((and (consp parser)
                     (keywordp (car parser))
                     (getf parser :optional))
                (let ((match (apply-rule-normalized ctx (cadr parser) str pos)))
                  (when match
                    (setf pos (match-end match))
                    (push match matches))))
               ;; otherwise, it's a non-optional parser
               (t
                (let ((match (apply-rule-normalized ctx parser str pos)))
                  (if match
                      (progn
                        (setf pos (match-end match))
                        (push match matches))
                      (return-from consec-with-optional nil))))))
    (make-match :begin start
                :end pos
                :str str
                :ctx ctx
                :children (nreverse matches))))

(defun between-whitespace (ctx str pos rule)
  "a combinator that uses when-match to match a RULE only if the match
is surrounded by whitespace or the boundaries of the string.
The surrounding whitespace is not consumed."
  (flet ((is-preceded-by-whitespace-p (ctx str pos)
           (or (zerop pos)
               (whitespace-p (char str (1- pos)))))
         (is-succeeded-by-whitespace-p (ctx str pos)
           (or (>= pos (length str))
               (whitespace-p (char str pos)))))
    (let ((rule-with-succeeding-check
            `(followed-by ,rule ,#'is-succeeded-by-whitespace-p)))
      (apply-rule ctx
                  `(when-match
                    ,rule-with-succeeding-check
                    ,#'is-preceded-by-whitespace-p)
                  str
                  pos))))

(defun flanked-by-whitespace (ctx str pos rule)
  "a combinator that matches a RULE only if it is either preceded OR
succeeded by whitespace or a string boundary. the flanking whitespace is not consumed."
  (let ((match (apply-rule-normalized ctx rule str pos)))
    (when match
      (flet ((is-preceded-by-whitespace-p (p)
               (or (zerop p)
                   (whitespace-p (char str (1- p)))))
             (is-succeeded-by-whitespace-p (p)
               (or (>= p (length str))
                   (whitespace-p (char str p)))))
        (let* ((start-pos (match-begin match))
               (end-pos (match-end match)))
          ;; succeed if EITHER the preceding OR succeeding check is true
          (when (or (is-preceded-by-whitespace-p start-pos)
                    (is-succeeded-by-whitespace-p end-pos))
            match))))))

(defun when-match-after (ctx str pos rule condition-fn)
  "this function is used to condition matches after they occur.

for a given match, run CONDITION-FN and only if it returns true, return the match."
  (let ((match (apply-rule ctx rule str pos)))
    (when (funcall condition-fn ctx str pos rule match)
      match)))