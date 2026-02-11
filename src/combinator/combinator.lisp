(defpackage :cltpt/combinator
  (:use :cl :cltpt/combinator/match)
  (:import-from
   :cltpt/reader
   :reader-char :reader-string= :reader-string-equal :is-before-eof :is-after-eof :make-reader
   :is-le-eof :reader-from-string :reader-from-input
   :reader-fast-buffer :reader-fast-buffer-length
   :*reader-fast-buffer* :*reader-fast-buffer-length*)
  (:export
   :literal :literal-casein :consec :parse :word-matcher :upcase-word-matcher
   :consec-atleast-one :symbol-matcher :scan-all-rules :any :all-but
   :all-but-newline :atleast-one :atleast-one-discard :lisp-sexp :pair
   :unescaped :natural-number-matcher :when-match :at-line-start-p
   :at-line-end-p :followed-by :separated-atleast-one
   :all-but-whitespace :handle-rule-string :all-upto
   :all-upto-included :succeeded-by :all-upto-without
   :context-rules :consec-with-optional :compile-rule-string
   :between-whitespace :when-match-after :flanked-by-whitespace :flanked-by-whitespace-or-punctuation
   :context-parent-begin :context-parent-match :context-copy
   :apply-rule

   :match-id :match-begin :match-end :match-ctx :match-children
   :match-begin-absolute :match-end-absolute
   :make-match :make-match-simple :match-clone :match-rule :match-parent
   :match-set-children-parent :match-props :match :match-region
   :find-submatch :find-submatch-last :find-submatch-all

   :match-text
   :copy-rule :copy-modify-rule

   :reader-from-string :reader-from-input))

(in-package :cltpt/combinator)

;; ways to improve this code:
;; 1. we always invoke :match on the boundaries we find, this is too slow. often we might be able to get away with just storing the boundaries we found, and storing a reference of the main string, then computing :match on demand. this can be ddone perhaps using a custom structure with a special access function `get-match' built exactly for this.
;; 2. the `pair' function should be able to receive a predicate that may discard "false positives". e.g. we dont want \begin{env1} \end{env2} to be matched, which currently happens.
;; 3. the `pair' function isnt optimal, it does redundant work.
;; 4. we need a function that may discard any custom "left padding" such as a sequence of characters used for comments, e.g. ";;" in this case. in this exact comment section we have ";;" at the start of each line, we need a function that takes a "list matcher", and allows it to work regardless of those "padding sequences".
;; 5. some rules may only want to attempt matching in very specific cases, such as the beginning of a line, ending of a line. currently we support the heuristic of :on-char, and it speeds things up because it reduces the number of matchers executed at each point in the string, but we can add other heuristics too (perhaps an :after-char which for example could be used for ':after-char #\newline').
;; 6. the usage of this code is currently done on buffers after they have been loaded from the files, which results in the buffer being processed atleast twice. a better approach would be to process the string while its being loaded into memory.
;; 7. to optimize and reduce the amount of redundant matching we could generalize the :on-char heuristic to a trie-based approach that works with a sequence instead of a single char.
;; 8. avoid calling (and perhaps cache calls to) match-{begin,end}-absolute.

(declaim (inline context-parent-begin normalize-match))

;; this is used to keep track of the rules being processed, so that a matcher
;; may be aware of other matches
(defstruct context
  rules
  ;; parent-match will not have the 'end' set, only the 'begin'
  parent-match*
  ;; cache for context-parent-begin to avoid repeated tree walks
  (parent-begin-cache nil :type (or null fixnum)))

(defun context-parent-match (ctx)
  (when ctx
    (context-parent-match* ctx)))

(defun context-parent-begin (ctx)
  "return the absolute begin position of the context's parent match.
returns 0 if ctx is nil or has no parent match. result is cached."
  (if (null ctx)
      0
      (the fixnum
           (or (context-parent-begin-cache ctx)
               (let ((begin (if (context-parent-match ctx)
                                (match-begin-absolute (context-parent-match ctx))
                                0)))
                 (setf (context-parent-begin-cache ctx) begin)
                 begin)))))


(defun context-copy (ctx &optional (parent-match (context-parent-match ctx)))
  (if ctx
      (make-context :rules (context-rules ctx)
                    :parent-match* parent-match
                    :parent-begin-cache nil)
      (make-context :parent-match* parent-match)))

(defvar *escaped*)

(defun plistp (list1)
  "check whether LIST1 is a plist."
  (and (consp list1)
       (keywordp (car list1))))

;; this really helps with performance. perhaps we should find a way to make the detection of the
;; first char a rule would need to match easier and work for all rules by default. (a better idea
;; would to to generalize it to even more than one char, using some form of trie.)
(defun extract-literal-from-rule (rule)
  "extract the first character that a rule would match, if determinable.
returns the character if the rule starts with a known literal, NIL otherwise."
  (cond
    ;; nil or atom that's not a string
    ((or (null rule) (and (atom rule) (not (stringp rule))))
     nil)
    ;; string directly
    ((and (stringp rule) (plusp (length rule)))
     (char rule 0))
    ;; not a list - give up
    ((not (listp rule))
     nil)
    ;; (literal "string")
    ((and (eq (car rule) 'literal)
          (stringp (cadr rule))
          (plusp (length (cadr rule))))
     (char (cadr rule) 0))
    ;; (literal-casein "string")
    ((and (eq (car rule) 'literal-casein)
          (stringp (cadr rule))
          (plusp (length (cadr rule))))
     (char-downcase (char (cadr rule) 0)))
    ;; (consec ...) - extract from first element
    ((and (eq (car rule) 'consec) (cdr rule))
     (extract-literal-from-rule (cadr rule)))
    ;; (any ...) - can't determine (multiple possibilities)
    ((eq (car rule) 'any)
     nil)
    ;; (:pattern ... :on-char #\c) - plist with :on-char
    ((and (keywordp (car rule)) (getf rule :on-char))
     (getf rule :on-char))
    ;; (:pattern (literal ...) ...) - plist with :pattern
    ((and (keywordp (car rule)) (getf rule :pattern))
     (extract-literal-from-rule (getf rule :pattern)))
    ;; (unescaped (literal ...))
    ((and (eq (car rule) 'unescaped) (cadr rule))
     (extract-literal-from-rule (cadr rule)))
    ;; (when-match rule fn) or (followed-by rule fn)
    ((and (member (car rule) '(when-match followed-by)) (cadr rule))
     (extract-literal-from-rule (cadr rule)))
    ;; unknown pattern
    (t nil)))

(defvar *literal-char-cache* (make-hash-table :test 'eq))

(defun extract-literal-from-rule-cached (rule)
  (multiple-value-bind (val found) (gethash rule *literal-char-cache*)
    (if found val
        (setf (gethash rule *literal-char-cache*)
              (extract-literal-from-rule rule)))))

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

(defun whitespace-p (char)
  "check if a character is whitespace (space, newline, or tab)."
  (case char
    ((#\space #\newline #\tab) t)
    (otherwise nil)))

(defun is-punctuation-p (char)
  "check if a character is punctuation (check the source to see which chars are considered)."
  (case char
    ((#\. #\, #\; #\:
      #\! #\?
      #\" #\' #\`
      #\( #\)
      #\[ #\]
      #\{ #\}
      #\< #\>
      #\/ #\\
      #\|
      #\@
      #\#
      #\$
      #\%
      #\^
      #\&
      #\*
      #\- #\_
      #\+ #\=
      #\~)
     t)
    (otherwise nil)))

(defun normalize-match (match ctx rule pos)
  "ensures a match result is in the standard plist cons structure."
  (declare (type fixnum pos))
  (if (typep match 'fixnum)
      (let ((parent-begin (context-parent-begin ctx)))
        (declare (type fixnum parent-begin match))
        (make-match :begin (- pos parent-begin)
                    :end (- (+ pos match) parent-begin)
                    :ctx ctx
                    :rule rule
                    :parent (context-parent-match ctx)
                    :children nil))
      match))

(defun any (ctx reader pos &rest all)
  "match any of the given combinator rules."
  (declare (type fixnum pos))
  (let ((current-char (reader-char reader pos))
        (parent-begin (context-parent-begin ctx))
        (wrapper)
        (child-ctx))
    (declare (type fixnum parent-begin))
    (loop for one in all
          for first-char = (extract-literal-from-rule-cached one)
          when (or (null first-char)
                   (null current-char)
                   (char= current-char first-char)
                   (char= (char-downcase current-char) first-char))
            do (unless wrapper
                 (setf wrapper (make-match-simple
                                (- pos parent-begin)
                                0
                                ctx
                                (context-parent-match ctx)))
                 (setf child-ctx (context-copy ctx wrapper)))
               (let ((match (apply-rule child-ctx one reader pos)))
                 (when match
                   (setf (match-children wrapper) (list match))
                   (setf (match-end wrapper)
                         (+ (match-begin wrapper) (match-end match)))
                   (match-set-children-parent wrapper)
                   (return-from any wrapper))))))

(defun literal (ctx reader pos substr)
  "match a literal string."
  (declare (ignore ctx)
           (type fixnum pos))
  (let ((sublen (length substr)))
    (declare (type fixnum sublen))
    (when (and (is-le-eof reader (+ pos sublen))
               (reader-string= reader
                               substr
                               :start1 pos
                               :end1 (+ pos sublen)
                               :end2 sublen))
      sublen)))

(defun literal-casein (ctx reader pos substr)
  "match a literal string, case-insensitive."
  (declare (ignore ctx)
           (type fixnum pos))
  (let ((sublen (length substr)))
    (declare (type fixnum sublen))
    (when (is-le-eof reader (+ pos sublen))
      (when (reader-string-equal reader
                                 substr
                                 :start1 pos
                                 :end1 (+ pos sublen)
                                 :end2 sublen)
        sublen))))

(defun eng-char-p (ctx reader pos)
  "match an english character."
  (when (alpha-char-p (reader-char reader pos))
    1))

(defun eng-alphanump (ctx reader pos)
  "match an english character or a digit."
  (let ((c (reader-char reader pos)))
    (when (or (alpha-char-p c)
              (digit-char-p c))
      1)))

(defun symbol-char (ctx reader pos)
  "helper for `symbol-matcher'."
  (when (or (eng-alphanump ctx reader pos)
            (let ((c (reader-char reader pos)))
              (or (char= c #\-)
                  (char= c #\+)
                  (char= c #\_)
                  (char= c #\*)
                  (char= c #\$))))
    1))

(defun symbol-matcher (ctx reader pos)
  "matches a 'symbol', which may include some special characters."
  (apply-rule ctx '(atleast-one-discard (symbol-char)) reader pos))

(defun all-but (ctx reader pos exceptions)
  "match everything but the characters in EXCEPTIONS."
  (declare (ignore ctx)
           (type fixnum pos))
  (let ((start pos))
    (declare (type fixnum start))
    (if exceptions
        (loop while (is-before-eof reader pos)
              for c of-type character = (reader-char reader pos)
              while (not (find c exceptions :test #'char=))
              do (incf pos))
        ;; if no exceptions, match everything until EOF
        (loop while (is-before-eof reader pos)
              do (incf pos)))
    (when (> pos start)
      (- pos start))))

(defun only (ctx reader pos allowed)
  (let ((start pos))
    (when (or (not (is-before-eof reader pos))
              (not (find (reader-char reader pos) allowed :test #'char=)))
      (return-from only nil))
    (loop while (is-before-eof reader pos)
          for c = (reader-char reader pos)
          while (find c allowed :test #'char=)
          do (incf pos))
    (- pos start)))

(defvar *newline-string* (string #\newline))
(defvar *whitespace-string* (concatenate 'string " " (string #\newline)))

(defun all-but-newline (ctx reader pos)
  "match all characters but a newline."
  (all-but ctx reader pos *newline-string*))

;; we should add more chars that quality as whitespace
(defun all-but-whitespace (ctx reader pos)
  "match all characters but whitespaces."
  (all-but ctx reader pos *whitespace-string*))

(defun consec (ctx reader pos &rest all)
  "match a consecutive set of rules, each one has to be present."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (children)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (loop for one in all
          for m = (apply-rule child-ctx one reader pos)
          for len fixnum = (if m
                               (- (match-end m) (match-begin m))
                               0)
          do (if (and m (plusp len))
                 (progn
                   (setf pos (+ child-parent-begin (match-end m)))
                   (push m children))
                 (return-from consec nil)))
    (setf (match-end match) (- pos parent-begin))
    (setf (match-children match) (nreverse children))
    (when children
      (match-set-children-parent match))))

(defun consec-atleast-one (ctx reader pos &rest all)
  "match a consecutive set of rules, atleast the first has to be present.

the matcher stops once it encounters a rule that hasnt been matched, and returns
the consecutive matches up to that point."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (children)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (loop for one in all
          for m = (apply-rule child-ctx one reader pos)
          for len fixnum = (if m
                               (- (match-end m) (match-begin m))
                               0)
          do (if (and m (plusp len))
                 (progn
                   (setf pos (+ child-parent-begin (match-end m)))
                   (push m children))
                 (return)))
    (setf (match-end match) (- pos parent-begin))
    (setf (match-children match) (nreverse children))
    (when children
      (match-set-children-parent match))))

;; a consecutive set of matchers, separated by a specific matcher. atleast one
(defun separated-atleast-one (ctx reader pos sep-matcher matcher)
  "apply the sequence of matcher `MATCHER' separated by `SEP-MATCHER'."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (matches)
         (first t)
         (sep-match)
         (pos-after-sep pos)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start pos-after-sep child-parent-begin))
    (loop
      do (unless first
           (let ((m (apply-rule child-ctx sep-matcher reader pos)))
             (unless m
               (return))
             (setf pos-after-sep (+ child-parent-begin (match-end m)))
             (setf sep-match m)))
         (let ((m (apply-rule child-ctx matcher reader pos-after-sep)))
           (unless m
             (return))
           (setf pos (+ child-parent-begin (match-end m)))
           (unless first
             (push sep-match matches))
           (push m matches))
         (setf first nil))
    (setf (match-end match) (- pos parent-begin))
    (setf (match-children match) (nreverse matches))
    (when matches
      (match-set-children-parent match))))

(defun atleast-one (ctx reader pos matcher)
  "match the rule MATCHER atleast once."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (matches)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (loop while (is-before-eof reader pos)
          for m = (apply-rule child-ctx matcher reader pos)
          while m
          for len fixnum = (- (match-end m) (match-begin m))
          do (if (plusp len)
                 (progn
                   (setf pos (+ child-parent-begin (match-end m)))
                   (push m matches))
                 (return)))
    (when matches
      (setf (match-end match) (- pos parent-begin))
      (setf (match-children match) (nreverse matches))
      (match-set-children-parent match))))

(defun atleast-one-discard (ctx reader pos matcher)
  "like `atleast-one', but doesnt collect submatches. better performance."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (loop while (is-before-eof reader pos)
          for m = (apply-rule child-ctx matcher reader pos)
          while m
          for len fixnum = (- (match-end m) (match-begin m))
          do (if (plusp len)
                 (setf pos (+ child-parent-begin (match-end m)))
                 (return)))
    (when (> pos start)
      (setf (match-end match) (- pos parent-begin))
      (setf (match-children match) nil)
      match)))

(defun digit-p (ctx reader pos)
  "returns 1 if char at POS of STR is a digit, NIL otherwise."
  (and (is-before-eof reader pos)
       (digit-char-p (reader-char reader pos))
       1))

(defun simple-wrapper (ctx reader pos rule)
  "apply a simple parsing rule with the proper wrapping of context and return the correct wrapped match."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (wrapper (make-match-simple (- pos parent-begin)
                                     0
                                     ctx
                                     (context-parent-match ctx)))
         (child-ctx (context-copy ctx wrapper))
         (child (apply-rule child-ctx rule reader pos)))
    (declare (type fixnum parent-begin))
    (when child
      (setf (match-rule wrapper) rule)
      (setf (match-end wrapper) (+ (match-begin wrapper) (match-end child)))
      (setf (match-children wrapper) (list child))
      (match-set-children-parent wrapper)
      wrapper)))

;; shouldnt we be matching words from any language?
(defun word-matcher (ctx reader pos)
  "match an english word."
  (simple-wrapper ctx reader pos '(atleast-one-discard (eng-char-p))))

(defun natural-number-matcher (ctx reader pos)
  "matches a natural number."
  (simple-wrapper ctx reader pos '(atleast-one-discard (digit-p))))

(defun pair (ctx reader pos opening-rule closing-rule
             &optional rules-for-content pair-id (allow-multiline t))
  "matches an opening-rule, then content parsed by rules-for-content, then a closing-rule.
handles nesting of the same pair structure.
if ALLOW-MULTILINE is NIL, the match will fail if a newline is encountered
before the final closing rule is found."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (parent-match (make-match-simple (- start parent-begin)
                                          0
                                          ctx
                                          (context-parent-match ctx)))
         (child-ctx (context-copy ctx parent-match))
         (child-parent-begin (context-parent-begin child-ctx))
         (opening-match (apply-rule child-ctx opening-rule reader pos))
         (closing-first-char (extract-literal-from-rule-cached closing-rule))
         (opening-first-char (extract-literal-from-rule-cached opening-rule)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (when opening-match
      (let ((current-search-pos (+ child-parent-begin (match-end opening-match)))
            ;; the nesting-level thing is for nesting the rule unless the rule is in rules-for-content
            (nesting-level 1)
            (final-closing-match)
            ;; collect content matches as we go
            (content-matches))
        (declare (type fixnum current-search-pos nesting-level))
        (loop while (is-before-eof reader current-search-pos)
              do
                 (let ((current-char (reader-char reader current-search-pos)))
                   ;; if multiline is not allowed and we are at the base nesting level,
                   ;; fail the match if we encounter a newline character.
                   (when (and (not allow-multiline)
                              (= nesting-level 1)
                              (char= current-char #\newline))
                     (return-from pair nil))
                   ;; only try closing rule if first char could match (or we don't know the first char)
                   (let ((potential-close
                           (when (or (null closing-first-char)
                                     (char= current-char closing-first-char)
                                     (char= (char-downcase current-char) closing-first-char))
                             (apply-rule child-ctx
                                         closing-rule
                                         reader
                                         current-search-pos))))
                     (if potential-close
                         (progn
                           (decf nesting-level)
                           (setf current-search-pos
                                 (+ child-parent-begin
                                    (match-end potential-close)))
                           (when (= nesting-level 0)
                             (setf final-closing-match potential-close)
                             (return)))
                         ;; try to match content rules before checking for nested opens,
                         ;; this allows nested blocks to be matched as children
                         (let ((matched-content))
                           (when rules-for-content
                             (loop for rule in rules-for-content
                                   until matched-content
                                   do (let ((match-result (apply-rule child-ctx rule reader current-search-pos)))
                                        (when match-result
                                          (setf current-search-pos
                                                (+ child-parent-begin
                                                   (match-end match-result)))
                                          (push match-result content-matches)
                                          (setf matched-content t)))))
                           (unless matched-content
                             ;; if no content matched, check for nested opens.
                             ;; only try if first char could match.
                             (let ((potential-open
                                     (when (or (null opening-first-char)
                                               (char= current-char opening-first-char)
                                               (char= (char-downcase current-char) opening-first-char))
                                       (apply-rule child-ctx
                                                   opening-rule
                                                   reader
                                                   current-search-pos))))
                               (if potential-open
                                   (progn
                                     (incf nesting-level)
                                     (setf current-search-pos
                                           (+ child-parent-begin (match-end potential-open))))
                                   (incf current-search-pos)))))))))
        (when (and final-closing-match (= nesting-level 0))
          (let ((overall-end-pos (+ child-parent-begin
                                    (match-end final-closing-match))))
            (declare (type fixnum overall-end-pos))
            (setf (match-end parent-match) (- overall-end-pos parent-begin))
            (setf (match-children parent-match)
                  (append (list opening-match)
                          (nreverse content-matches)
                          (list final-closing-match)))
            (when pair-id
              (setf (match-id parent-match) pair-id))
            (match-set-children-parent parent-match)))))))

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
(defun word-digits-hyphen (ctx reader pos)
  "matches a (non-empty) word with hyphens, underscores or dollar signs."
  (let ((start pos))
    (if (or (is-after-eof reader pos)
            (let ((c (reader-char reader pos)))
              (not (or (alpha-char-p c)
                       (digit-char-p c)
                       (char= c #\-)
                       (char= c #\_)
                       (char= c #\$)))))
        (return-from word-digits-hyphen nil)
        (loop while (is-before-eof reader pos)
              for c = (reader-char reader pos)
              while (or (alpha-char-p c)
                        (digit-char-p c)
                        (char= c #\-)
                        (char= c #\_)
                        (char= c #\$))
              do (incf pos)))
    (- pos start)))

(defun word-digits (ctx reader pos)
  "matches a (non-empty) word (possibly) with digits."
  (let ((start pos))
    (if (or (is-after-eof reader pos)
            (let ((c (reader-char reader pos)))
              (not (or (alpha-char-p c)
                       (digit-char-p c)))))
        (return-from word-digits nil)
        (loop while (is-before-eof reader pos)
              for c = (reader-char reader pos)
              while (or (alpha-char-p c)
                        (digit-char-p c))
              do (incf pos)))
    (- pos start)))

;; TODO: why dont we just make the 'reader' class implement the gray stream interface?
(defun reader-read-sexp (reader pos)
  "read a lisp sexp from READER starting at POS using a gray stream wrapper.
returns (values success-p end-pos) on success, or (values nil nil) on failure."
  ;; ensure we have data at pos
  (unless (cltpt/reader:is-before-eof reader pos)
    (return-from reader-read-sexp (values nil nil)))
  (let ((stream (make-instance 'cltpt/reader:reader-input-stream
                               :reader reader
                               :index pos)))
    (handler-case
        (progn
          (read-preserving-whitespace stream)
          (values t (cltpt/reader:stream-index stream)))
      (end-of-file ()
        (values nil nil))
      (error ()
        (values nil nil)))))

(defun lisp-sexp (ctx reader pos)
  "reads a single lisp S-expression from the current position."
  (when (or (is-after-eof reader pos)
            (whitespace-p (reader-char reader pos)))
    (return-from lisp-sexp nil))
  (multiple-value-bind (success end-pos)
      (reader-read-sexp reader pos)
    (when success
      ;; find the last non-whitespace position for tight bounds
      (let ((last-char-pos
              (loop for i from (1- end-pos) downto pos
                    when (not (whitespace-p (reader-char reader i)))
                      return i)))
        (when last-char-pos
          (let ((parent-begin (context-parent-begin ctx)))
            (declare (type fixnum parent-begin))
            (make-match :begin (- pos parent-begin)
                        :end (- (1+ last-char-pos) parent-begin)
                        :ctx ctx
                        :id 'lisp-form-content
                        :children nil)))))))

(defun apply-rule (ctx rule reader pos)
  "returns a 'raw' match: a number (length) for simple successful matches,
or a pre-formed plist cons cell for combinators/structured matches, or NIL."
  (declare (type fixnum pos))
  (let ((result
          (cond
            ;; if its a single symbol we resolve the rule from the symbol's value
            ((symbolp rule)
             (let ((resolved (symbol-value rule)))
               (if resolved
                   (apply-rule ctx resolved reader pos)
                   (error "invalid rule: ~A" rule))))
            ;; plist rules (keywordp car), check before function call rules (next one), which use
            ;; 'fboundp' and keywordp is cheaper.
            ((and (listp rule) (keywordp (car rule)))
             (let ((pattern (getf rule :pattern))
                   (on-char (getf rule :on-char))
                   (id (getf rule :id)))
               (when pattern
                 (unless (and on-char
                              (is-before-eof reader pos)
                              (not (char= (reader-char reader pos) on-char)))
                   (let ((sub-pattern-match (apply-rule ctx pattern reader pos)))
                     (when sub-pattern-match
                       (setf (match-id sub-pattern-match) id)
                       sub-pattern-match))))))
            ;; function call rules
            ((and (listp rule) (symbolp (car rule)) (fboundp (car rule)))
             (apply (car rule) ctx reader pos (cdr rule)))
            ((stringp rule)
             (literal ctx reader pos rule))
            (t (error "invalid rule: ~A" rule)))))
    (when result
      (setf result (normalize-match result ctx rule pos))
      (setf (match-rule result) rule)
      result)))

;; the hash table thing is a heuristic that makes things slightly faster
(defun hash-rules (rules)
  "construct a hash table by :on-char of the list of plists RULES."
  (let ((hash (make-hash-table :test 'eq)))
    (loop for rule in rules
          do (when (and (consp rule) (keywordp (car rule)))
               (let ((on-char (getf rule :on-char)))
                 (when on-char
                   (push rule (gethash on-char hash))))))
    hash))

(defun scan-all-rules (ctx input rules &optional (start-idx 0) end-idx)
  "iterate through STR, apply each matcher of RULES repeatedly at each position."
  (let* ((reader (reader-from-input input))
         (events)
         (*escaped*)
         ;; bind fast-buffer for reader-char/is-before-eof fast path
         (*reader-fast-buffer* (reader-fast-buffer reader))
         (*reader-fast-buffer-length* (reader-fast-buffer-length reader))
         (i start-idx)
         (rule-hash (hash-rules rules))
         ;; hash rules by :on-char to speed up iteration, we only call hashed
         ;; rules when we see the :on-char they need. this way we avoid
         ;; trying all rules at all locations
         (hashed-rules (loop for value being the hash-values of rule-hash
                             append value))
         (unhashed-rules (set-difference rules hashed-rules :test #'eq))
         (ctx (or ctx (make-context :rules rules :parent-match* nil))))
    (let ((ctx-parent-begin (context-parent-begin ctx)))
      (declare (type fixnum i ctx-parent-begin))
      (loop while (if end-idx
                      (< i end-idx)
                      (is-before-eof reader i))
            do (let ((matched)
                     (current-char (reader-char reader i)))
                 (labels ((handle-rule (rule)
                            (let ((match-result (apply-rule ctx rule reader i)))
                              (when match-result
                                (setf i (+ ctx-parent-begin (match-end match-result)))
                                (push (match-set-children-parent match-result) events)
                                (setf matched t)))))
                   (loop for rule in (union unhashed-rules
                                            (gethash current-char rule-hash)
                                            :test #'eq)
                         until matched
                         do (handle-rule rule)
                         finally (unless matched
                                   (incf i))))))
      (values (nreverse events) (nreverse *escaped*)))))

(defun parse (input rules)
  (scan-all-rules nil input rules 0))

(defun is-preceded-by-odd-escape-p (ctx reader pos escape-char)
  "checks if the character at pos is preceded by an odd number of
contiguous escape-char characters."
  (declare (ignore ctx)
           (type fixnum pos)
           (type character escape-char))
  (if (zerop pos)
      nil
      (let ((i (1- pos))
            (escape-count 0))
        (declare (type fixnum i escape-count))
        (loop while (and (>= i 0)
                         (let ((c (reader-char reader i)))
                           (and c (char= c escape-char))))
              do (incf escape-count)
                 (decf i))
        (oddp escape-count))))

(defun unescaped (ctx reader pos rule &optional (escape-char #\\))
  "a combinator that fails if the current position is escaped.
if not escaped, it attempts to match the given 'rule'.
an optional escape-char can be provided, defaulting to backslash."
  ;; check if the current position is escaped by an odd number of escape chars.
  (if (is-preceded-by-odd-escape-p ctx reader pos escape-char)
      (when (apply-rule ctx rule reader pos)
        ;; TODO: isnt this hacky? it only makes sense tho :(
        (let ((last-escaped (car *escaped*)))
          (unless (and last-escaped (equal (match-begin-absolute last-escaped) (1- pos)))
            (push (make-match :begin (1- pos)
                              :end (1+ pos)
                              :props (list :replace (string (reader-char reader pos)))
                              :ctx ctx
                              :id 'escape
                              :children nil)
                  *escaped*)))
        nil)
      (apply-rule ctx rule reader pos)))

(defun at-line-start-p (ctx reader pos)
  "predicate to check if the current position is at the start of a line."
  (or (zerop pos)
      (char= (reader-char reader (1- pos)) #\newline)))

(defun at-line-end-p (ctx reader pos)
  "predicate to check if the current position is at the end of a line
(i.e., at the end of the string or followed by a newline)."
  (or (is-after-eof reader pos)
      (char= (reader-char reader pos) #\newline)))

(defun when-match (ctx reader pos rule condition-fn)
  "a combinator that attempts to match 'rule' only if 'condition-fn' returns true.
'condition-fn' is a function that takes str and pos and returns a boolean."
  (when (funcall condition-fn ctx reader pos)
    (apply-rule ctx rule reader pos)))

(defun followed-by (ctx reader pos rule condition-fn)
  "a combinator that succeeds only if 'rule' matches and the position
immediately after the match satisfies 'condition-fn'."
  (let ((match (apply-rule ctx rule reader pos)))
    (when match
      ;; use absolute position since condition-fn (e.g. at-line-end-p) expects absolute
      ;; positions in the reader
      (let ((end-pos (match-end-absolute match)))
        (when (funcall condition-fn ctx reader end-pos)
          match)))))

(defun succeeded-by (ctx reader pos pattern successor-pattern)
  "match PATTERN only if it is immediately followed by SUCCESSOR-PATTERN.
the SUCCESSOR-PATTERN is not captured as part of the match."
  (declare (type fixnum pos))
  (let ((pattern-match (apply-rule ctx pattern reader pos)))
    (when pattern-match
      (let* ((match-end-val (match-end pattern-match))
             (successor-match
               (apply-rule ctx
                           successor-pattern
                           reader
                           (+ match-end-val (context-parent-begin ctx)))))
        (when successor-match
          pattern-match)))))

(defun unsucceeded-by (ctx reader pos pattern successor-pattern)
  "match PATTERN only if it is not followed by SUCCESSOR-PATTERN."
  (declare (type fixnum pos))
  (let ((pattern-match (apply-rule ctx pattern reader pos)))
    (when pattern-match
      (let* ((match-end-val (match-end pattern-match))
             (successor-match
               (when match-end-val
                 (apply-rule ctx
                             successor-pattern
                             reader
                             (+ match-end-val (context-parent-begin ctx))))))
        (unless successor-match
          pattern-match)))))

(defun upcase-char-p (ctx reader pos)
  "returns 1 if the char at POS of STR is an uppercase character."
  (and (is-before-eof reader pos)
       (upper-case-p (reader-char reader pos))
       1))

(defun upcase-word-matcher (ctx reader pos)
  "matches an uppercase (non-empty) word."
  (apply-rule ctx '(atleast-one-discard (upcase-char-p)) reader pos))

(defun all-upto (ctx reader pos delimiter-rule)
  "match all characters up to but not including the pattern defined by DELIMITER-RULE.
returns the matched substring and its bounds."
  (declare (type fixnum pos))
  (let ((start pos)
        (parent-begin (context-parent-begin ctx)))
    (declare (type fixnum start parent-begin))
    (loop while (is-before-eof reader pos)
          for match = (apply-rule ctx delimiter-rule reader pos)
          while (not match)
          do (incf pos))
    (when (> pos start)
      (make-match :begin (- start parent-begin)
                  :end (- pos parent-begin)
                  :ctx ctx
                  :children nil))))

(defun all-upto-without (ctx reader pos delimiter-rule without-rule)
  "match all characters up to but not including the pattern defined by DELIMITER-RULE unless WITHOUT-RULE is matched.
returns the matched substring and its bounds."
  (declare (type fixnum pos))
  (let ((start pos)
        (parent-begin (context-parent-begin ctx)))
    (declare (type fixnum start parent-begin))
    (loop while (is-before-eof reader pos)
          do (when (apply-rule ctx delimiter-rule reader pos)
               (return))
             (if (apply-rule ctx without-rule reader pos)
                 (return-from all-upto-without nil)
                 (incf pos)))
    (when (> pos start)
      (make-match :begin (- start parent-begin)
                  :end (- pos parent-begin)
                  :ctx ctx
                  :children nil))))

;; ended up not using this, but will keep it.
(defun upto-cond (ctx reader pos cond-fn)
  "match all characters until COND-FN returns t."
  (declare (type fixnum pos))
  (let ((start pos)
        (parent-begin (context-parent-begin ctx)))
    (declare (type fixnum start parent-begin))
    (loop while (is-before-eof reader pos)
          for result = (funcall cond-fn ctx reader pos)
          while (not result)
          do (incf pos))
    (when (> pos start)
      (make-match :begin (- start parent-begin)
                  :end (- pos parent-begin)
                  :ctx ctx
                  :children nil))))

(defun consec-with-optional (ctx reader pos &rest parsers)
  "match a consecutive set of rules, where some are optional and some are non-optional.

parsers can be marked as optional by wrapping them in an :optional keyword.
all non-optional parsers must match in order. optional parsers are attempted,
but if they don't match, parsing continues without them."
  (declare (type fixnum pos))
  (let* ((parent-begin (context-parent-begin ctx))
         (start pos)
         (matches)
         (match (make-match-simple (- start parent-begin)
                                   0
                                   ctx
                                   (context-parent-match ctx)))
         (child-ctx (context-copy ctx match))
         (child-parent-begin (context-parent-begin child-ctx)))
    (declare (type fixnum parent-begin start child-parent-begin))
    (loop for parser in parsers
          do (cond
               ;; if the parser is marked as optional
               ((and (consp parser)
                     (keywordp (car parser))
                     (getf parser :optional))
                (let ((m (apply-rule child-ctx (cadr parser) reader pos)))
                  (when m
                    (setf pos (+ child-parent-begin (match-end m)))
                    (push m matches))))
               ;; otherwise, it's a non-optional parser
               (t
                (let ((m (apply-rule child-ctx parser reader pos)))
                  (if m
                      (progn
                        (setf pos (+ child-parent-begin (match-end m)))
                        (push m matches))
                      (return-from consec-with-optional nil))))))
    (setf (match-end match) (- pos parent-begin))
    (setf (match-children match) (nreverse matches))
    (when matches
      (match-set-children-parent match))))

(defun between-whitespace (ctx reader pos rule)
  "a combinator that uses when-match to match a RULE only if the match is surrounded by whitespace or the boundaries of the string."
  (declare (type fixnum pos))
  (when (or (zerop pos)
            (whitespace-p (reader-char reader (1- pos))))
    (let ((match (apply-rule ctx rule reader pos)))
      (when match
        (let ((end-pos (+ (context-parent-begin ctx) (match-end match))))
          (declare (type fixnum end-pos))
          (when (or (is-after-eof reader end-pos)
                    (whitespace-p (reader-char reader end-pos)))
            match))))))

(defun flanked-by-whitespace (ctx reader pos rule)
  "a combinator that matches a RULE only if it is either preceded OR
succeeded by whitespace or a string boundary. the flanking whitespace is not consumed."
  (declare (type fixnum pos))
  (let ((match (apply-rule ctx rule reader pos)))
    (when match
      (flet ((is-preceded-by-whitespace-p (p)
               (declare (type fixnum p))
               (or (zerop p)
                   (whitespace-p (reader-char reader (1- p)))))
            (is-succeeded-by-whitespace-p (p)
               (declare (type fixnum p))
               (or (is-after-eof reader p)
                   (whitespace-p (reader-char reader p)))))
        (let* ((parent-begin (context-parent-begin ctx))
               (start-pos (+ parent-begin (match-begin match)))
               (end-pos (+ parent-begin (match-end match))))
          (declare (type fixnum parent-begin start-pos end-pos))
          (when (or (is-preceded-by-whitespace-p start-pos)
                    (is-succeeded-by-whitespace-p end-pos))
            match))))))

(defun flanked-by-whitespace-or-punctuation (ctx reader pos rule)
  "a combinator that matches a RULE only if it is either preceded OR
succeeded by whitespace, punctuation, or a string boundary. the flanking characters are not consumed."
  (declare (type fixnum pos))
  (let ((match (apply-rule ctx rule reader pos)))
    (when match
      (flet ((is-preceded-by-whitespace-or-punctuation-p (p)
               (declare (type fixnum p))
               (or (zerop p)
                   (whitespace-p (reader-char reader (1- p)))
                   (is-punctuation-p (reader-char reader (1- p)))))
             (is-succeeded-by-whitespace-or-punctuation-p (p)
               (declare (type fixnum p))
               (or (is-after-eof reader p)
                   (whitespace-p (reader-char reader p))
                   (is-punctuation-p (reader-char reader p)))))
        (let* ((parent-begin (context-parent-begin ctx))
               (start-pos (+ parent-begin (match-begin match)))
               (end-pos (+ parent-begin (match-end match))))
          (declare (type fixnum parent-begin start-pos end-pos))
          (when (or (is-preceded-by-whitespace-or-punctuation-p start-pos)
                    (is-succeeded-by-whitespace-or-punctuation-p end-pos))
            match))))))

(defun when-match-after (ctx reader pos rule condition-fn)
  "this function is used to condition matches after they occur.

for a given match, run CONDITION-FN and only if it returns true, return the match."
  (let ((match (apply-rule ctx rule reader pos)))
    (when (and match (funcall condition-fn ctx reader pos rule match))
      match)))