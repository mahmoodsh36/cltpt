(in-package :cltpt/base)

;; this is better done in-place and not as an external function, before matching
(defun begin-of-line (str pos match-str)
  "return T if POS is at the beginning of STR or immediately after a newline."
  (or (= pos 0)
      (char= (elt str (1- pos)) #\newline)))

(defun end-of-line (str pos match-str)
  "return T if the match that starts at POS with MATCH-STR ends at the end of STR
or is immediately followed by a newline."
  (let ((end (+ pos (length match-str))))
    (or (= end (length str))
        (char= (elt str end) #\newline))))

(defun any (str pos &rest all)
  (loop for one in all
        for match = (match-pattern-normalized one str pos)
        do (when match
             (return-from any
               (cons (list :begin pos
                           :end (getf (car match) :end)
                           :match (getf (car match) :match))
                     match)))))

;; some matchers may simply return the matched length, we turn them into the
;; assumed cons (parent . children)
(defun match-pattern-normalized (one str pos)
  (let ((match (match-pattern one str pos)))
    (if (numberp match)
        (cons
         (list :begin pos
               :end (+ pos match)
               :match (subseq str pos (+ pos match)))
         nil)
        match)))

(defun consec (str pos &rest all)
  (let ((start pos)
        (matches))
    (loop for one in all
          for match = (match-pattern-normalized one str pos)
          for len = (when match
                      (- (getf (car match) :end)
                         (getf (car match) :begin)))
          do (if len
                 (progn
                   (setf pos (getf (car match) :end))
                   (push match matches))
                 (return-from consec nil)))
    (cons (list :begin start
                :end pos
                :match (subseq str start pos))
          (nreverse matches))))

(defun literal-casein (str pos substr)
  (when (loop for c across substr for i from 0
              always (and (< (+ pos i) (length str))
                          (char= (char-downcase (char str (+ pos i)))
                                 (char-downcase (char substr i)))))
    (length substr)))

(defun literal (str pos substr)
  (when (loop for c across substr for i from 0
              always (and (< (+ pos i) (length str))
                          (char= (char str (+ pos i))
                                 (char substr i))))
    (length substr)))

(defun eng-char-p (str pos)
  (when (alpha-char-p (char str pos))
    1))

(defun eng-alphanump (str pos)
  (when (or (alpha-char-p (char str pos))
            (digit-char-p (char str pos)))
    1))

(defun symbol-char (str pos)
  (when (or (eng-alphanump str pos)
            (char= (char str pos) #\-)
            (char= (char str pos) #\_)
            (char= (char str pos) #\$))
    1))

(defun all-but (str pos exceptions)
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
  (all-but str pos (string #\newline)))

;; very inefficient, calls subparser many times
(defun atleast-one (str pos matcher)
  (let ((start pos))
    (if (or (>= pos (length str))
            (not (match-pattern matcher str pos)))
        (return-from atleast-one nil)
        (loop while (< pos (length str))
              while (match-pattern matcher str pos)
              for match-len = (match-pattern matcher str pos)
              do (incf pos match-len)))
    (- pos start)))

(defun symbol-matcher (str pos)
  (match-pattern '(atleast-one (symbol-char)) str pos))

(defun word-matcher (str pos)
  (match-pattern '(atleast-one (eng-char-p)) str pos))

(defun compile-pattern-string-helper (spec bindings)
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
                            (push "%" result)
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

(defun compile-pattern-string (str)
  (let ((my-replacements
          '((#\w . (word-matcher))
            (#\W . (symbol-matcher))
            (#\a . (all-but-newline)))))
    (compile-pattern-string-helper str my-replacements)))

;; atleast one
(defun word-digits-hyphen (str pos)
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

(defun skip-ignore (str pos ignore)
  "skip over the IGNORE prefix starting at POS in STR.
return the first position where IGNORE is not found."
  (if (and (< pos (length str))
           (<= (+ pos (length ignore)) (length str))
           (string= ignore (subseq str pos (+ pos (length ignore)))))
      (skip-ignore str (+ pos (length ignore)) ignore)
      pos))

;; may be faster than other options (such as composition of subseq and string=)
(defun substring-equal (str1 other begin end)
  (loop for i from 0 below (- end begin)
        always (char= (char str1 (+ begin i))
                      (char other i))))

(defun match-pattern (pattern str pos)
  "attempt to match PATTERN at position POS in STR.
return the total length of the match if successful, or NIL otherwise."
  (cond
    ;; custom parsing function was passed, invoke it
    ((and (listp pattern)
          (symbolp (car pattern))
          (fboundp (car pattern)))
     (apply (car pattern) str pos (cdr pattern)))
    ;; list of lists without that isnt supposed to be a function call?
    ;; but what if the first list returns a function to be called?
    ;; maybe we shouldnt enable this one
    ;; ((and (listp pattern) (listp (car pattern)))
    ;;  (match-pattern (cons 'consec pattern) str pos))
    ;; handle :pattern
    ((and (listp pattern) (getf pattern :pattern))
     (let ((match (match-pattern-normalized (getf pattern :pattern) str pos)))
       (when match
         (setf (getf (car match) :id) (getf pattern :id))
         match)))
    ;; a pattern to be "compiled"
    ((stringp pattern)
     (match-pattern (cons 'consec (compile-pattern-string pattern)) str pos))
    (t (error "invalid pattern: ~A" pattern))))

;; different "types" of rules have different ways to specify conditions, so here we are..
(defun marker-condition-key (marker)
  "return the condition key for MARKER based on its type."
  (case (getf marker :marker-type)
    (:begin   :begin-conditions)
    (:end     :end-conditions)
    (:text    :text-conditions)
    (t nil)))

(defun apply-condition (cond-fn str pos match-str)
  "apply COND-FN to STR at POS with MATCH-STR.
if COND-FN is NIL, return T.
if it is a list, assume its a list of functions to call.
if it is a symbol or a function, call it."
  (cond
    ((null cond-fn) t)
    ((listp cond-fn)
     (loop for fn in cond-fn
           always (funcall (if (symbolp fn) (symbol-function fn) fn)
                           str pos match-str)))
    ((functionp cond-fn)
     (funcall cond-fn str pos match-str))
    ((symbolp cond-fn)
     (funcall (symbol-function cond-fn) str pos match-str))
    (t (error "invalid condition: ~A" cond-fn))))

(defun build-marker-records (rules)
  "process RULES to build marker records supporting :begin, :end, and :text markers.
a rule may also include a :pair-predicate for pairing and an :id.
returns a list of marker records (plists)."
  (let (markers)
    (dolist (r rules)
      ;; include markers from children
      (when (getf r :children)
        (dolist (record1 (build-marker-records (getf r :children)))
          (setf (getf record1 :main-parent-id)
                (or (getf r :main-parent-id) (getf r :id)))
          ;; we shouldnt re-set :parent-id if it was set by a child
          (unless (getf record1 :parent-id)
            (setf (getf record1 :parent-id) (getf r :id)))
          (push record1 markers)))
      (dolist (key '(:begin :end :text))
        (let ((spec (getf r key)))
          (when spec
            (let* ((record (list :rule r
                                 :marker-type key
                                 :matcher spec
                                 :id (getf r :id)))
                   (cond-key (case key
                               (:begin  :begin-conditions)
                               (:end    :end-conditions)
                               (:text   :text-conditions)))
                   (to-hash-key (case key
                                  (:begin  :begin-to-hash)
                                  (:end    :end-to-hash)
                                  (:text   :text-to-hash))))
              (when (getf r cond-key)
                (setf record (append record (list cond-key (getf r cond-key)))))
              (when (getf r to-hash-key)
                (setf record (append record (list :to-hash (getf r to-hash-key)))))
              ;; (when (getf r :children)
              ;;   (setf record (append record (list :children (getf r :children)))))
              (push record markers))))))
    markers))

(defun build-marker-hash (markers)
  "build a hash table mapping specific characters to markers. makes lookup/matching faster.
the key is the first character of the pattern."
  (let ((ht (make-hash-table :test 'eql)))
    (dolist (marker markers)
      (let ((to-hash (getf marker :to-hash)))
        (when to-hash
          ;; if :to-hash isnt `t' we use it as the key, otherwise we use the first char
          ;; assuming a string was supplied.
          (let ((key (or (and (not (eq to-hash t)) to-hash)
                         (char (getf marker :matcher) 0))))
            (push marker (gethash key ht))))))
    ht))

;; given a marker, test if it should be treated as a match at idx of str1
(defun marker-match (str1 marker idx)
  (let* ((matcher (getf marker :matcher))
         (cond-key (marker-condition-key marker)))
    (let* ((result (match-pattern matcher str1 idx))
           (match (if (numberp result)
                      (subseq str1 idx (+ idx result))
                      (getf (car result) :match))))
      (when (numberp result)
        (setf result
              (cons (list :begin idx
                          :end (+ idx result)
                          :match match
                          ;; :pattern (getf marker :matcher)
                          :marker marker)
                    nil)))
      (when result
        (setf (getf (car result) :marker) marker)
        (when (apply-condition (getf marker cond-key)
                               str1
                               idx
                               match)
          result)))))

;; matches are returned as nested lists of the form (parent . children),
;; here we flatten them into a single list
(defun flatten-match (match)
  (if (or (atom match) (plistp match))
      (list match)
      (cons (car match)
            (loop for list1 in (mapcar (lambda (x) (flatten-match x)) (cdr match))
                  append list1))))

;; "marker" matching (finding matching candidate locations), note
;; that these are only initial candidates and may be filtered later.
;; regions are currently not "hashed" properly.
;; the hashmap is just a lookup table that maps a specific character to the markers
;; that the text starting at the character could be used to match, this is to speed up
;; traversal and not have to apply the matching predicates for each rule/marker at each position
;; and reduce the number of operations we need to make per position.
(defun scan-all-markers-helper (str markers marker-hash)
  "scan STR for markers in one pass.
returns a list of event plists with keys :begin, :end, :match, and :marker.
for region markers, we check at each beginning-of-line.
to allow overlapping/nested events from different rules yet avoid multiple
overlapping events from the same rule, we track active region events in
a hash table."
  (let ((n (length str))
        (nonhashed-markers
          (remove-if
           (lambda (m)
                 (getf m :to-hash))
           markers))
        (active-regions)
        (events)
        (line-num 0)
        (escaped))
    (dotimes (i n)
      (let ((c (elt str i)))
        (unless escaped
          (when (or (= i 0) (char= (elt str (1- i)) #\newline))
            ;; TODO: should apply beginning-of-line check here.
            ;; -
            ;; set line-num which is used later to detect matches on different lines
            ;; and prune them if they're not allowed
            (incf line-num))
          ;; hashed ones (according to a specific char) make our job easier/faster.
          (dolist (marker (gethash c marker-hash))
            (let ((match-result (marker-match str marker i)))
              (when match-result
                ;; TODO: we're only setting :line-num for the parent here
                (setf (getf (car match-result) :line-num) line-num)
                (loop for item in (flatten-match match-result)
                      do (push item events)))))
          (dolist (marker nonhashed-markers)
            (let ((match-result (marker-match str marker i)))
              (when match-result
                ;; TODO: we're only setting :line-num for the parent here
                (setf (getf (car match-result) :line-num) line-num)
                (loop for item in (flatten-match match-result)
                      do (push item events))))))
        (if (char= c #\\)
            (setf escaped (not escaped))
            (setf escaped nil))))
    (nreverse events)))

(defun scan-all-markers (str markers)
  "scan STR for all markers.
uses a single pass for markers (via a hash table, if possible/requested)."
  (let* ((marker-hash (build-marker-hash markers))
         (events (scan-all-markers-helper str markers marker-hash)))
    events))

(defun find-with-rules (str rules)
  "scan STR for markers based on RULES.
RULES is a list of plists that may specify marker types:
  - :begin for begin markers,
  - :end for end markers,
  - :text for standalone text markers"
  (let* ((markers (build-marker-records rules))
         (events (scan-all-markers str markers))
         (sorted-events
           (sort events
                 (lambda (a b)
                   (if (= (getf a :begin) (getf b :begin))
                       (< (getf a :end) (getf b :end))
                       (< (getf a :begin) (getf b :begin))))))
         (results)
         (active-begins (make-hash-table :test 'equal)) ;; rule-id -> list of begin events
         (last-ends (make-hash-table :test 'equal)))
    (dolist (ev sorted-events)
      ;; if :id is set then the event was a nested parser, handle it as such
      ;; this is temporary, we should unify the parser combinator with the
      ;; other "forms" of parsing
      (let* ((ev-marker (getf ev :marker))
             (ev-type (if (getf ev :id) :text (getf ev-marker :marker-type)))
             (is-nested-rule (getf ev-marker :parent-id))
             (parent-id (when is-nested-rule (getf ev-marker :parent-id)))
             (rule-id (or (getf ev-marker :id) (getf ev :id)))
             (main-parent-id (if is-nested-rule
                                 (getf ev-marker :main-parent-id)
                                 rule-id))
             ;; allow nested rules only if parent is active
             ;; todo: we need to later discard the rules if the parent gets discarded
             ;; also we need to handle it when some rule is unended inside a parent, and the
             ;; parent tries to end, because currently the parent's attempt to end would fail
             (allow-event (if is-nested-rule
                              (gethash parent-id active-begins)
                              t)))
        ;; here we handle some situations in which we dont want the event to go through
        (let ((last-begin-ev (car (gethash main-parent-id active-begins)))
              (last-end (gethash main-parent-id last-ends)))
          ;; dont allow a rule or its children to be started or ended multiple
          ;; times by the same "region" of text
          (when (and last-begin-ev
                     (or (< (getf ev :begin)
                            (getf last-begin-ev :end))
                         (and last-end
                              (< (getf ev :begin)
                                 last-end))
                         ;; if the marker's rule has :nestable=nil, we must ensure
                         ;; we dont open any more events until we close the currently open one
                         (and (not (getf (getf ev-marker :rule) :nestable t))
                              (equal ev-type :begin)
                              (gethash rule-id active-begins))))
            (setf allow-event nil))
          ;; was used for debugging..
          ;; (format t "marker1 ~A,~A ~A,~A ~A,~A, ~A,~A,  ~A  ~A~%"
          ;;         (char str (getf ev :begin))
          ;;         (length (gethash main-parent-id active-begins))
          ;;         (null allow-event)
          ;;         ev-type
          ;;         (getf ev :begin)
          ;;         last-end
          ;;         parent-id
          ;;         (length (gethash parent-id active-begins))
          ;;         main-parent-id
          ;;         rule-id)
          ;; if the parent doesnt allow this element nested inside it, we need to discard the event
          (when last-begin-ev
            (let ((allowed (getf (getf (getf last-begin-ev :marker) :rule) :allow))
                  (disallowed (getf (getf (getf last-begin-ev :marker) :rule) :disallow)))
              ;; according to this :allow=nil wouldnt work which may cause confusion
              (unless (equal rule-id (getf (getf (getf last-begin-ev :marker) :rule) :id))
                (if (eq disallowed t)
                    (setf allow-event nil)
                    (if allowed
                        (unless (eq allowed t)
                          (setf allow-child (and allow-child (member rule-id allowed))))
                        (if disallowed
                            (setf allow-child (not (member rule-id disallowed)))))))))
          ;; when we have :same-line is t, we need to discard the match if the end is on
          ;; a different line
          (let ((last-line (getf last-begin-ev :line-num))
                (this-line (getf ev :line-num)))
            (when (getf (getf (getf last-begin-ev :marker) :rule) :same-line)
              (let ((begin-idx
                      (position
                       rule-id
                       (gethash rule-id active-begins)
                       :key (lambda (entry)
                              (getf (getf (getf last-begin-ev :marker) :rule) :id))
                       :from-end t)))
                (when (and begin-idx last-line this-line (not (eq last-line this-line)))
                  (setf (gethash rule-id active-begins) nil)
                  (when (not (equal rule-id main-parent-id))
                    (dotimes (_ (1+ begin-idx))
                      (setf (gethash main-parent-id active-begins)
                            (cdr (gethash main-parent-id active-begins))))))))))
        (when allow-event
          (cond
            ((eq ev-type :begin)
             ;; push this begin event onto the stack for the rule.
             (push ev (gethash rule-id active-begins))
             (unless (equal main-parent-id rule-id)
               (push ev (gethash main-parent-id active-begins))))
            ((eq ev-type :end)
             (let ((stack (gethash rule-id active-begins)))
               (when stack
                 (let* ((begin-ev (car stack))
                        (main-stack-last-begin-ev (car (gethash main-parent-id active-begins)))
                        (pair-predicate (getf (getf ev-marker :rule) :pair-predicate))
                        (begin-ev-id (getf (getf begin-ev :marker) :id)))
                   ;; we need to call the :pair-predicate if existent before deciding
                   ;; whether to match the pair
                   (when (or (not pair-predicate)
                             (funcall pair-predicate
                                      str
                                      (getf begin-ev :begin)
                                      (getf ev :begin)
                                      (getf begin-ev :end)
                                      (getf ev :end)))
                     (push (list (getf begin-ev :begin)
                                 (getf ev :end)
                                 (getf begin-ev :match)
                                 (getf ev :match)
                                 ;; use the id of the begin marker, not `rule-id',
                                 ;; because otherwise we will get issues if multiple nested
                                 ;; rules have the same `:end'
                                 begin-ev-id)
                           results)
                     (setf (gethash main-parent-id last-ends) (getf ev :end)))
                   ;; here we pop the begin event
                   ;; this wouldnt work if the parent and child have the same ending string..
                   ;; (setf (gethash rule-id active-begins) (cdr stack))
                   ;; (unless (equal main-parent-id rule-id)
                   ;;   (setf (gethash main-parent-id active-begins)
                   ;;         (cdr (gethash main-parent-id active-begins))))
                   ;; this is currently a workaround for the above case
                   (setf (gethash begin-ev-id active-begins)
                         (cdr (gethash begin-ev-id active-begins)))
                   (unless (equal main-parent-id begin-ev-id)
                     (setf (gethash main-parent-id active-begins)
                           (cdr (gethash main-parent-id active-begins))))
                   ))))
            ((eq ev-type :text)
             (push (list (getf ev :begin)
                         (getf ev :end)
                         (getf ev :match)
                         rule-id)
                   results))))))
    (sort results #'< :key #'first)))

(defun modify-substring (str1 func main-region &optional small-region)
  "modify the string in SMALL-REGION of MAIN-REGION of string STR1. SMALL-REGION is relative
to MAIN-REGION."
  (let ((small-begin (if small-region
                         (+ (region-begin main-region) (region-begin small-region))
                         (region-begin main-region)))
        (small-end (if small-region
                       (+ (region-begin main-region) (region-end small-region))
                       (region-end main-region))))
    (concatenate 'string
                 (subseq str1 (region-begin main-region) small-begin)
                 (funcall func (subseq str1 small-begin small-end))
                 (subseq str1 small-end (region-end main-region)))))

(defun extract-modified-substring (str1 func main-region modification-region)
  "extract and modify the substring in MODIFICATION-REGION contained in MAIN-REGION of
string STR1. notice that MODIFICATION-REGION can be wider and even disjoint from MAIN-REGION."
  (let ((modification-begin (max (region-begin main-region)
                                 (region-begin modification-region)))
        (modification-end (min (region-end main-region)
                               (region-end modification-region))))
    (if (and (<= (region-begin modification-region)
                 (region-end main-region))
             (>= (region-end modification-region)
                 (region-begin main-region)))
        (concatenate 'string
                     (subseq str1 (region-begin main-region) modification-begin)
                     (funcall func (subseq str1 modification-begin modification-end))
                     (subseq str1 modification-end (region-end main-region)))
        str1)))

(defun replace-chars (s replace-table)
  "return a new string where every character in S that is a key in REPLACE-TABLE is
replaced by its associated string."
  (with-output-to-string (out)
    (loop for ch across s do
      (let ((replacement (cdr (assoc ch replace-table :test #'char=))))
        (if replacement
            (write-string replacement out)
            (write-char ch out))))))

(defun replace-chars-and-escapes (s replace-table &optional escapable-chars)
  "return a new string where chars in S are replaced via REPLACE-TABLE (alist).
handles escapes: '\\' followed by a char in ESCAPABLE-CHARS (list) drops
the '\\' and processes the char normally (replace or emit)."
  (with-output-to-string (out)
    (loop with len = (length s)
          for i from 0 below len
          do (let ((ch (aref s i)))
               (if (and escapable-chars
                        (char= ch #\\)
                        (< i (1- len))
                        (find (aref s (1+ i)) escapable-chars :test #'char=))
                   ;; handle escape
                   (let* ((next-char (aref s (1+ i)))
                          (replacement (cdr (assoc next-char replace-table :test #'char=))))
                     (princ (or replacement next-char) out)
                     (incf i))
                   ;; handle normal character
                   (let ((replacement (cdr (assoc ch replace-table :test #'char=))))
                     (princ (or replacement ch) out)))))))