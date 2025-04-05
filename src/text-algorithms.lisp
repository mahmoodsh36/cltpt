(in-package :cltpt)

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

(defun skip-ignore (str pos ignore)
  "skip over the IGNORE prefix starting at POS in STR.
return the first position where IGNORE is not found."
  (if (and (< pos (length str))
           (<= (+ pos (length ignore)) (length str))
           (string= ignore (subseq str pos (+ pos (length ignore)))))
      (skip-ignore str (+ pos (length ignore)) ignore)
      pos))

;; this is used to match "regions" - continguous sets of lines, for lists and tables
(defun match-region-with-ignore (str1 pos marker)
  "if the line at POS (assumed to be at the start of a line) matches the region marker,
return the total length of the contiguous region.
MARKER must have:
  - a :pattern (e.g. \"-\"),
  - optionally, an :ignore key (a literal string to skip at the start of each line).
for each line starting at POS, SKIP-IGNORE is used to bypass the ignore prefix,
then the marker pattern is checked.
if it matches, we advance to the next line.
when a line does not match (or end-of-string is reached), the region ends.
returns the total length (from the initial POS) if at least one line matches, or NIL otherwise."
  (let* ((n (length str1))
         (ignore (getf marker :ignore))
         (pattern (getf marker :pattern))
         (plen (length pattern))
         (start pos)
         (matched))
    (loop
      (if (>= pos n)
          (return (if matched (- pos start) nil)))
      (let ((line-pos (if ignore (skip-ignore str1 pos ignore) pos)))
        (if (and (< line-pos n)
                 (<= (+ line-pos plen) n)
                 (marker-match str1 n marker line-pos))
            (progn
              (setf matched t)
              ;; advance pos to the beginning of the next line.
              (let ((nl (position #\newline str1 :start pos)))
                (if nl (setf pos (1+ nl)) (setf pos n))))
            (return (if matched (- pos start) nil)))))))

;; this is used for %or which is a special kind of rule, since its provided as a list and
;; not just a string
(defun compile-marker-pattern (rule)
  "compile an alternative marker rule.
if RULE is a string, treat it as a pattern string.
if RULE is a plist, then it must have either a :pattern or a :string key."
  (cond
    ((stringp rule)
     (compile-restricted-pattern rule))
    ((getf rule :pattern)
     (compile-restricted-pattern (getf rule :pattern)))
    ((getf rule :string)
     (list (list 'literal (getf rule :string))))
    (t (error "each cell in a special rule must be a string or a plist with :pattern or :string: ~A" rule))))

(defun compile-restricted-pattern (pattern)
  "compile a restricted pattern containing literal parts and tokens.
supported tokens are:
  \"(%w)\"         — one or more alphabetic characters.
  \"(%w-)\"        — one or more letters or hyphen.
  \"(%W)\"         — one or more alphanumeric characters.
  \"(%W-)\"         — 
  \"(%C:<chars>)\" — one or more characters from the given set <chars>.
returns a list of segments, each of which is one of:
  (literal <string>)
  (word %w)
  (word-hyphen %w-)
  (word-digits %W)
  (word-digits-hyphen %W-)
  (custom-chars <allowed-string>)"
  ;; if an %or rule is requested compile each alternative recursively and return an or-segment.
  (if (and (listp pattern) (eq (car pattern) '%or))
      (list (list 'choices (mapcar #'compile-marker-pattern (cdr pattern))))
      ;; otherwise, pattern is a string, so use your original code:
      (let ((segments)
            (start 0)
            (plen (length pattern)))
        (loop while (< start plen) do
          (let* ((pos-w        (search "(%w)" pattern :start2 start :end2 plen :test #'char=))
                 (pos-w-up     (search "(%W)" pattern :start2 start :end2 plen :test #'char=))
                 (pos-w-hyphen (search "(%w-)" pattern :start2 start :end2 plen :test #'char=))
                 (pos-C        (search "(%C:" pattern :start2 start :end2 plen :test #'char=))
                 (pos-E        (search "(%E:" pattern :start2 start :end2 plen :test #'char=))
                 (pos-a        (search "(%a)" pattern :start2 start :end2 plen :test #'char=))
                 (pos-w-up-hyphen (search "(%W-)" pattern :start2 start :end2 plen :test #'char=))
                 (candidates   (remove-if-not #'identity
                                              (list (and pos-w        (cons pos-w "(%w)"))
                                                    (and pos-w-hyphen (cons pos-w-hyphen "(%w-)"))
                                                    (and pos-w-up     (cons pos-w-up "(%W)"))
                                                    (and pos-C        (cons pos-C "(%C:"))
                                                    (and pos-E        (cons pos-E "(%E:"))
                                                    (and pos-a        (cons pos-a "(%a)"))
                                                    (and pos-w-up-hyphen
                                                         (cons pos-w-up-hyphen "(%W-)")))))
                 (next (if candidates
                           (car (sort candidates
                                      (lambda (a b)
                                        (< (car a) (car b)))))
                           nil)))
            (if next
                (let* ((pos-token (car next))
                       (token     (cdr next))
                       (token-length (length token)))
                  (when (< start pos-token)
                    (push (list 'literal (subseq pattern start pos-token)) segments))
                  (cond
                    ((string= token "(%w)")
                     (push (list 'word '%w) segments)
                     (setf start (+ pos-token token-length)))
                    ((string= token "(%w-)")
                     (push (list 'word-hyphen '%w-) segments)
                     (setf start (+ pos-token token-length)))
                    ((string= token "(%W)")
                     (push (list 'word-digits '%W) segments)
                     (setf start (+ pos-token token-length)))
                    ((string= token "(%W-)")
                     (push (list 'word-digits-hyphen '%W-) segments)
                     (setf start (+ pos-token token-length)))
                    ((string= token "(%a)")
                     (push (list 'all '%a) segments)
                     (setf start (+ pos-token token-length)))
                    ((string= token "(%C:")
                     (let ((end (position #\) pattern :start (+ pos-token 4)
                                                      :end plen :test #'char=)))
                       (unless end
                         (error "unterminated custom token in pattern: ~A" pattern))
                       (let ((allowed (subseq pattern (+ pos-token 4) end)))
                         (push (list 'custom-chars allowed) segments)
                         (setf start (1+ end)))))
                    ((string= token "(%E:")
                     (let ((end (position #\) pattern :start (+ pos-token 4)
                                                      :end plen :test #'char=)))
                       (unless end
                         (error "unterminated disallowed custom token in pattern: ~A" pattern))
                       (let ((disallowed (subseq pattern (+ pos-token 4) end)))
                         (push (list 'disallowed-chars disallowed) segments)
                         (setf start (1+ end)))))))
                (progn
                  (push (list 'literal (subseq pattern start)) segments)
                  (setf start plen)))))
        (nreverse segments))))

;; may be faster than other options (such as composition of subseq and string=)
(defun substring-equal (str1 other begin end)
  (loop for i from 0 below (- end begin)
        always (char= (char str1 (+ begin i))
                      (char other i))))

;; "restricted" patterns that allow some arbitrarity but not as complex as regex - matching
;; them is alot faster
(defun match-restricted-pattern (compiled-pattern str pos)
  "attempt to match COMPILED-PATTERN at position POS in STR.
return the total length of the match if successful, or NIL otherwise."
  (let ((start pos))
    (dolist (seg compiled-pattern)
      (cond
        ((eq (first seg) 'literal)
         (let ((lit (second seg)))
           (unless (and (<= (+ pos (length lit)) (length str))
                        (substring-equal str lit pos (+ pos (length lit))))
             (return-from match-restricted-pattern nil))
           (setf pos (+ pos (length lit)))))
        ((eq (first seg) 'word)
         ;; (%w) matches one or more alphabetic characters.
         (if (or (>= pos (length str))
                 (not (alpha-char-p (char str pos))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (alpha-char-p c)
               do (incf pos)))
        ((eq (first seg) 'word-hyphen)
         ;; (%w-) matches one or more letters or hyphen or underscore
         (if (or (>= pos (length str))
                 (not (or (alpha-char-p (char str pos))
                          (char= (char str pos) #\-)
                          (char= (char str pos) #\_))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (or (alpha-char-p c)
                         (char= c #\-)
                         (char= c #\_))
               do (incf pos)))
        ((eq (first seg) 'word-digits)
         ;; (%W) matches one or more alphanumeric characters.
         (if (or (>= pos (length str))
                 (not (or (alpha-char-p (char str pos))
                          (digit-char-p (char str pos)))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (or (alpha-char-p c)
                         (digit-char-p c))
               do (incf pos)))
        ;; (%W-) matches one or more alphanumeric characters or hyphens or underscores
        ((eq (first seg) 'word-digits-hyphen)
         (if (or (>= pos (length str))
                 (not (or (alpha-char-p (char str pos))
                          (digit-char-p (char str pos))
                          (char= (char str pos) #\-)
                          (char= (char str pos) #\_)
                          (char= (char str pos) #\$))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (or (alpha-char-p c)
                         (digit-char-p (char str pos))
                         (char= c #\-)
                         (char= c #\_)
                         (char= c #\$))
               do (incf pos)))
        ;; (%a) matches "all", except newline
        ((eq (first seg) 'all)
         (if (or (>= pos (length str))
                 (char= (char str pos) #\newline))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (not (char= (char str pos) #\newline))
               do (incf pos)))
        ((eq (first seg) 'custom-chars)
         ;; (%C:<chars>) matches one or more characters from the allowed set.
         (let ((allowed (second seg)))
           (if (or (>= pos (length str))
                   (not (find (char str pos) allowed :test #'char=)))
               (return-from match-restricted-pattern nil))
           (loop while (< pos (length str))
                 for c = (char str pos)
                 while (find c allowed :test #'char=)
                 do (incf pos))))
        ((eq (first seg) 'choices)
         (let ((matched-length nil))
           (dolist (alt (second seg))
             (let ((len (match-restricted-pattern alt str pos)))
               (when len
                 (setf matched-length len)
                 (return))))
           (unless matched-length
             (return-from match-restricted-pattern nil))
           (incf pos matched-length)))
        ((eq (first seg) 'disallowed-chars)
         ;; (%E:<chars>) matches all except the chars from the disallowed set, and newlines.
         (let ((disallowed (second seg)))
           (if (or (>= pos (length str))
                   (find (char str pos) disallowed :test #'char=))
               (return-from match-restricted-pattern nil))
           (loop while (< pos (length str))
                 for c = (char str pos)
                 while (not (find c disallowed :test #'char=))
                 do (incf pos))))
        (t (error "unknown segment type in compiled pattern: ~A" (first seg)))))
    (- pos start)))

;; different "types" of rules have different ways to specify conditions, so here we are..
(defun marker-condition-key (marker)
  "return the condition key for MARKER based on its type."
  (case (getf marker :marker-type)
    (:begin   :begin-conditions)
    (:end     :end-conditions)
    (:text    :text-conditions)
    (:region  :region-conditions)
    (t nil)))

(defun apply-condition (cond-fn str pos match-str)
  "apply COND-FN to STR at POS with MATCH-STR.
if COND-FN is NIL, return T.
if it is a list, assume its car is the function and the rest are extra arguments.
if it is a symbol or a function, call it."
  (cond
    ((null cond-fn) t)
    ((listp cond-fn)
     (let ((fn (car cond-fn))
           (extra-args (cdr cond-fn)))
       (if (or (functionp fn) (symbolp fn))
           (apply (if (symbolp fn) (symbol-function fn) fn)
                  str pos match-str extra-args)
           (error "invalid condition list: ~A" cond-fn))))
    ((functionp cond-fn)
     (funcall cond-fn str pos match-str))
    ((symbolp cond-fn)
     (funcall (symbol-function cond-fn) str pos match-str))
    (t (error "invalid condition: ~A" cond-fn))))

(defun build-marker-records (rules)
  "process RULES to build marker records supporting :begin, :end, :text, and :region markers.
each marker spec is a two-element list (kind pattern) where KIND is one of
:string, :pattern, or :regex.
a rule may also include a :pair-predicate for pairing and an :id.
if the rule has an :ignore key, that value is attached to the marker record.
returns a list of marker records (plists)."
  (let (markers)
    (dolist (r rules)
      ;; if a rule has :begin and :end, and both are equal, we ensure :nestable is 'nil'
      (when (and (stringp (cadr (getf r :end)))
                 (stringp (cadr (getf r :begin)))
                 (string= (cadr (getf r :end))
                          (cadr (getf r :begin))))
        (setf (getf r :nestable) nil))
      ;; include markers from children
      (when (getf r :children)
        (dolist (record1 (build-marker-records (getf r :children)))
          (setf (getf record1 :main-parent-id)
                (getf r :id))
          ;; we shouldnt re-set :parent-id if it was set by a child
          (unless (getf record1 :parent-id)
            (setf (getf record1 :parent-id) (getf r :id)))
          (push record1 markers)))
      (dolist (key '(:begin :end :text :region))
        (let ((spec (getf r key)))
          (when spec
            (let* ((kind (first spec))
                   (pat (second spec))
                   (record (list :rule r
                                 :marker-type key
                                 :pattern pat
                                 :id (getf r :id)))
                   (cond-key (case key
                               (:begin :begin-conditions)
                               (:end   :end-conditions)
                               (:text  :text-conditions)
                               (:region :region-conditions)))
                   (to-hash-key (case key
                                  (:begin :begin-to-hash)
                                  (:end   :end-to-hash)
                                  (:text  :text-to-hash)
                                  (:region :region-to-hash))))
              (cond
                ((eq kind :string)
                 (setf record (append record (list :match-type :literal
                                                   :length (length pat)))))
                ((eq kind :pattern)
                 (setf record (append record (list :match-type :pattern
                                                   :compiled (compile-restricted-pattern pat)
                                                   :length nil))))
                ((eq kind :regex)
                 (setf record (append record (list :match-type :regex
                                                   :compiled pat
                                                   :length nil)))))
              (when (getf r cond-key)
                (setf record (append record (list cond-key (getf r cond-key)))))
              (when (getf r :ignore)
                (setf record (append record (list :ignore (getf r :ignore)))))
              (when (getf r to-hash-key)
                (setf record (append record (list :to-hash (getf r to-hash-key)))))
              ;; (when (getf r :children)
              ;;   (setf record (append record (list :children (getf r :children)))))
              (dolist (option '(:shares-end-delim :shared-begin-delim))
                (when (getf r option)
                  (setf record (append record (list option (getf r option))))))
              (push record markers))))))
    markers))

(defun build-marker-hash (markers)
  "build a hash table mapping specific characters to markers. makes lookup/matching faster.
the key is the first character of the pattern."
  (let ((ht (make-hash-table :test 'eql)))
    (dolist (marker markers)
      (when (getf marker :to-hash)
        (let ((key (char (getf marker :pattern) 0)))
          (push marker (gethash key ht)))))
    ht))

;; given a marker, test if it should be treated as a match at idx of str1
(defun marker-match (str1 str1-length marker idx)
  (let* ((ptype (getf marker :match-type))
         (pattern (getf marker :pattern))
         (cond-key (marker-condition-key marker)))
    (cond
      ((eq ptype :literal)
       (let* ((plen (getf marker :length))
              (end (+ idx plen)))
         (when (and (<= end str1-length)
                    (loop for i from 0 below plen
                          always (char= (char str1 (+ idx i))
                                        (char pattern i))))
           (when (apply-condition (getf marker cond-key) str1 idx end)
             (list :pos idx :end end
                   :match (subseq str1 idx end)
                   :marker marker)))))
      ((eq ptype :pattern)
       (let ((mlen (match-restricted-pattern (getf marker :compiled) str1 idx)))
         (when mlen
           (let ((match-str (subseq str1 idx (+ idx mlen))))
             (when (apply-condition (getf marker cond-key) str1 idx match-str)
               (list :pos idx :end (+ idx mlen)
                     :match match-str :marker marker)))))))))

;; "marker" matching (finding matching candidate locations), note
;; that these are only initial candidates and may be filtered later.
;; regions are currently not "hashed" properly.
;; the hashmap is just a lookup table that maps a specific character to the markers
;; that the text starting at the character could be used to match, this is to speed up
;; traversal and not have to apply the matching predicates for each rule/marker at each position
;; and reduce the number of operations we need to make per position.
(defun scan-all-optimized-markers (str markers marker-hash)
  "scan STR for non-regex markers in one pass.
returns a list of event plists with keys :pos, :end, :match, and :marker.
for region markers, we check at each beginning-of-line.
to allow overlapping/nested events from different rules yet avoid multiple overlapping
events from the same rule, we track active region events in a hash table."
  (let ((n (length str))
        (region-markers
          (remove-if-not
           (lambda (m)
             (eq (getf m :marker-type) :region))
           markers))
        (nonregion-nonhashed-markers
          (remove-if
           (lambda (m)
             (or (eq (getf m :marker-type) :region)
                 (getf m :to-hash)))
           markers))
        (active-regions)
        (events)
        (line-num 0))
    (dotimes (i n)
      (when (or (= i 0) (char= (elt str (1- i)) #\newline))
        ;; region markers: check at beginning-of-line.
        (dolist (marker region-markers)
          (let* ((rule-id (getf marker :id))
                 (active (assoc rule-id active-regions)))
            ;; only try matching if theres no active region overlapping.
            (unless (and active (< i (cdr active)))
              (let ((rlen (match-region-with-ignore str i marker)))
                (when rlen
                  (push (list :pos i :end (+ i rlen)
                              :match (subseq str i (+ i rlen))
                              :marker marker)
                        events)
                  (setf active-regions
                        (acons rule-id (+ i rlen)
                               (remove-if (lambda (pair)
                                            (eq (car pair) rule-id))
                                          active-regions))))))))
        ;; set :line-number which is used later to detect matches on different lines
        ;; and prune them if they're not allowed
        (incf line-num))
      ;; process literal and restricted markers.
      ;; hashed ones (according to a specific char) make our job easier/faster.
      (let ((c (char str i)))
        (dolist (marker (gethash c marker-hash))
          (let ((match-result (marker-match str n marker i)))
            (when match-result
              (setf (getf match-result :line-num) line-num)
              (push match-result events)))))
      (dolist (marker nonregion-nonhashed-markers)
        (let ((match-result (marker-match str n marker i)))
          (when match-result
            (setf (getf match-result :line-num) line-num)
            (push match-result events)))))
    (nreverse events)))

;; regex scanning fallback (only if specified by rules), this applies its own pass
;; and is really slow
(defun scan-all-regex-markers (str markers)
  "scan STR for regex markers.
returns a list of event plists with keys :pos, :end, :match, and :marker."
  (let ((events))
    (dolist (marker markers)
      (when (eq (getf marker :match-type) :regex)
        (let ((pattern (getf marker :compiled))
              (cond-key (marker-condition-key marker)))
          (dolist (m (cl-ppcre:all-matches pattern str))
            (let ((match-str (subseq str (first m) (second m))))
              (when (apply-condition (getf marker cond-key) str (first m) match-str)
                (push (list :pos (first m)
                            :end (second m)
                            :match match-str :marker marker)
                      events)))))))
    (nreverse events)))

(defun scan-all-markers (str markers)
  "scan STR for all markers.
uses a single pass for non-regex markers (via a hash table, if possible/requested)
and a separate pass for regex markers."
  (let* ((opt-markers (remove-if (lambda (m) (eq (getf m :match-type) :regex))
                                 markers))
         (regex-markers (remove-if-not (lambda (m) (eq (getf m :match-type) :regex))
                                       markers))
         (marker-hash (build-marker-hash opt-markers))
         (opt-events (scan-all-optimized-markers str markers marker-hash))
         (regex-events (scan-all-regex-markers str regex-markers)))
    (append opt-events regex-events)))

(defun find-with-rules (str rules)
  "scan STR for markers based on RULES.
RULES is a list of plists that may specify marker types:
  - :begin for begin markers,
  - :end for end markers,
  - :text for standalone text markers,
  - :region for region markers.
each marker spec is a two-element list (kind pattern), where KIND is one of
:string, :pattern, or :regex.
a rule may also include a :pair-predicate and an :id.
for paired markers the result is a list of quintuples:
  (begin-index end-index begin-match end-match rule-id).
for text and region markers the event is a list:
  (start-index end-index match rule-id)."
  (let* ((markers (build-marker-records rules))
         (events (scan-all-markers str markers))
         (sorted-events (sort events
                              (lambda (a b)
                                (if (= (getf a :pos) (getf b :pos))
                                    (< (getf a :end) (getf b :end))
                                    (< (getf a :pos) (getf b :pos))))))
         (results)
         (active-begins (make-hash-table :test 'equal)) ;; rule-id -> list of begin events
         (last-ends (make-hash-table :test 'equal)))
    (dolist (ev sorted-events)
      (let* ((ev-marker (getf ev :marker))
             (ev-type (getf ev-marker :marker-type))
             (is-nested-rule (getf ev-marker :parent-id))
             (parent-id (when is-nested-rule (getf ev-marker :parent-id)))
             (rule-id (getf ev-marker :id))
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
                     (or (< (getf ev :pos)
                            (getf last-begin-ev :end))
                         (and last-end
                              (< (getf ev :pos)
                                 last-end))
                         ;; if the marker's rule has :nestable=nil, we must ensure
                         ;; we dont open any more events until we close the currently open one
                         (and (not (getf (getf ev-marker :rule) :nestable t))
                              (equal ev-type :begin)
                              (gethash rule-id active-begins))))
            (setf allow-event nil))
          ;; was used for debugging..
          ;; (format t "marker1 ~A,~A ~A,~A ~A,~A, ~A,~A, ~A~%"
          ;;         (char str (getf ev :pos))
          ;;         (length (gethash main-parent-id active-begins))
          ;;         (null allow-event)
          ;;         ev-type
          ;;         (getf ev :pos)
          ;;         last-end parent-id
          ;;         (length (gethash parent-id active-begins))
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
                                      (getf begin-ev :pos)
                                      (getf ev :pos)
                                      (getf begin-ev :end)
                                      (getf ev :end)))
                     (push (list (getf begin-ev :pos)
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
            ((or (eq ev-type :text) (eq ev-type :region))
             (push (list (getf ev :pos)
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