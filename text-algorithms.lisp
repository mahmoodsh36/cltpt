(in-package :cltpt)

;; code for the 'line-region method for regions of consecutive lines sharing a
;; common pattern.
;; A
(defun find-line-regions-matching-regex (text patterns &optional ids)
  "finds contiguous regions of lines where each line matches the given regex.
for each pattern in PATTERNS (using the corresponding identifier from IDS, if provided),
this function returns a list of regions as (start-pos end-pos region-text id).
START-POS is the offset of the first line in the region and END-POS is the offset
immediately after the last matching line, while REGION-TEXT is the concatenated
text of all lines in the region."
  (let ((matches)
        (lines (str:split (string #\newline) text)))
    (dotimes (i (length patterns))
      (let ((line-pos 0)
            (pattern (nth i patterns))
            (id (if ids (nth i ids) pattern))
            (region-start-pos)
            (region-lines))
        (loop for j from 0 below (length lines)
              for line = (nth j lines)
              do (if (cl-ppcre:scan pattern line)
                     (progn
                       (unless region-start-pos
                         (setf region-start-pos line-pos))
                       (push line region-lines))
                     (when region-lines
                       (let ((region-text (format nil "~{~a~^~%~}" (nreverse region-lines))))
                         (push (list region-start-pos
                                     (+ region-start-pos (length region-text))
                                     region-text
                                     id)
                               matches))
                       (setf region-lines nil)
                       (setf region-start-pos nil)))
                 (incf line-pos (if (< j (1- (length lines)))
                                    (1+ (length line))
                                    (length line))))
        (when region-lines
          (let ((region-text (format nil "~{~a~^~%~}" (nreverse region-lines))))
            (push (list region-start-pos
                        (+ region-start-pos (length region-text))
                        region-text
                        id)
                  matches)))))
    (nreverse matches)))

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
(defun match-region-with-ignore (str pos marker)
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
  (let* ((n (length str))
         (ignore (getf marker :ignore))
         (pattern (getf marker :pattern))
         (plen (length pattern))
         (start pos)
         (matched))
    (loop
      (if (>= pos n)
          (return (if matched (- pos start) nil)))
      (let ((line-pos (if ignore (skip-ignore str pos ignore) pos)))
        (if (and (< line-pos n)
                 (<= (+ line-pos plen) n)
                 (string= pattern (subseq str line-pos (+ line-pos plen))))
            (progn
              (setf matched t)
              ;; advance pos to the beginning of the next line.
              (let ((nl (position #\newline str :start pos)))
                (if nl (setf pos (1+ nl)) (setf pos n))))
            (return (if matched (- pos start) nil)))))))

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
  (let ((segments)
        (start 0)
        (plen (length pattern)))
    (loop while (< start plen) do
      (let* ((pos-w        (search "(%w)" pattern :start2 start :end2 plen :test #'char=))
             (pos-w-up     (search "(%W)" pattern :start2 start :end2 plen :test #'char=))
             (pos-w-hyphen (search "(%w-)" pattern :start2 start :end2 plen :test #'char=))
             (pos-C        (search "(%C:" pattern :start2 start :end2 plen :test #'char=))
             (pos-w-up-hyphen (search "(%W-)" pattern :start2 start :end2 plen :test #'char=))
             (candidates   (remove-if-not #'identity
                                          (list (and pos-w       (cons pos-w "(%w)"))
                                                (and pos-w-hyphen (cons pos-w-hyphen "(%w-)"))
                                                (and pos-w-up    (cons pos-w-up "(%W)"))
                                                (and pos-C       (cons pos-C "(%C:"))
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
                ((string= token "(%C:")
                 (let ((end (position #\) pattern :start (+ pos-token 4)
                                                  :end plen :test #'char=)))
                   (unless end
                     (error "unterminated custom token in pattern: ~A" pattern))
                   (let ((allowed (subseq pattern (+ pos-token 4) end)))
                     (push (list 'custom-chars allowed) segments)
                     (setf start (1+ end)))))))
            (progn
              (push (list 'literal (subseq pattern start)) segments)
              (setf start plen)))))
    (nreverse segments)))

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
                        (string= lit (subseq str pos (+ pos (length lit)))))
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
         ;; (%w-) matches one or more letters or hyphen.
         (if (or (>= pos (length str))
                 (not (or (alpha-char-p (char str pos))
                          (char= (char str pos) #\-))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (or (alpha-char-p c)
                         (char= c #\-))
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
         ;; (%W-) matches one or more alphanumeric characters or hyphens.
        ((eq (first seg) 'word-digits-hyphen)
         (if (or (>= pos (length str))
                 (not (or (alpha-char-p (char str pos))
                          (digit-char-p (char str pos))
                          (char= (char str pos) #\-))))
             (return-from match-restricted-pattern nil))
         (loop while (< pos (length str))
               for c = (char str pos)
               while (or (alpha-char-p c)
                         (digit-char-p (char str pos))
                         (char= c #\-))
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

;; "marker" record building (finding matching candidate locations), note
;; that these are only initial candidates and may be filtered later
(defun build-marker-records (rules)
  "process RULES to build marker records supporting :begin, :end, :text, and :region markers.
each marker spec is a two-element list (kind pattern) where KIND is one of
:string, :pattern, or :regex.
a rule may also include a :predicate for pairing and an :id.
if the rule has an :ignore key, that value is attached to the marker record.
returns a list of marker records (plists)."
  (let (markers)
    (dolist (r rules)
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
                               (:region :region-conditions))))
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
              (push record markers))))))
    markers))

;; hash table & optimized scanning for non-regex markers
(defun marker-first-char (marker)
  "return the first character used for hashing from MARKER.
for literal and restricted markers, use the first character of the pattern string."
  (char (getf marker :pattern) 0))

(defun build-marker-hash (markers)
  "build a hash table mapping marker keys to marker records for non-regex markers.
for region markers, the key is the special keyword :region.
for others, the key is the first character of their pattern."
  (let ((ht (make-hash-table :test 'eql)))
    (dolist (marker markers)
      (when (not (eq (getf marker :match-type) :regex))
        (let ((key (if (eq (getf marker :marker-type) :region)
                       :region
                       (char (getf marker :pattern) 0))))
          (push marker (gethash key ht)))))
    ht))

;; this handles markers that werent matched by regexes - i.e. :pattern or :string
(defun scan-all-optimized-markers (str marker-hash)
  "scan STR for non-regex markers using MARKER-HASH in one pass.
returns a list of event plists with keys :pos, :end, :match, and :marker.
for literal and pattern markers, matching is done by a hash lookup by first character.
for region markers, we check at each beginning-of-line.
to allow overlapping/nested events from different rules yet avoid multiple overlapping
events from the same rule, we track active region events in a hash table."
  (let ((events)
        (n (length str))
        (active-regions (make-hash-table :test 'equal)))
    (dotimes (i n)
      ;; region markers: check at beginning-of-line.
      (when (or (= i 0) (char= (elt str (1- i)) #\newline))
        (when (gethash :region marker-hash)
          (dolist (marker (gethash :region marker-hash))
            (let* ((rule-id (getf marker :id))
                   (active-end (gethash rule-id active-regions)))
              (unless (and active-end (< i active-end))
                (let ((rlen (match-region-with-ignore str i marker)))
                  (when rlen
                    (push (list :pos i :end (+ i rlen)
                                :match (subseq str i (+ i rlen))
                                :marker marker)
                          events)
                    (setf (gethash rule-id active-regions) (+ i rlen)))))))))
      ;; process literal and pattern markers.
      (let ((c (elt str i)))
        (when (gethash c marker-hash)
          (dolist (marker (gethash c marker-hash))
            (unless (eq (getf marker :marker-type) :region)
              (let* ((ptype (getf marker :match-type))
                     (pattern (getf marker :pattern))
                     (cond-key (marker-condition-key marker)))
                (cond
                  ((eq ptype :literal)
                   (let ((plen (getf marker :length)))
                     (when (and (<= (+ i plen) n)
                                (string= pattern (subseq str i (+ i plen))))
                       (let ((match-str (subseq str i (+ i plen))))
                         (when (apply-condition (getf marker cond-key) str i match-str)
                           (push (list :pos i :end (+ i plen)
                                       :match match-str :marker marker)
                                 events))))))
                  ((eq ptype :pattern)
                   (let ((mlen (match-restricted-pattern (getf marker :compiled) str i)))
                     (when mlen
                       (let ((match-str (subseq str i (+ i mlen))))
                         (when (apply-condition (getf marker cond-key) str i match-str)
                           (push (list :pos i :end (+ i mlen)
                                       :match match-str :marker marker)
                                 events)))))))))))))
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
uses an optimized single pass for non-regex markers (via a hash table)
and a separate pass for regex markers."
  (let* ((opt-markers (remove-if (lambda (m) (eq (getf m :match-type) :regex))
                                 markers))
         (regex-markers (remove-if-not (lambda (m) (eq (getf m :match-type) :regex))
                                       markers))
         (marker-hash (build-marker-hash opt-markers))
         (opt-events (scan-all-optimized-markers str marker-hash))
         (regex-events (scan-all-regex-markers str regex-markers)))
    (append opt-events regex-events)))

;; if markers for rule A nest inside markers for rule B, they wont conflict. each rule's nested pairs are handled independently, and markers for different rules are allowed to overlap or nest, as they're processed in one pass.
(defun find-with-rules (str rules)
  "scan STR for markers based on RULES.
RULES is a list of plists that may specify marker types:
  - :begin for begin markers,
  - :end for end markers,
  - :text for standalone text markers,
  - :region for region markers.
each marker spec is a two-element list (kind pattern), where KIND is one of
:string, :pattern, or :regex.
a rule may also include a :predicate and an :id.
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
         (pairs)
         (active-begins (make-hash-table :test 'equal))) ;; rule-id -> list of begin events
    (dolist (ev sorted-events)
      (let* ((ev-marker (getf ev :marker))
             (ev-type (getf ev-marker :marker-type))
             (rule-id (getf ev-marker :id)))
        (cond
          ((eq ev-type :begin)
           ;; push this begin event onto the stack for the rule.
           (push ev (gethash rule-id active-begins)))
          ((eq ev-type :end)
           (let ((stack (gethash rule-id active-begins)))
             (when stack
               ;; remove any begin events that don't leave a gap.
               (loop while (and stack (not (< (getf (car stack) :end) (getf ev :pos))))
                     do (pop stack))
               (when stack
                 (let ((begin-ev (pop stack)))
                   (push (list (getf begin-ev :pos)
                               (getf ev :end)
                               (getf begin-ev :match)
                               (getf ev :match)
                               rule-id)
                         pairs)
                   (setf (gethash rule-id active-begins) stack))))))
          ((or (eq ev-type :text) (eq ev-type :region))
           (push (list (getf ev :pos)
                       (getf ev :end)
                       (getf ev :match)
                       rule-id)
                 pairs)))))
    (sort pairs #'< :key #'first)))

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