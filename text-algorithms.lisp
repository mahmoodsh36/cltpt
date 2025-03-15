(in-package :cltpt)

;; code for the simplest 'regex method
;; A
(defun find-regex-multiple (text patterns &optional ids)
  (let ((matches)
        (ids (or ids patterns))
        (pos 0))
    (loop for pattern in patterns for id in ids
          do (loop while (cl-ppcre:scan pattern text :start pos)
                   do (multiple-value-bind (start end)
                          (cl-ppcre:scan pattern text :start pos)
                        (when start
                          (push (list start end (subseq text start end) id) matches)
                          (setf pos end)))))
    (nreverse matches)))

;; code for the 'line-regex method
;; A
(defun find-lines-matching-regex (text patterns &optional ids)
  "finds regex matches across lines for multiple patterns."
  (let ((matches)
        (ids (or ids patterns))
        (lines (str:split (string #\newline) text))
        (line-pos 0))
    (loop for line in lines
          do (loop for pattern in patterns for id in ids
                   do (loop for (start end) on (cl-ppcre:all-matches pattern line) by #'cddr
                            do (push (list (+ line-pos start)
                                           (+ line-pos end)
                                           (subseq line start end)
                                           id)
                                     matches)))
             (incf line-pos (1+ (length line))))
    (nreverse matches)))

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

;; code for 'pair method, this is by far the slowest of the bunch
;; A
(defun scan-events-for-spec (spec str)
  "scan STR for all occurrences of SPEC.
returns a list of events, each as (start-index end-index matched-string)."
  (destructuring-bind (type pattern) spec
    (cond
      ((eq type :regex)
       (let ((matches (cl-ppcre:all-matches pattern str)))
         ;; matches is a flat list: (start1 end1 start2 end2 …)
         (let ((events))
           (loop for (s e) on matches by #'cddr do
             (push (list s e (subseq str s e)) events))
           (nreverse events))))
      ((eq type :string)
       (let ((direct)
             (colon)
             (plen (length pattern))
             (slen (length str)))
         ;; scan for direct occurrences.
         (loop for i from 0 below slen do
           (when (and (<= (+ i plen) slen)
                      (string= pattern (subseq str i (+ i plen))))
             (push (list i (+ i plen) (subseq str i (+ i plen))) direct)))
         ;; for patterns not starting with colon, also scan for colon-prefixed occurrences.
         (unless (char= (char pattern 0) #\:)
           (let ((colon-pattern (concatenate 'string ":" pattern))
                 (cplen (1+ plen)))
             (loop for i from 0 below slen do
               (when (and (<= (+ i cplen) slen)
                          (string= colon-pattern (subseq str i (+ i cplen))))
                 (push (list i (+ i cplen) (subseq str i (+ i cplen))) colon))))
           ;; merge both lists. If events occur at the same index, choose the one with the smaller end index.
           (let ((all (append direct colon)))
             (setf all (sort all (lambda (a b)
                                   (if (= (first a) (first b))
                                       (< (second a) (second b))
                                       (< (first a) (first b))))))
             all))))
      (t nil))))

;; struct to hold per-rule pre-scanned events
(defstruct rule-info
  rule              ;; the original rule plist
  begin-events      ;; list of events for the :begin spec
  end-events        ;; list of events for the :end spec
  begin-index       ;; pointer into begin-events (integer)
  end-index)        ;; pointer into end-events (integer)

;; A
(defun find-multiple-pairs (str rules)
  "find nested delimiter pairs in STR based on RULES using pre-scanned events.
RULES is a list of plists, each with keys:
  :begin  -- a two-element spec for the begin marker,
  :end    -- a two-element spec for the end marker,
  :predicate (optional) -- a function of (begin-match end-match),
  :id     (optional) -- an identifier.
returns a list of quintuples:
  (begin-index end-index begin-match end-match rule-id)."
  (let* ((slen (length str))
         ;; build rule-info for each rule.
         (rule-infos
           (mapcar (lambda (r)
                     (make-rule-info
                      :rule r
                      :begin-events (scan-events-for-spec (getf r :begin) str)
                      :end-events   (scan-events-for-spec (getf r :end) str)
                      :begin-index 0
                      :end-index 0))
                   rules))
         (pairs)
         ;; the stack holds open begin markers. each element is a plist with keys:
         ;; :event (the begin event, a triple (start end match))
         ;; :rule-info (the associated rule-info structure)
         (stack)
         (pos 0))
    (loop while (< pos slen) do
      (if stack
          ;; there is an open begin marker.
          (let* ((top (first stack))
                 (ri (getf top :rule-info))
                 (open-event (getf top :event)) ;; (start end match)
                 (open-pos (first open-event))
                 (begin-match (third open-event))
                 (end-evs (rule-info-end-events ri))
                 (ei (rule-info-end-index ri)))
            (if (and (< ei (length end-evs))
                     (= (first (nth ei end-evs)) pos))
                ;; an end event for the top rule is found at pos.
                (let ((end-event (nth ei end-evs))
                      (pred (getf (rule-info-rule ri) :predicate)))
                  (if (or (null pred)
                          (funcall pred begin-match (third end-event)))
                      (progn
                        (push (list open-pos (second end-event)
                                    begin-match (third end-event)
                                    (getf (rule-info-rule ri) :id))
                              pairs)
                        (pop stack)
                        (setf (rule-info-end-index ri) (1+ ei))
                        (setf pos (second end-event)))
                      (pop stack)))
                ;; no matching end event at pos; try to find a new begin event.
                (let ((found nil))
                  (dolist (ri rule-infos found)
                    (let* ((bev (rule-info-begin-events ri))
                           (bi (rule-info-begin-index ri)))
                      (when (and (< bi (length bev))
                                 (= (first (nth bi bev)) pos))
                        (push (list :event (nth bi bev)
                                    :rule-info ri)
                              stack)
                        (setf pos (second (nth bi bev)))
                        (setf (rule-info-begin-index ri) (1+ bi))
                        (setf found t)
                        (return))))
                  (unless found (incf pos 1)))))
          ;; stack is empty: look for a begin event at pos.
          (let ((found nil))
            (dolist (ri rule-infos found)
              (let* ((bev (rule-info-begin-events ri))
                     (bi (rule-info-begin-index ri)))
                (when (and (< bi (length bev))
                           (= (first (nth bi bev)) pos))
                  (push (list :event (nth bi bev)
                              :rule-info ri)
                        stack)
                  (setf pos (second (nth bi bev)))
                  (setf (rule-info-begin-index ri) (1+ bi))
                  (setf found t)
                  (return))))
            (unless found
              ;; no event at pos; jump to the next available begin event.
              (let ((next-pos nil))
                (dolist (ri rule-infos)
                  (let* ((bev (rule-info-begin-events ri))
                         (bi (rule-info-begin-index ri)))
                    (when (< bi (length bev))
                      (let ((candidate (first (nth bi bev))))
                        (when (or (null next-pos) (< candidate next-pos))
                          (setf next-pos candidate))))))
                (if next-pos
                    (setf pos next-pos)
                    (setf pos slen)))))))
    (sort pairs #'< :key #'first)))

;; code for line-pair method
;; A
(defun line-boundaries (str)
  "return a list of cons cells (start . end) representing the boundaries of each line in STR."
  (let ((boundaries)
        (start 0)
        (len (length str)))
    (loop for i from 0 below len do
      (when (char= (char str i) #\Newline)
        (push (cons start i) boundaries)
        (setf start (1+ i)))
          finally (push (cons start len) boundaries))
    (nreverse boundaries)))

;; A
(defun match-in-line (spec line)
  "search LINE for SPEC, a two-element list (type pattern).
returns three values: match-start, match-end, and the matched substring, or NIL if no match."
  (destructuring-bind (type pattern) spec
    (cond
      ((eq type :string)
       (let ((pos (search pattern line)))
         (when pos
           (values pos (+ pos (length pattern))
                   (subseq line pos (+ pos (length pattern)))))))
      ((or (eq type :regex) (eq type :line-regex))
       (multiple-value-bind (m-start m-end)
           (cl-ppcre:scan pattern line)
         (if m-start
             (values m-start m-end (subseq line m-start m-end))
             nil)))
      (t nil))))

;; A
(defun find-line-pairs (str rules)
  "find delimiter pairs in STR on a per-line basis for a list of RULES.
each RULE is a property list with keys:
  :begin -- a two-element list specifying the begin pattern.
  :end   -- a two-element list specifying the end pattern.
  :predicate -- an optional function taking the begin-match and end-match that returns T if they should be paired.
  :id    -- (optional) an identifier.
returns a list of quintuples:
  (begin-index end-index begin-match end-match rule-id)"
  (let ((tokens))
    ;; collect tokens from each line.
    (dolist (bounds (line-boundaries str))
      (let ((line-start (car bounds))
            (line (subseq str (car bounds) (cdr bounds))))
        (dolist (rule rules)
          (dolist (type '(begin end))
            (let ((spec (if (eq type 'begin)
                            (getf rule :begin)
                            (getf rule :end)))
                  (pos 0))
              (loop while (< pos (length line)) do
                (multiple-value-bind (ms me token)
                    (match-in-line spec (subseq line pos))
                  (if ms
                      (progn
                        (push (list (+ line-start ms)
                                    (+ line-start me)
                                    token
                                    type
                                    rule)
                              tokens)
                        (setf pos (+ pos me)))
                      (setf pos (length line))))))))))
    ;; sort tokens by their global start.
    (setf tokens (sort tokens (lambda (a b) (< (first a) (first b)))))
    ;; process tokens using a per-rule stack.
    (let ((pairs)
          (active (make-hash-table :test 'eq)))
      (dolist (tok tokens)
        (destructuring-bind (gstart gend token type rule) tok
          (cond
            ((eq type 'begin)
             (setf (gethash rule active)
                   (cons tok (gethash rule active))))
            ((eq type 'end)
             (let ((stack (gethash rule active)))
               (when stack
                 (let ((begin-token (first stack)))
                   (setf (gethash rule active) (rest stack))
                   (when (or (null (getf rule :predicate))
                             (funcall (getf rule :predicate)
                                      (third begin-token) token))
                     (push (list (first begin-token)
                                 gend
                                 (third begin-token)
                                 token
                                 (getf rule :id))
                           pairs)))))))))
      (nreverse pairs))))

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