(in-package :cltpt)

;; code for the simplest 'regex method
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

;; code for the simplest 'line-regex method
(defun find-lines-matching-regex (text patterns &optional ids)
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

;; code for 'pair method
(defun match-at (spec str i)
  "attempt to match SPEC at position i in STR.
SPEC is a two-element list: (type pattern) where type is either :string or :regex.
for :string, if the substring starting at i equals the pattern, it returns that match.
if not--and if the pattern does not begin with a colon--it also checks whether the
character at i is a colon and the substring starting at i+1 equals the pattern.
FOr :regex, it uses cl-ppcre:scan and requires the match to start at i.
returns three values: the match start, the match end, and the matched substring, or nil."
  (destructuring-bind (type pattern) spec
    (cond
      ((eq type :string)
       (let ((plen (length pattern)))
         (cond
           ((and (<= (+ i plen) (length str))
                 (string= pattern (subseq str i (+ i plen))))
            (values i (+ i plen) (subseq str i (+ i plen))))
           ((and (< i (length str))
                 (not (char= (char pattern 0) #\:)) ; only try optional colon if pattern doesn't start with one
                 (char= (char str i) #\:)
                 (<= (+ i 1 plen) (length str))
                 (string= pattern (subseq str (1+ i) (+ i 1 plen))))
            (values i (+ i 1 plen) (subseq str i (+ i 1 plen))))
           (t nil))))
      ((eq type :regex)
       (multiple-value-bind (m-start m-end)
           (cl-ppcre:scan pattern str :start i)
         (if (and m-start (= m-start i))
             (values m-start m-end (subseq str m-start m-end))
             nil)))
      (t nil))))

(defun find-multiple-pairs (str rules &optional (pos 0))
  "find nested delimiter pairs in STR based on RULES.
RULES is a list of property lists. each rule has:
  :begin     -- a two-element list specifying the begin pattern,
               e.g. (:regex \":[a-zA-Z]+:\") or (:string \":begin:\")
  :end       -- a two-element list specifying the end pattern,
               e.g. (:string \":end:\") or (:regex \"(?i):end:\")
  :predicate -- an optional function taking the begin-match and end-match that returns T if they should be paired.
  :id        -- an optional identifier.
returns a list of quintuples:
  (begin-index end-index begin-match end-match rule-id)."
  (let ((pairs)
        (stack)
        (i pos)
        (len (length str)))
    (loop while (< i len) do
      (if stack
          (let* ((top (first stack)) ;; top is of the form (begin-index rule begin-match)
                 (rule (second top))
                 (end-spec (getf rule :end)))
            (multiple-value-bind (ms me match) (match-at end-spec str i)
              (if ms
                  (progn
                    (push (list (first top) me (third top) match (getf rule :id)) pairs)
                    (pop stack)
                    (setf i me))
                  (let ((found))
                    (dolist (r rules found)
                      (multiple-value-bind (msb meb matchb) (match-at (getf r :begin) str i)
                        (when msb
                          (push (list i r matchb) stack)
                          (setf i meb)
                          (setf found t)
                          (return))))
                    (unless found
                      (incf i 1))))))
          (let ((found))
            (dolist (r rules found)
              (multiple-value-bind (ms me match) (match-at (getf r :begin) str i)
                (when ms
                  (push (list i r match) stack)
                  (setf i me)
                  (setf found t)
                  (return))))
            (unless found
              (incf i 1)))))
    (sort pairs #'< :key #'first)))

;; code for line-pair method
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

(defun match-in-line (spec line)
  "search LINE for SPEC, a two-element list of the form (type pattern).
TYPE can be:
  :string     -- uses `search` to find the substring anywhere in LINE.
  :regex      -- uses cl-ppcre:scan to search LINE for the pattern.
  :line-regex -- treated the same as :regex here.
returns three values: match-start, match-end, and the matched substring,
or NIL if no match is found."
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

(defun find-line-pairs (str rules)
  "find delimiter pairs in STR on a per-line basis for a list of RULES.
each RULE is a property list with keys:
  :begin -- a two-element list specifying the begin pattern.
            the type can be :string, :regex, or :line-regex.
  :end   -- a two-element list specifying the end pattern.
            the type can be :string, :regex, or :line-regex.
  :id    -- (optional) an identifier.
returns a list of quintuples:
  (begin-index end-index begin-match end-match rule-id)
with indices relative to the original STR.
for each rule, if a begin delimiter is found (via `match-in-line`)
and later an end delimiter is found on the same or a subsequent line,
a pair is recorded. If both begin and end occur on the same line and the
end match follows the begin match, the pair is recorded immediately."
  (let ((pairs)
        ;; ACTIVE is an association list mapping a rule to its active begin info.
        ;; each active begin info is a list:
        ;; (global-begin-index, begin-match, local-begin-index, begin-line-start)
        (active))
    (dolist (bounds (line-boundaries str))
      (let* ((line-start (car bounds))
             (line-end (cdr bounds))
             (line (subseq str line-start line-end)))
        (dolist (rule rules)
          (let* ((rule-id (getf rule :id))
                 (begin-spec (getf rule :begin))
                 (end-spec (getf rule :end))
                 (active-info (assoc rule active)))
            (if active-info
                ;; we already have a begin match for this rule.
                (multiple-value-bind (ems eme end-match) (match-in-line end-spec line)
                  (when ems
                    (let* ((info (cdr active-info))
                           (global-begin (nth 0 info))
                           (begin-match (nth 1 info))
                           (local-begin (nth 2 info))
                           (begin-line  (nth 3 info)))
                      ;; if the begin was on the same line, ensure the end match comes later.
                      (when (or (/= begin-line line-start)
                                (> ems local-begin))
                        (push (list global-begin
                                    (+ line-start eme)
                                    begin-match
                                    end-match
                                    rule-id)
                              pairs)
                        (setf active (remove active-info active :test #'eq))))))
                ;; no active begin exists for this rule; try to match the begin delimiter.
                (multiple-value-bind (bms bme begin-match) (match-in-line begin-spec line)
                  (when bms
                    (let ((global-begin (+ line-start bms))
                          (global-end (+ line-start bme)))
                      ;; check if an end match also exists on this same line after the begin match.
                      (multiple-value-bind (ems eme end-match)
                          (match-in-line end-spec line)
                        (if (and ems (> ems bme))
                            (push (list global-begin
                                        (+ line-start eme)
                                        begin-match
                                        end-match
                                        rule-id)
                                  pairs)
                            ;; otherwise, record this begin match as active for later pairing.
                            (push (cons rule (list global-begin begin-match bms line-start))
                                  active)))))))))))
    (nreverse pairs)))