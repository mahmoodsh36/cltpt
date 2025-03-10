(defpackage :cltpt
  (:use :cl))
(in-package :cltpt)
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)

(defun find-regex-multiple (text patterns &optional ids)
  "Finds regex matches across lines for multiple patterns."
  (let ((matches)
        (ids (or ids patterns))
        (lines (str:split (string #\Newline) text))
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

;; new here
(defun find-multiple-pairs (str rules &optional (pos 0))
  "find nested delimiter pairs in STR based on RULES.
RULES is a list of property lists, each with keys:
  :begin     -- a list specifying the begin pattern, e.g. (:regex \"#\\+begin_[a-z]+\")
  :end       -- a list specifying the end pattern, e.g. (:regex \"#\\+end_[a-z]+\")
  :predicate -- an optional function that takes two matched strings (from the 'begin' and 'end' of a rule) and returns T if they should be matched together as a pair.
  :id        -- an optional identifier of the rule.
returns a list of quintuples: (start-index end-index begin-match end-match rule-id)."
  (labels ((match-info (spec str i)
             "attempt to match SPEC at position i in STR.
SPEC is a two-element list: (type pattern), where type is either :string or :regex.
Returns three values: the start position, end position, and the matched substring,
or NIL if no match is found at i."
             (destructuring-bind (type pattern) spec
               (cond
                 ((eq type :string)
                  (if (and (<= (+ i (length pattern)) (length str))
                           (string= pattern (subseq str i (+ i (length pattern)))))
                      (values i (+ i (length pattern)) pattern)
                      nil))
                 ((eq type :regex)
                  (multiple-value-bind (start end) (cl-ppcre:scan pattern str :start i)
                    (if (and start (= start i))
                        (values start end (subseq str start end))
                        nil)))
                 (t nil)))))
    (let ((pairs)
          (stack)
          (len (length str))
          (i pos))
      (loop while (< i len) do
        (cond
          ;; try matching any begin delimiter from the provided rules.
          ((let ((found nil))
             (dolist (rule rules found)
               (let ((begin-spec (getf rule :begin)))
                 (multiple-value-bind (ms me begin-match) (match-info begin-spec str i)
                   (when ms
                     (push (list i rule begin-match) stack)
                     (incf i (- me ms))
                     (setf found t)
                     (return)))))))
          ;; if no begin match was found, and there's an open pair,
          ;; try matching the expected end delimiter for the top rule.
          ((and stack
                (let* ((top (first stack)) ; top is (start rule begin-match)
                       (rule (second top))
                       (end-spec (getf rule :end)))
                  (multiple-value-bind (ms me end-match) (match-info end-spec str i)
                    (when ms
                      (let ((pred (getf rule :predicate)))
                        (if (or (null pred) (funcall pred (third top) end-match))
                            (progn
                              (push (list (first top) me (third top) end-match (getf rule :id))
                                    pairs)
                              (pop stack)
                              (incf i (- me ms))
                              t)
                            nil)))))))
          ;; otherwise, advance one character.
          (t (incf i))))
      (sort pairs #'< :key #'first))))

;; example usage:
(find-multiple-pairs
 "#+begin_comment Some text #+end_comment"
 (list
  `(:begin (:regex "#\\+begin_[a-z]+")
    :end   (:regex "#\\+end_[a-z]+")
    :predicate ,(lambda (b e)
                  (string= (subseq b 8) (subseq e 6)))
    :id 'org-block)))