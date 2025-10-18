(in-package :cltpt/base)

(defun last-atom (seq)
  (car (last seq)))

(defun md5-str (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (sb-ext:string-to-octets s :external-format :utf-8))))

;; this is a temporary workaround because `find-class' is really slow..
(defparameter *class-map* (make-hash-table))
(defun find-class-faster (class-sym)
  (let ((result (gethash class-sym *class-map*)))
    (unless result
      (setf result (find-class class-sym))
      (setf (gethash class-sym *class-map*) result))
    result))

(defun change-symbol-package (symbol pkg)
  "change the package to which SYMBOL refers."
  ;; we use `symbol-name' and then `intern' to change the package of a symbol.
  (intern (symbol-name symbol) pkg))

(defun eval-in-package (expr pkg)
  "takes expression EXPR and package name PKG, evaluates EXPR in the scope of PKG."
  (let ((*package* (find-package pkg)))
    (eval expr)))

;; TODO: the non-star one doesnt work correctly, but im not using it anywhere anyway
(defun bind-and-eval (bindings func &optional (pkg-to-eval-in :cl-user))
  "dynamically binds symbols from BINDINGS (a list of symbol,value pairs) and executes FUNC."
  (let ((keys (mapcar (lambda (x) (change-symbol-package x pkg-to-eval-in))
                      (mapcar #'car bindings)))
        (values (mapcar (lambda (x) (eval-in-package x pkg-to-eval-in))
                        (mapcar 'cadr bindings))))
    (progv keys values
      (funcall func))))
(defun bind-and-eval* (bindings func &optional (pkg-to-eval-in :cl-user))
  (if (null bindings)
      (let ((*package* (find-package pkg-to-eval-in)))
        (funcall func))
    (destructuring-bind (sym val-expr) (car bindings)
      (progv
          (list (change-symbol-package sym pkg-to-eval-in))
          (list val-expr)
        (bind-and-eval* (cdr bindings) func pkg-to-eval-in)))))
;; example
;; (bind-and-eval '((x 1) (y (+ 1 2))) (lambda () (+ x y)))
;; (bind-and-eval* '((x 1) (y (+ x 2))) (lambda () (+ x y)))

(defmacro pcase (keyform &body clauses)
  "a case matcher that can take variables, like elisp's `pcase'.

example usage: `(let ((myvar 'latex)) (pcase 'latex ('html 1) (myvar 2)))'"
  (let ((keyval-gensym (gensym "pcase")))
    `(let ((,keyval-gensym ,keyform))
       (cond
         ,@(loop for clause in clauses
                 if (member (car clause) '(t otherwise) :test #'eq)
                   collect `(t ,@(cdr clause))
                 else
                   collect (let ((test-form (car clause))
                                 (body (cdr clause)))
                             `((eql ,keyval-gensym ,test-form)
                               ,@body)))))))

(defun plistp (list1)
  "check whether LIST1 is a plist."
  (and (consp list1)
       (keywordp (car list1))))

(defun flatten (l)
  "flatten a tree: turn it into a list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

;; ended up not needing this more complex one
;; (defun flatten (tree &optional (max-depth 1))
;;   "flatten TREE so that no node in the result exceeds MAX-DEPTH nesting.
;; example: (flatten '(1 (2 (3 4)) (((5 10)))) 2) => (1 2 (3 4) (5 10))"
;;   (labels
;;       ((node-depth (node)
;;          (if (atom node)
;;              0
;;              (1+ (reduce 'max (mapcar #'node-depth node)))))
;;        (flatten-if-too-deep (node)
;;          (if (or (atom node)
;;                  (<= (node-depth node) max-depth))
;;              node
;;              (mapcan (lambda (x)
;;                        (let ((res (flatten-if-too-deep x)))
;;                          (if (listp res) res (list res))))
;;                      node))))
;;     (flatten-if-too-deep tree)))

(defun compress-consec (s char-to-compress)
  "compresses runs of a specific character into a single instance. aaab -> ab."
  (with-output-to-string (out)
    (loop for current-char across s
          with last-char = nil
          do
             (unless (and (char= current-char char-to-compress)
                          (and last-char (char= last-char char-to-compress)))
               (write-char current-char out))
             (setf last-char current-char))))

(defun alist-get (alist key)
  "grab value by KEY from ALIST (compare using `equal')."
  (cdr (assoc key alist :test 'equal)))

(defun str-join (string-list separator)
  "joins a list of strings with a SEPARATOR in between each element."
  (with-output-to-string (stream)
    (loop for (str . rest) on string-list
          do (write-string str stream)
          when rest do (write-string separator stream))))

(defun seq-type (seq)
  "get the type of a sequence (`type-of' is unreliable). this returns one of 3 values, so its not completely general as the types are hardcoded"
  (typecase seq
    (string 'string)
    (vector 'vector)
    (list 'list)))

;; note that `type' is still necessary in some cases because NIL can be
;; ambiguous. should we return an empty string? and empty list? its unclear.
(defun concat (seqs &optional type)
  "run `concatenate', automatically detect type of sequence using `seq-type'.

the type of sequence to return is determined by the first sequences in the list of sequences
  example usage:
  CL-USER> (concatenate-type-aware '((1 2 3) (1)))
  (1 2 3 1)
  CL-USER> (concatenate-type-aware '(\"hi\" \"hey\"))
  \"hihey\"
  CL-USER> (concatenate-type-aware '(#(1 2 3) #(10)))
  #(1 2 3 10)"
  (apply #'concatenate
         (list* (or type (seq-type (first seqs))) seqs)))

(defun str-prune (string max-length &optional (omission "..."))
  "truncates a string to a maximum length, appending an omission string.

args:
  string: the input string to prune.
  max-length: the maximum desired length of the resulting string.
  omission: the string to append if truncation occurs (defaults to \"...\").

returns the pruned string, or the original string if it's short enough."
  (let ((string-length (length string))
        (omission-length (length omission)))
    ;; if the original string is already short enough, return it as is.
    (if (<= string-length max-length)
        string
        ;; check if the max-length is even long enough for the omission.
        ;; if not, truncate the omission string itself to fit.
        (if (< max-length omission-length)
            (subseq omission 0 max-length)
            ;; otherwise, concatenate the start of the string with the omission.
            (concatenate 'string
                         (subseq string 0 (- max-length omission-length))
                         omission)))))

(defun str-split (str sep)
  "split STR by the seperator SEP. simply calls `uiop:split-string'."
  (uiop:split-string str :separator sep))

(defun subseq* (sequence start &optional end)
  "a version of `subseq` that handles negative indices.

a negative index 'n' is treated as `(length sequence) + n`.
if 'end' is nil, it defaults to the end of the sequence."
  (let* ((len (length sequence))
         ;; resolve the start index. if negative, calculate from the end.
         (resolved-start (if (< start 0) (+ len start) start))
         ;; resolve the end index. if nil, use the length. if negative, calculate.
         (resolved-end (cond ((null end) len)
                             ((< end 0) (+ len end))
                             (t end))))
    ;; to prevent errors from out-of-bounds calculations, clamp the indices
    ;; to the valid range [0, len]. the real `subseq' will handle cases where
    ;; the final start >= end by returning an empty sequence.
    (let ((clamped-start (max 0 (min len resolved-start)))
          (clamped-end (max 0 (min len resolved-end))))
      (subseq sequence clamped-start clamped-end))))

(defun find-cdr-where-greater (list x &key (key #'identity))
  "return the cons cell whose CDR starts with the first element whose
key value is greater than X. returns NIL if insertion should be at head."
  (loop for prev = nil then curr
        for curr = list then (cdr curr)
        while curr
        when (> (funcall key (car curr)) x)
          do (return prev)
        finally (return prev)))

(defun sorted-insert (list item &key (key #'identity))
  "insert ITEM into LIST, preserving ascending order based on KEY.
returns the (possibly new) list."
  (let* ((x (funcall key item))
         (cdr-cell (find-cdr-where-greater list x :key key)))
    (if cdr-cell
        ;; insert after cdr-cell
        (setf (cdr cdr-cell) (cons item (cdr cdr-cell)))
        ;; insert at head if no previous cons
        (setf list (cons item list)))
    list))

(defun merge-plist (p1 p2)
  "merge two given plists into one."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound)
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

(defun replace-substr (original replacement begin end)
  "return a new string where ORIGINAL[BEGIN..END) is replaced with REPLACEMENT.
BEGIN is inclusive, END is exclusive."
  (concatenate 'string
               (subseq original 0 begin)
               replacement
               (subseq original end)))

(defun add-duration (timestamp duration &key (sign 1))
  "apply a duration stored as a plist like (:hour 1 :minute 30).
if SIGN is -1 this subtracts instead of adds."
  (let ((ts timestamp))
    (loop for (k v) on duration by #'cddr do
      (ecase k
        (:hour   (setf ts (local-time:timestamp+ ts (* sign v) :hour)))
        (:minute (setf ts (local-time:timestamp+ ts (* sign v) :minute)))
        (:sec    (setf ts (local-time:timestamp+ ts (* sign v) :sec)))
        (:day    (setf ts (local-time:timestamp+ ts (* sign v) :day)))
        (:year   (setf ts (local-time:timestamp+ ts (* sign v) :year)))
        (otherwise (error "unsupported duration key: ~S" k))))
    ts))

(defun list-dates (start-date end-date interval)
  "generates and returns a list of timestamps from START-DATE to END-DATE.

- START-DATE and END-DATE should be local-time timestamp objects.
- INTERVAL is a plist like (:day 1 :hour 12)."
  (loop for current-date = start-date then (add-duration current-date interval)
        while (local-time:timestamp<= current-date end-date)
        collect current-date))

(defun list-date-pairs (start-date end-date interval)
  "generates a list of (current-date . next-date) cons pairs.
the loop stops when NEXT-DATE would be after END-DATE."
  (loop for current-date = start-date then next-date
        for next-date = (add-duration current-date interval)
        while (local-time:timestamp<= next-date end-date)
        collect (cons current-date next-date)))

(defun truncate-to-day (timestamp)
  (multiple-value-bind (nsec sec min hr day month year day-of-week dst-p offset tz-obj)
      (local-time:decode-timestamp timestamp)
    (local-time:encode-timestamp 0 0 0 0 day month year)))

(defun today-timestamp ()
  (truncate-to-day (local-time:today)))

(defun replace-all (string part replacement &key (test #'string=))
  "returns a new string in which all occurrences of PART are replaced with REPLACEMENT."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))