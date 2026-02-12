(in-package :cltpt/base)

(defun last-atom (seq)
  (car (last seq)))

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
          (list (eval val-expr))
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

(defun alist-get (alist key)
  "grab value by KEY from ALIST (compare using `equal')."
  (cdr (assoc key alist :test 'equal)))

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

the type of sequence to return is determined by the first sequences in the list of sequences.
example usage:
  CL-USER> (concatenate-type-aware '((1 2 3) (1)))
  (1 2 3 1)
  CL-USER> (concatenate-type-aware '(\"hi\" \"hey\"))
  \"hihey\"
  CL-USER> (concatenate-type-aware '(#(1 2 3) #(10)))
  #(1 2 3 10)"
  (apply #'concatenate
         (list* (or type (seq-type (first seqs))) seqs)))

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

(defun find-cons-if (predicate list &key (key #'identity))
  "finds and returns the first cons cell in LIST whose CAR satisfies the PREDICATE.

- PREDICATE: a function of one argument that returns true or false.
- LIST: the list to search through.
- KEY: a function to be applied to the element before it's passed to the predicate.

returns the cons cell on success, or NIL if no element satisfies the predicate."
  (loop for cons on list
        when (funcall predicate (funcall key (car cons)))
          do (return cons)))

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
          do (push value p2)
             (push indicator p2))
  p2)

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
        (:week   (setf ts (local-time:timestamp+ ts (* sign v 7) :day)))
        (:year   (setf ts (local-time:timestamp+ ts (* sign v) :year)))
        (otherwise (error "unsupported duration key: ~S" k))))
    ts))

(defun list-dates (start-date end-date interval)
  "generates and returns a list of timestamps from START-DATE to END-DATE.

- START-DATE and END-DATE should be local-time timestamp objects.
- INTERVAL is a plist like (:day 1 :hour 12)."
  ;; validate interval to prevent infinite loops
  (when (or (and (getf interval :day) (zerop (getf interval :day)))
            (and (getf interval :hour) (zerop (getf interval :hour)))
            (and (getf interval :minute) (zerop (getf interval :minute)))
            (and (getf interval :second) (zerop (getf interval :second)))
            (and (getf interval :week) (zerop (getf interval :week)))
            (and (getf interval :month) (zerop (getf interval :month)))
            (and (getf interval :year) (zerop (getf interval :year))))
    (error "zero interval values are not allowed in list-dates: ~A" interval))
  (loop for current-date = start-date then (add-duration current-date interval)
        while (local-time:timestamp<= current-date end-date)
        collect current-date))

(defun list-date-pairs (start-date end-date interval)
  "generates a list of (current-date . next-date) cons pairs.
the loop stops when NEXT-DATE would be after END-DATE."
  ;; validate interval to prevent infinite loops
  (when (or (and (getf interval :day) (zerop (getf interval :day)))
            (and (getf interval :hour) (zerop (getf interval :hour)))
            (and (getf interval :minute) (zerop (getf interval :minute)))
            (and (getf interval :second) (zerop (getf interval :second)))
            (and (getf interval :week) (zerop (getf interval :week)))
            (and (getf interval :month) (zerop (getf interval :month)))
            (and (getf interval :year) (zerop (getf interval :year))))
    (error "zero interval values are not allowed in list-date-pairs: ~A" interval))
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