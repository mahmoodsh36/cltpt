(in-package :cltpt/base)

;; dynamically bound when executing a post-lexer text-macro to give the context of the object
;; to the function that will be running
(defvar *post-lexer-text-macro-dynamic-object*)

(defstruct region
  (begin 0)
  (end -1))

(defmethod region-decf ((r region) num)
  (decf (region-begin r) num)
  (decf (region-end r) num))

(defmethod region-incf ((r region) num)
  (incf (region-begin r) num)
  (incf (region-end r) num))

(defmethod region-text ((r region) str1)
  (subseq str1 (region-begin r) (region-end r)))

(defmethod region-contains ((r region) pos)
  (and (< pos (region-end r))
       (>= pos (region-begin r))))

(defmethod region-encloses ((r region) (r2 region))
  "returns whether region R encloses region R2."
  (and (<= (region-begin r2) (region-end r))
       (>= (region-begin r2) (region-begin r))))

(defmethod region-length ((r region))
  (with-slots (begin end) r
    (- end begin)))

(defmethod region-clone ((r region))
  (make-region :begin (region-begin r)
               :end (region-end r)))

(defclass text-object ()
  ((properties
    :initarg :properties
    :accessor text-object-properties
    :initform nil
    :documentation "other properties that the cltpt text-object may hold.")
   (text
    :initarg :text
    :accessor text-object-text
    :documentation "the text that the element corresponds to.")
   (children
    :initarg :children
    :accessor text-object-children
    :documentation "the children elements of this element."
    :initform nil)
   (parent
    :initarg :parent
    :accessor text-object-parent
    :documentation "the parent of this element."
    :initform nil)
   (text-region
    :initarg :text-region
    :accessor text-object-text-region
    :documentation "the bounds of the text corresponding to the object in its parent's text.")
   (rule
    :accessor text-object-rule
    :allocation :class
    :initform nil
    :documentation "the matching method from `*matching-methods*' used to match against the text object.")
   (shared-name
    :accessor text-object-shared-name
    :allocation :class
    :initform nil
    :documentation "text objects that have the same `shared-name' should be easier to identify across formats."))
  (:documentation "cltpt objects base class"))

(defgeneric text-object-init (text-obj str1 match)
  (:documentation "this function is invoked by the parser,
STR1 is the string (or buffer) being parsed, MATCH is the matching text for the
object that was detected by the parser combinator."))

(defgeneric text-object-finalize (text-obj)
  (:documentation "this function is invoked by the parser once it is done.
the text object should finalize initialization or any other functionality."))

(defgeneric text-object-ends-by (text-obj value)
  (:documentation "should return whether the value indicates the ending of the
object's region. you should just make it return a symbol like `end-type'."))

;; the default end function returns the value 'end, which should end any text object that
;; comes before it, this isnt recommended as it may cause ambiguations
;; value can be 'end-of-buffer to denote the end of file/buffer/text, this is useful for headers
;; which should be ended by new headers but also by the end of the text
;; value can also be another text object
(defmethod text-object-ends-by ((text-obj text-object) value)
  (and (symbolp value) (string= value 'end)))

(defmethod text-object-property ((obj text-object) property &optional (default nil))
  (getf (text-object-properties obj) property default))

(defmethod (setf text-object-property) (value (obj text-object) property &optional default)
  (setf (getf (text-object-properties obj) property) value))

(defgeneric text-object-convert (text-obj backend)
  (:documentation "returns a plist or a string, if string, converted with recursion and no \"reparsing\".
if plist, plist can contain the keywords:
  :text    - the text to convert,
  :reparse - whether to reparse the given :text, or if :text isnt provided the result of text-object-text, the default behavior is to recurse,
  :reparse-region - the region of the text that is given that is reparsed, if at all,
  :recurse - whether to convert children as well."))

(defgeneric text-object-convert-options (text-obj backend)
  (:documentation "a set of options are available by the conversion function.
the options are:
- :remove-newline-after: whether to trim a new line that comes after the object during conversion."))

(defmethod text-object-convert ((obj text-object) backend)
  "default convert function."
  (if (text-object-property obj :eval-result)
      (list :text (format nil "~A" (text-object-property obj :eval-result)))
      (list :text (text-object-text obj)
            :recurse t
            :escape t)))

(defmethod text-object-convert-options ((obj text-object) backend)
  "default convert options function."
  (list :remove-newline-after (not (text-object-property obj :is-inline))))

;; default init function will just set the text slot of the object
;; we are currently using `subseq' to extract the region from the text and store
;; a new sequence for every object, this is both slow and memory-consuming
(defmethod text-object-init ((text-obj text-object) str1 match)
  ;; text of text-object should only be the text that it encloses in its parent
  (setf (text-object-text text-obj)
        (getf (car match) :match))
  (setf (text-object-text-region text-obj)
        (make-region :begin (getf (car match) :begin)
                     :end (getf (car match) :end)))
  (setf (text-object-property text-obj :combinator-match) match))

(defmethod text-object-adjust-to-parent ((child text-object) (parent text-object))
  (region-decf (text-object-text-region child)
               (text-object-begin parent)))

(defmethod text-object-begin ((text-obj text-object))
  "where the text object begins."
  (region-begin (text-object-text-region text-obj)))

(defmethod text-object-end ((text-obj text-object))
  "where the text object ends relative to its parent."
  (region-end (text-object-text-region text-obj)))

;; TODO: this doesnt take into account ordering of children in parent, it just
;; *pushes* into the parent.
(defmethod text-object-set-parent ((child text-object) (parent text-object))
  (when (text-object-parent child)
    (delete child (text-object-children (text-object-parent child))))
  (setf (text-object-parent child) parent)
  (push child (text-object-children parent)))

(defmethod print-object ((obj text-object) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "-> ~A ~A"
            (if (slot-boundp obj 'text-region)
                (text-object-text-region obj)
                nil)
            (if (slot-boundp obj 'text)
                (cltpt/base:str-prune (text-object-text obj) 15)
                nil))))

;; this is actually the slowest way to traverse siblings
;; TODO: easy to optimize
(defmethod text-object-next-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (< idx (1- (length (text-object-children parent))))
          (elt (text-object-children parent) (1+ idx)))))))

;; this is actually the slowest way to traverse siblings
;; TODO: easy to optimize
(defmethod text-object-prev-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (and (numberp idx) (> idx 0))
          (elt (text-object-children parent) (1- idx)))))))

(defmethod text-object-finalize ((obj text-object))
  "default finalize function, does nothing.")

(defclass document (text-object)
  ()
  (:documentation "top-level text element."))

(defmethod document-title ((obj document))
  "a generic function for grabbing the title of a document. defaults to properties."
  (text-object-property obj :title))

(defmethod document-date ((obj document))
  "a generic function for grabbing the date of a document. defaults to properties."
  (text-object-property obj :date))

(defclass text-block (text-object)
  ()
  (:documentation "a text block."))

(defun make-block (&rest kws &key &allow-other-keys)
  (let* ((loop-expr (getf kws :loop))
         (obj (make-instance 'text-block)))
    (loop for (key value) on kws by #'cddr
          do (setf (text-object-property obj key) value))
    ;; we shouldnt be evalling this.
    ;; (setf (text-object-property obj :loop)
    ;;       (cons (car loop-expr) (eval (cadr loop-expr))))
    obj))

(defun block-end ()
  'block-end)

;; aliases for blocks
(setf (symbol-function 'b) (symbol-function 'make-block))
(setf (symbol-function '/b) (symbol-function 'block-end))
(setf (symbol-function 'blk) (symbol-function 'make-block))
(setf (symbol-function '/blk) (symbol-function 'block-end))

(defmethod text-object-ends-by ((text-obj text-block) value)
  (and (symbolp value) (string= value 'block-end)))

;; a utility function to reduce boilerplate for `text-object-convert' implementations
(defun wrap-contents-for-convert (text-obj preamble postamble
                                 &key (reparse t) (escape t))
  (let* ((text (concatenate 'string
                            preamble
                            (text-object-contents text-obj)
                            postamble))
         (inner-region (make-region :begin (length preamble)
                                    :end (- (length text) (length postamble)))))
    (list :text text
          :reparse reparse
          :escape escape
          :reparse-region inner-region
          :escape-region inner-region)))

(defun sort-text-objects (text-objects)
  "return TEXT-OBJECTS, sorted by starting point."
  (sort
   text-objects
   '<
   :key
   (lambda (obj)
     (region-begin (text-object-text-region obj)))))

(defmethod text-object-sorted-children (obj)
  "return the children of the text-obj, sorted by starting point."
  (sort-text-objects (text-object-children obj)))

(defmethod text-object-contents-begin ((text-obj text-object))
  (if (text-object-property text-obj :contents-region)
      (region-begin (text-object-property text-obj :contents-region))
      0))

(defmethod text-object-contents-end ((text-obj text-object))
  (if (text-object-property text-obj :contents-region)
      (or (region-end (text-object-property text-obj :contents-region))
          (length (text-object-text text-obj)))
      (length (text-object-text text-obj))))

(defmethod text-object-contents ((obj text-object))
  (subseq (text-object-text obj)
          (text-object-contents-begin obj)
          (text-object-contents-end obj)))

(defclass text-macro (text-object)
  ((rule
    :allocation :class
    :initform '(:pattern
                (cltpt/combinator:unescaped
                 (cltpt/combinator:consec
                  (cltpt/combinator:literal "#")
                  (:pattern (cltpt/combinator:lisp-sexp)
                   :id lisp-code)))
                :on-char #\#))))

(defvar *post-lexer-text-macro-rule*
  '(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:literal "%")
     (:pattern (cltpt/combinator:lisp-sexp)
      :id lisp-code))
    :on-char #\%))
(defclass post-lexer-text-macro (text-object)
  ((rule
    :allocation :class
    :initform *post-lexer-text-macro-rule*)))

;; we need to evaluate post-lexer text-macros during finalization. the result
;; of the evaluation should be also be cached for later use.
(defmethod cltpt/base:text-object-finalize ((obj post-lexer-text-macro))
  (eval-post-lexer-macro obj))

(defvar *cache-post-lexer-macro-evals* nil)
(defun eval-post-lexer-macro (obj)
  ;; TODO: we can cache results to avoid re-eval which is log(n). i tried this
  ;; but it causes issues as the value sometimes seems to evaluate to 'broken during
  ;; conversion even though it shouldnt be. i really need to investigate this.
  ;; it probably has to do with objects being reparsed/recreated during conversion.
  (when (and *cache-post-lexer-macro-evals* (text-object-property obj :eval-result))
      (return-from eval-post-lexer-macro (text-object-property obj :eval-result)))
  (let ((txt-to-eval (subseq (text-object-text obj) 1))
        (macro-eval-result)
        (*post-lexer-text-macro-dynamic-object* obj))
    (handler-case
        (eval-in-text-object-lexical-scope
         obj
         (lambda ()
           (let ((*package* (find-package :cl-user)))
             (eval
              (read-from-string txt-to-eval)))))
      (error (c)
        (when (getf cltpt:*debug* :parse)
          (format t "error while evaluating post-lexer macro ~A: ~A.~%"
                  txt-to-eval c))
        (setf macro-eval-result 'broken))
      (:no-error (result1)
        (setf macro-eval-result result1)))
    (setf (text-object-property obj :eval-result) macro-eval-result)
    macro-eval-result))

(defun eval-in-text-object-lexical-scope (obj func)
  (let ((parent (text-object-parent obj))
        (binds (or (text-object-property obj :let*)
                   (text-object-property obj :let))))
    (if parent
        (eval-in-text-object-lexical-scope
         parent
         (lambda ()
           (bind-and-eval* binds func)))
        (bind-and-eval* binds func))))

(defun convert-post-lexer-macro-obj (obj backend)
  (let ((eval-result (eval-post-lexer-macro obj)))
    (if (typep eval-result 'text-object)
        (text-object-convert eval-result backend)
        (list :text (princ-to-string eval-result)
              ;; :reparse makes post-lexer macros able to contain markup contents
              ;; that gets handled during conversion.
              ;; currently, the code for converting org-document exploits this.
              :reparse t))))

(defmethod text-object-convert ((obj post-lexer-text-macro) backend)
  (convert-post-lexer-macro-obj obj backend))

;; we need to "finalize" the classes to be able to use MOP, a temporary workaround..
(defparameter *finalized-map* (make-hash-table))
(defun ensure-finalized (mytype)
  (unless (gethash mytype *finalized-map*)
    (sb-mop:finalize-inheritance (find-class-faster mytype))
    (setf (gethash mytype *finalized-map*) t))
  t)

;; modifies the tree of a rule, replaces 'eval' instance with the evaluation result
(defun post-process-rule (rule)
  (if (listp rule)
      (if (null rule)
          nil
          (if (and (symbolp (car rule)) (string= (car rule) 'eval))
              (loop for child in (eval (cadr rule))
                    collect (post-process-rule child))
              (if (symbolp (car rule))
                  (cons (car rule)
                        (loop for child in (cdr rule)
                              collect (post-process-rule child)))
                  (loop for child in rule
                        collect (post-process-rule child)))))
      rule))

(defvar *text-object-rule-hash*
  (make-hash-table :test 'equal)
  "a hashtable mapping symbols of `text-object' subclasses to their combinator rules.")
(defun text-object-rule-from-subclass (subclass)
  ;; we need to finalize it, otherwise it'll error out.
  (ensure-finalized subclass)
  ;; (sb-mop:finalize-inheritance (find-class-faster subclass))
  (let ((stored-rule (gethash subclass *text-object-rule-hash*))
        (rule))
    (if stored-rule
        (setf rule stored-rule)
        (progn
          (setf rule
                (post-process-rule
                 (slot-value
                  (sb-mop:class-prototype (find-class-faster subclass))
                  'rule)))
          (setf (gethash subclass *text-object-rule-hash*) rule)))
    (unless (plistp rule)
      (setf rule (list :pattern rule :id subclass)))
    (unless (getf rule :id)
      (setf (getf rule :id) subclass))
    rule))

(defun text-object-shared-name-from-subclass (subclass)
  (ensure-finalized subclass)
  (let ((cls (find-class-faster subclass)))
    (when (slot-boundp (sb-mop:class-prototype cls) 'shared-name)
      (slot-value (sb-mop:class-prototype cls)
                  'shared-name))))

(defmethod cltpt/tree:tree-children ((subtree text-object))
  (text-object-children subtree))

(defmethod cltpt/tree:tree-value ((subtree text-object))
  subtree)

(defmethod cltpt/tree:is-subtree ((subtree text-object) child)
  (typep child 'text-object))

(defmethod map-text-object ((text-obj text-object) func)
  "traverse the text object tree starting at TEXT-OBJ."
  (cltpt/tree:tree-map text-obj func))

(defun prev-obj ()
  (text-object-prev-sibling *post-lexer-text-macro-dynamic-object*))

(defun prev-obj-eval ()
  (let* ((obj *post-lexer-text-macro-dynamic-object*)
         (prev (when obj (text-object-prev-sibling obj))))
    (when prev
      (cond
        ((typep prev 'post-lexer-text-macro)
         (eval-post-lexer-macro
          (text-object-prev-sibling *post-lexer-text-macro-dynamic-object*)))
        ((typep prev 'text-object) ;; pre-lexer text-macro's turn into text-object's
         (text-object-property prev :eval-result))
        (t nil)))))

(defun make-text-link (&rest kws &key &allow-other-keys)
  (let* ((id (getf kws :id))
         (text (getf kws :text))
         (obj (make-instance 'text-link)))
    (setf (text-object-property obj :id) id)
    (setf (text-object-property obj :text) text)
    (loop for (key value) on kws by #'cddr
          do (setf (text-object-property obj key) value))
    obj))

;; TODO: easy to optimize
;; inefficient :(, takes O(log(n)) when it should take O(1)
(defmethod text-object-begin-in-root ((text-obj text-object))
  "where the text object begins in the root-most parent."
  (let ((begin (region-begin (text-object-text-region text-obj)))
        (parent (text-object-parent text-obj)))
    (if parent
        (+ begin (text-object-begin-in-root parent))
        begin)))

;; TODO: easy to optimize
(defmethod text-object-end-in-root ((text-obj text-object))
  "where the text object begins in the root-most parent."
  (let ((begin-in-root (text-object-begin-in-root text-obj)))
    (+ begin-in-root (region-length (text-object-text-region text-obj)))))

(defmethod find-children-recursively ((text-obj text-object) cond)
  (let ((results))
    (labels ((add-if (obj)
               (when (funcall cond obj)
                 (push obj results))))
      (map-text-object text-obj #'add-if))
    results))

(defmethod find-children ((text-obj text-object) cond)
  (remove-if-not cond (text-object-children text-obj)))

(defmethod list-children-recursively ((text-obj text-object))
  "lists all nested children text objects of TEXT-OBJ. including the object itself."
  (let ((results))
    (labels ((handle (obj)
               (push obj results)))
      (cltpt/base:map-text-object text-obj #'handle))
    results))

(defmethod child-at-pos ((text-obj text-object) pos)
  "find the leaf-most child that encloses the index POS."
  (loop for child in (text-object-children text-obj)
        do (when (region-contains (text-object-text-region child) pos)
             (return-from child-at-pos
               (if (text-object-children child)
                   (or
                    (child-at-pos
                     child
                     (+ pos
                        (region-begin
                         (text-object-text-region child))))
                    child)
                   child)))))

(defmethod text-object-clone ((text-obj text-object))
  (let ((new-obj (make-instance (class-of text-obj))))
    (setf (text-object-properties new-obj)
          (copy-tree (text-object-properties text-obj)))
    (setf (text-object-text new-obj)
          (copy-seq (text-object-text text-obj)))
    (setf (text-object-text-region new-obj)
          (make-region :begin (text-object-begin text-obj)
                       :end (text-object-end text-obj)))
    (setf (text-object-children new-obj)
          (loop for child in (text-object-children text-obj)
                collect (text-object-clone child)))
    new-obj))

(defun map-text-object-with-pos-in-root (text-obj func &optional (pos 0))
  "traverse the text object tree starting at TEXT-OBJ. POS is the position of
TEXT-OBJ according to the root. this is more efficient than iterating through objects
and grabbing each position of each object through its ascendants in the tree."
  (let ((new-pos (+ pos (text-object-begin text-obj))))
    (cons (funcall func text-obj pos)
          (loop for child in (text-object-children text-obj)
                collect (map-text-object-with-pos-in-root child func new-pos)))))

(defmethod cltpt/tree:tree-parent ((text-obj text-object))
  (text-object-parent text-obj))

;; completely handles the movement of a text object from one tree to another
;; TODO: optimize this, it is very slow.
(defmethod text-object-move ((child text-object) (parent text-object))
  (let ((old-parent (text-object-parent child)))
    ;; insert it at the right location, keeping the list of children sorted.
    (setf (text-object-children parent)
           (cltpt/base:sorted-insert
            (text-object-children parent)
            child
            :key 'text-object-begin-in-root))
    ;; correctly re-set the region for the child
    (setf (text-object-text-region child)
          (make-region
           :begin (- (text-object-begin-in-root child)
                     (text-object-begin-in-root parent))
           :end (- (text-object-end-in-root child)
                   (text-object-begin-in-root parent))))
    ;; remove it from its previous parent
    (when old-parent
      (setf (text-object-children old-parent)
            (delete child (text-object-children old-parent))))
    ;; set its parent property
    (setf (text-object-parent child) parent)))

;; new-end-in-root is the desired new end position, relative to the document root.
;; new-pos is the new end position, relative to the parent of obj.
(defmethod text-object-extend-in-parent ((obj text-object) new-pos)
  (let* ((parent (text-object-parent obj))
         (old-end (text-object-end obj)))
    ;; proceed only if there is a parent and the new position actually extends the object.
    (when (and parent (> new-pos old-end))
      ;; update the object's region to the new end position.
      (setf (region-end (text-object-text-region obj)) new-pos)
      ;; update the cached text by re-slicing it from the parent's text
      ;; using the newly updated region.
      (setf (text-object-text obj)
            (subseq (text-object-text parent)
                    (text-object-begin obj)
                    (text-object-end obj)))
      ;; find any sibling objects that are now encompassed by the new, larger region.
      (let ((siblings-to-move
              (loop for sibling in (text-object-children parent)
                    ;; a sibling should be moved if it's not the object itself,
                    ;; and it starts at or after the old end boundary,
                    ;; and it ends before the new end boundary.
                    when (and (not (eq sibling obj))
                              (>= (text-object-begin sibling) old-end)
                              (<= (text-object-end sibling) new-pos))
                      collect sibling)))
        (dolist (sibling-to-move siblings-to-move)
          (text-object-move sibling-to-move obj))))))

;; TODO: optimize this, we dont need the position relative to the root to find
;; the position relative to an internal node, its mostly a shortcut. and the
;; functionality for finding position in root currently takes O(n), which should
;; also be optimized to O(1) anyway.
(defmethod relative-child-region ((parent text-object) (child text-object))
  (let ((parent-begin-in-root (text-object-begin-in-root parent))
        (parent-end-in-root (text-object-end-in-root parent))
        (child-begin-in-root (text-object-begin-in-root child))
        (child-end-in-root (text-object-end-in-root child)))
    (make-region :begin (- child-begin-in-root parent-begin-in-root)
                 :end (- child-end-in-root parent-begin-in-root))))