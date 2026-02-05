(in-package :cltpt/base)

;; dynamically bound when executing a post-lexer text-macro to give the context of the object
;; to the function that will be running
(defvar *post-lexer-text-macro-dynamic-object*)

(defclass text-object (cltpt/buffer:buffer)
  ((properties
    :initarg :properties
    :accessor text-object-properties
    :initform nil
    :documentation "other properties that the cltpt text-object may hold.")
   (text
    :initarg :text
    :initform nil
    :documentation "the text that the text-object holds. this may not be set.")
   (match
    :initarg :match
    :accessor text-object-match
    :documentation "the instance of `cltpt/combinator:match' that was found before the object was constructed.")
   (rule
    :accessor text-object-rule
    :allocation :class
    :initform nil
    :documentation "the matching method from `*matching-methods*' used to match against the text object."))
  (:documentation "cltpt objects base class."))

(defmethod text-object-parent ((obj text-object))
  (cltpt/buffer:buffer-parent obj))

(defmethod (setf text-object-parent) (value (obj text-object))
  (setf (cltpt/buffer:buffer-parent obj) value))

(defmethod text-object-children ((obj text-object))
  (cltpt/buffer:buffer-children obj))

(defmethod (setf text-object-children) (value (obj text-object))
  (setf (cltpt/buffer:buffer-children obj) value))

(defmethod text-object-text-region ((obj text-object))
  (cltpt/buffer:buffer-region obj))

(defmethod (setf text-object-text-region) (value (obj text-object))
  (setf (cltpt/buffer:buffer-region obj) value))

(defgeneric text-object-init (text-obj str1 match)
  (:documentation "this function is invoked by the parser,
STR1 is the string (or buffer) being parsed, MATCH is the matching text for the
object that was detected by the parser combinator."))

(defgeneric text-object-finalize (text-obj)
  (:documentation "this function is invoked by the parser once it is done.

the text object should finalize initialization or any other functionality.

work that is done in finalization should not have a compounded effect when
the function is applied multiple times to an unchanged text-object.
this has to be true for incremental changes (via `reparse-change-in-tree') to
work properly."))

(defgeneric text-object-ends-by (text-obj value)
  (:documentation "should return whether the value indicates the ending of the
object's region. you should just make it return a symbol like `end-type'."))

(defmethod text-object-ends-by ((text-obj text-object) value)
  (and (symbolp value) (string= value 'end)))

(defmethod text-object-property ((obj text-object) property &optional default)
  (getf (text-object-properties obj) property default))

(defmethod (setf text-object-property) (value (obj text-object) property &optional default)
  (setf (getf (text-object-properties obj) property) value))

(defgeneric text-object-convert (text-obj backend)
  (:documentation "returns a plist or a string, if string, converted with recursion and no \"reparsing\".
if plist, plist can contain the keywords:
  :text    - the text to convert,
  :recurse - whether to convert children as well."))

(defgeneric text-object-convert-options (text-obj backend)
  (:documentation "a set of options are available by the conversion function.
the options are:
- :remove-newline-after: whether to trim a new line that comes after the object during conversion."))

;; TODO: this is easy to optimize by storing said parent beforehand.
(defmethod nearest-parent-with-text-helper ((node text-object))
  (if (cltpt/buffer:buffer-own-text node)
      node
      (when (text-object-parent node)
        (nearest-parent-with-text-helper (text-object-parent node)))))
(defmethod nearest-parent-with-text ((node text-object))
  "given a text-object NODE, return closest parent with the `text' slot assigned."
  (when (text-object-parent node)
    (nearest-parent-with-text-helper (text-object-parent node))))

;; TODO: this is easy to optimize by storing said ancestor beforehand.
(defmethod nearest-ancestor-with-text ((node text-object))
  "given a text-object NODE, return closest ancestor (non-strict) with the `text' slot assigned."
  (nearest-parent-with-text-helper node))

;; TODO: easy to optimize by not having to find the root (logarithmic complexity),
;; and not having to run subseq (linear complexity).
(defmethod text-object-text ((obj text-object))
  "function to grab the text of a text object, dictated by `text-object-text-region'.

this function assumes one ancestor (perhaps the root) has the `text-object-text' slot
set correctly."
  (let ((self-text (cltpt/buffer:buffer-own-text obj)))
    (or self-text
        (let* ((ancestor (nearest-parent-with-text obj))
               (str (cltpt/buffer:buffer-own-text ancestor))
               (rel-region (relative-child-region ancestor obj))
               (obj-str (cltpt/buffer:region-text rel-region str)))
          obj-str))))

(defmethod text-object-text-length ((obj text-object))
  (length (text-object-text obj)))

;; TODO: this doesnt correctly handle cases where some object in the tree has the 'text' slot set.
;; perhaps in such a case we should be updating the text slot.
(defmethod text-object-change-text ((obj text-object)
                                    new-text
                                    &key (propagate t))
  "function to change the text the text-object OBJ corresponds to.

this function doesnt propagate the changes to the children, so using it without
taking care of children indicies would cause issues."
  (cltpt/buffer:buffer-replace
   obj
   0
   (cltpt/buffer:region-length (text-object-text-region obj))
   new-text
   :delegate nil
   :propagate propagate)
  new-text)

(defmethod (setf text-object-text) (new-text (obj text-object))
  (text-object-change-text obj new-text :propagate nil))

(defmethod text-object-force-set-text ((text-obj text-object) new-text)
  "set text slot without propagating changes upwards in the tree."
  (setf (slot-value text-obj 'cltpt/buffer::text) new-text))

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

;; default init function will just set a few slots in the object
(defmethod text-object-init ((text-obj text-object) str1 match)
  ;; we initialize objects to an absolute region with respect to the root, but that is modified
  ;; later by 'text-object-adjust-to-parent'.
  (setf (text-object-text-region text-obj)
        (cltpt/buffer:make-region
         :begin (cltpt/combinator:match-begin-absolute match)
         :end (cltpt/combinator:match-end-absolute match)))
  (setf (text-object-match text-obj) match))

(defmethod text-object-adjust-to-parent ((child text-object) (parent text-object))
  (cltpt/buffer:region-decf
   (text-object-text-region child)
   (text-object-begin-in-root parent)))

(defmethod text-object-begin ((text-obj text-object))
  "where the text object begins."
  (cltpt/buffer:region-begin (text-object-text-region text-obj)))

(defmethod text-object-end ((text-obj text-object))
  "where the text object ends relative to its parent."
  (cltpt/buffer:region-end (text-object-text-region text-obj)))

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
            (if (slot-boundp obj 'cltpt/buffer::region)
                (text-object-text-region obj)
                nil)
            (let ((ancestor (nearest-ancestor-with-text obj)))
              (when (and ancestor
                         (cltpt/buffer:buffer-own-text ancestor))
                (cltpt/str-utils:str-prune (text-object-text obj) 15))))))

;; this is actually the slowest way to traverse siblings
;; TODO: easy to optimize
(defmethod text-object-next-sibling ((obj text-object))
  (let ((parent (text-object-parent obj)))
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (< idx (1- (length (text-object-children parent))))
          (elt (text-object-children parent) (1+ idx)))))))

;; this is actually the slowest way to traverse siblings
;; TODO: easy to optimize
(defmethod text-object-prev-sibling ((obj text-object))
  (let ((parent (text-object-parent obj)))
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (and (numberp idx) (> idx 0))
          (elt (text-object-children parent) (1- idx)))))))

(defmethod text-object-finalize ((obj text-object))
  "default finalize function, does nothing."
  nil)

(defclass document (text-object)
  ((escapes
    :accessor document-escapes
    :initform nil
    :documentation "escape sequences detected during parsing.")
   (src-file
    :accessor document-src-file
    :initform nil
    :documentation "the file from which the document was parsed. may be nil."))
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
  ;; the :loop part is handled differently, we dont 'eval' it here.
  (let* ((obj (make-instance 'text-block)))
    (loop for (key value) on kws by #'cddr
          do (setf (text-object-property obj key) value))
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

(defun sort-text-objects (text-objects)
  "return TEXT-OBJECTS, sorted by starting point. uses copy-list to avoid destructive modification."
  (sort
   (copy-list text-objects)
   '<
   :key
   (lambda (obj)
     (cltpt/buffer:region-begin (text-object-text-region obj)))))

(defmethod text-object-sorted-children ((obj text-object))
  "return the children of the text-obj, sorted by starting point."
  (sort-text-objects (text-object-children obj)))

;; this is helpful for being able to work similarly with different text-object types that each
;; have their own way of defining boundaries of "contents"
(defmethod compute-region-from-spec ((text-obj text-object) spec)
  "compute a region from a contents-region-spec and the object's match.
SPEC is a plist with keys:
:begin-submatch - submatch name for begin position (optional, use :self for main match)
:begin-side     - :start or :end (which side of the submatch to use)
:end-submatch   - submatch name for end position (optional, use :self for main match)
:end-side       - :start or :end
:compress       - amount to compress the region (optional)
:find-last-end  - if t, use find-submatch-last for end-submatch"
  ;; guard against unbound match slot - return default region
  (let* ((match (when (slot-boundp text-obj 'match)
                  (text-object-match text-obj))))
    (unless match
      (return-from compute-region-from-spec
        (let ((region (cltpt/buffer:make-region
                       :begin 0
                       :end (cltpt/buffer:region-length (text-object-text-region text-obj)))))
          (if (getf spec :compress)
              (cltpt/buffer:region-compress
               region
               (getf spec :compress)
               (getf spec :compress))
              region))))
    (let* ((match-begin-abs (cltpt/combinator:match-begin-absolute match))
           (begin-submatch-name (getf spec :begin-submatch))
           (begin-side (getf spec :begin-side :end))
           (end-submatch-name (getf spec :end-submatch))
           (end-side (getf spec :end-side :start))
           (compress (getf spec :compress))
           (find-last-end (getf spec :find-last-end))
           (begin-submatch (cond
                             ((eq begin-submatch-name :self) match)
                             (begin-submatch-name
                              (cltpt/combinator:find-submatch match begin-submatch-name))
                             (t nil)))
           (end-submatch (cond
                           ((eq end-submatch-name :self) match)
                           (end-submatch-name
                            (if find-last-end
                                (cltpt/combinator:find-submatch-last match end-submatch-name)
                                (cltpt/combinator:find-submatch match end-submatch-name)))
                           (t nil)))
           ;; use absolute positions and subtract match-begin-abs to get text-relative positions
           (begin-pos (if begin-submatch
                          (- (if (eq begin-side :end)
                                 (cltpt/combinator:match-end-absolute begin-submatch)
                                 (cltpt/combinator:match-begin-absolute begin-submatch))
                             match-begin-abs)
                          0))
           (end-pos (if end-submatch
                        (- (if (eq end-side :end)
                               (cltpt/combinator:match-end-absolute end-submatch)
                               (cltpt/combinator:match-begin-absolute end-submatch))
                           match-begin-abs)
                        (cltpt/buffer:region-length (text-object-text-region text-obj))))
           (region (cltpt/buffer:make-region :begin begin-pos :end end-pos)))
      (if compress
          (cltpt/buffer:region-compress region compress compress)
          region))))

(defmethod text-object-contents-begin ((text-obj text-object))
  (let ((spec (text-object-property text-obj :contents-region-spec)))
    (if spec
        (cltpt/buffer:region-begin (compute-region-from-spec text-obj spec))
        (if (text-object-property text-obj :contents-region)
            (cltpt/buffer:region-begin (text-object-property text-obj :contents-region))
            0))))

(defmethod text-object-contents-end ((text-obj text-object))
  (let ((spec (text-object-property text-obj :contents-region-spec)))
    (if spec
        (cltpt/buffer:region-end (compute-region-from-spec text-obj spec))
        (if (text-object-property text-obj :contents-region)
            (or (cltpt/buffer:region-end (text-object-property text-obj :contents-region))
                (length (text-object-text text-obj)))
            (length (text-object-text text-obj))))))

(defmethod text-object-contents-region ((text-obj text-object))
  (cltpt/buffer:make-region
   :begin (text-object-contents-begin text-obj)
   :end (text-object-contents-end text-obj)))

(defmethod text-object-contents ((obj text-object))
  (cltpt/buffer:region-text (text-object-contents-region obj)
                            (text-object-text obj)))

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

(defvar *cache-post-lexer-macro-evals* nil)
(defun eval-post-lexer-macro (obj &optional force)
  ;; TODO: we can cache results to avoid re-eval which is log(n). i tried this
  ;; but it causes issues as the value sometimes seems to evaluate to 'broken during
  ;; conversion even though it shouldnt be. i really need to investigate this.
  ;; it probably has to do with objects being reparsed/recreated during conversion.
  (when (and *cache-post-lexer-macro-evals*
             (text-object-property obj :eval-result)
             (not force))
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
        (when (getf *debug* :parse)
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
        (list :text (convert-tree eval-result
                                  (getf *convert-info* :src-fmt)
                                  (getf *convert-info* :dest-fmt))
              :recurse nil
              :escape nil)
        (list :text (princ-to-string eval-result)
              :recurse t
              :escape nil))))

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
;; TODO: get rid of this, i dont like how i did it.
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

;; this is a workaround because MOP lookup is very slow.
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

;; (defun make-text-link (&rest kws &key &allow-other-keys)
;;   (let* ((id (getf kws :id))
;;          (text (getf kws :text))
;;          (obj (make-instance 'text-link)))
;;     (setf (text-object-property obj :id) id)
;;     (setf (text-object-property obj :text) text)
;;     (loop for (key value) on kws by #'cddr
;;           do (setf (text-object-property obj key) value))
;;     obj))

;; TODO: easy to optimize
;; inefficient :(, takes O(log(n)) when it should take O(1)
(defmethod text-object-begin-in-root ((text-obj text-object))
  "where the text object begins in the root-most parent."
  (cltpt/buffer:buffer-begin-absolute text-obj))

;; TODO: easy to optimize
(defmethod text-object-end-in-root ((text-obj text-object))
  "where the text object ends in the root-most parent."
  (cltpt/buffer:buffer-end-absolute text-obj))

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
               (unless (eq obj text-obj)
                 (push obj results))))
      (map-text-object text-obj #'handle))
    results))

(defmethod child-at-pos ((text-obj text-object) pos)
  "find the leaf-most child that encloses the index POS."
  (loop for child in (text-object-children text-obj)
        do (when (cltpt/buffer:region-contains (text-object-text-region child) pos)
             (return-from child-at-pos
               (if (text-object-children child)
                   (or
                    (child-at-pos
                     child
                     (- pos
                        (cltpt/buffer:region-begin
                         (text-object-text-region child))))
                    child)
                   child)))))

(defmethod text-object-clone ((text-obj text-object)
                              &key
                                (clone-parent t)
                                (clone-children t))
  (let ((new-obj (make-instance (class-of text-obj))))
    (if (and clone-parent (text-object-parent text-obj))
        (setf (text-object-parent new-obj)
              (text-object-clone
               (text-object-parent text-obj)
               :clone-children nil))
        (setf (text-object-parent new-obj)
              (text-object-parent text-obj)))
    (setf (text-object-properties new-obj)
          (copy-tree (text-object-properties text-obj)))
    (when (slot-boundp text-obj 'cltpt/buffer::text)
      (setf (slot-value new-obj 'cltpt/buffer::text)
            (copy-seq (text-object-text text-obj))))
    (setf (text-object-text-region new-obj)
          (cltpt/buffer:region-clone (text-object-text-region text-obj)))
    (if clone-children
        (setf (text-object-children new-obj)
              (loop for child in (text-object-children text-obj)
                    for cloned-child = (text-object-clone child :clone-parent nil)
                    do (setf (text-object-parent cloned-child) new-obj)
                    collect cloned-child))
        (setf (text-object-children new-obj)
              (text-object-children text-obj)))
    new-obj))

;; we need a specialized text-object-clone for 'document' to copy the 'escapes' and 'src-file' slot.
;; though theres currently not really a need for this
;; (defmethod text-object-clone :around ((text-obj document) &rest args)
;;   (let ((clone (call-next-method)))
;;     ;; we arent really deep cloning here tho.. if at all
;;     (setf (document-escapes clone) (document-escapes text-obj))
;;     (setf (document-src-file clone) (document-src-file text-obj))
;;     clone))

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
          (sorted-insert
           (text-object-children parent)
           child
           :key 'text-object-begin-in-root))
    ;; correctly re-set the region for the child
    (setf (text-object-text-region child)
          (cltpt/buffer:make-region
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
      (setf (cltpt/buffer:region-end (text-object-text-region obj)) new-pos)
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
    (cltpt/buffer:make-region
     :begin (- child-begin-in-root parent-begin-in-root)
     :end (- child-end-in-root parent-begin-in-root))))

(defmethod text-object-root ((tree text-object))
  "given a text-object TREE, return the text-object at the root."
  (if (text-object-parent tree)
      (text-object-root (text-object-parent tree))
      tree))

;; TODO: easy to optimize
(defmethod text-object-text-region-in-root ((obj text-object))
  (cltpt/buffer:make-region
   :begin (text-object-begin-in-root obj)
   :end (text-object-end-in-root obj)))

;; this is a utility for reducing boilerplate for `text-object-convert' functionality
(defun rewrap-within-tags (text-obj open-tag close-tag
                           &key
                             reparse
                             (escape t)
                             compress-region
                             escape-region-options
                             open-tag-args
                             close-tag-args)
  "change the tags wrapping the contents of a text object. this is used for conversion.

contents region is further compressed by COMPRESS-REGION if provided."
  (let* ((contents-region (text-object-contents-region text-obj))
         ;; this is the inner region after region shifts caused by buffer changes.
         (inner-region
           (if compress-region
               (cltpt/buffer:region-compress-by
                (cltpt/buffer:region-clone contents-region)
                compress-region)
               contents-region))
         (old-open-tag-region
           (cltpt/buffer:make-region
            :begin 0
            :end (cltpt/buffer:region-begin inner-region)))
         (old-close-tag-region
           (cltpt/buffer:make-region
            :begin (cltpt/buffer:region-end inner-region)
            :end (cltpt/buffer:region-length (text-object-text-region text-obj)))))
    (setf (cltpt/buffer/region:region-props inner-region) escape-region-options)
    (list :changes (list (cltpt/buffer:make-change :operator open-tag
                                                   :region old-open-tag-region
                                                   :args open-tag-args)
                         (cltpt/buffer:make-change :operator close-tag
                                                   :region old-close-tag-region
                                                   :args close-tag-args))
          :recurse t
          :reparse reparse
          :escape escape
          :escape-regions (list inner-region))))

(defclass text-link (text-object)
  ((link
    :accessor text-link-link
    :initform nil
    :documentation "the link object of the text-link."))
  (:documentation "base text-object for links."))

(defmethod text-object-init :after ((obj text-link) str1 match)
  (let* ((link-type-match (cltpt/combinator:find-submatch match 'link-type))
         (link-dest-match (cltpt/combinator:find-submatch match 'link-dest))
         (link-desc-match (cltpt/combinator:find-submatch match 'link-desc))
         (type-str (string-upcase (and link-type-match
                                       (cltpt/combinator:match-text link-type-match str1)))))
    (setf (text-link-link obj)
          (make-link :type (when type-str (intern type-str :cltpt/base))
                     :desc (and link-desc-match
                                (cltpt/combinator:match-text link-desc-match str1))
                     :dest (cltpt/combinator:match-text link-dest-match str1)))
    (setf (text-object-property obj :is-inline) t)))

(defmethod text-link-resolve ((obj text-link))
  (let* ((link (text-link-link obj))
         (type (or (link-type link) 'file))
         (dest (link-dest link))
         (desc (link-desc link))
         (resolved (link-resolve obj type dest desc)))
    resolved))

(defmethod text-object-find-submatch ((obj text-object) submatch-id)
  "find a match that belongs to OBJ or to one of its children."
  (cltpt/tree:tree-map
   obj
   (lambda (child)
     (let ((result (cltpt/combinator:find-submatch (text-object-match child) submatch-id)))
       (when result
         (return-from text-object-find-submatch result)))))
  nil)

(defmethod text-object-match-text ((obj text-object) (match cltpt/combinator:match))
  "get text from MATCH relative to text object OBJ or string STR."
  (when match
    (let ((full-text (text-object-text obj))
          (ascendant-match (text-object-match obj)))
      (cltpt/buffer:region-text
       (cltpt/buffer:make-region
        ;; TODO: use a function that returns the bounds of the descendant match relative
        ;; to the ascendant instead of doing this manually. this functionality should be
        ;; present in the buffer structure which matches inherit from.
        :begin (- (cltpt/combinator:match-begin-absolute match)
                  (cltpt/combinator:match-begin-absolute ascendant-match))
        :end (- (cltpt/combinator:match-end-absolute match)
                (cltpt/combinator:match-begin-absolute ascendant-match)))
       full-text))))

(defmethod find-child-enclosing-region ((obj text-object) (r cltpt/buffer:region))
  (loop for child in (text-object-children obj)
        do (when (cltpt/buffer:region-encloses (text-object-text-region child) r)
             (return-from find-child-enclosing-region
               (if (text-object-children child)
                   (or
                    (find-child-enclosing-region
                     child
                     (let ((new-region (cltpt/buffer:region-clone r)))
                       (cltpt/buffer:region-decf
                        new-region
                        (cltpt/buffer:region-begin (text-object-text-region child)))
                       new-region))
                    child)
                   child))))
  obj)

(defmethod find-child-enclosing-region-strict ((obj text-object) (r cltpt/buffer:region))
  (loop for child in (text-object-children obj)
        do (when (cltpt/buffer:region-encloses-strict (text-object-text-region child) r)
             (return-from find-child-enclosing-region-strict
               (if (text-object-children child)
                   (or
                    (find-child-enclosing-region-strict
                     child
                     (let ((new-region (cltpt/buffer:region-clone r)))
                       (cltpt/buffer:region-decf
                        new-region
                        (cltpt/buffer:region-begin (text-object-text-region child)))
                       new-region))
                    child)
                   child))))
  obj)

(defmethod find-ancestor ((text-obj text-object) pred)
  "find an ancestor based on the given predicate PRED."
  (if (funcall pred text-obj)
      text-obj
      (when (text-object-parent text-obj)
        (find-ancestor (text-object-parent text-obj) pred))))