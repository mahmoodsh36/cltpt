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

(defmethod region-length ((r region))
  (with-slots (begin end) r
    (- end begin)))

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
   ;; do we need this "contents" thing?
   (contents-region
    :initarg :contents-region
    :accessor text-object-contents-region
    :documentation "the bounds of the contents of the object (excluding opening/ending).")
   ;; (opening-region
   ;;  :initarg :opening-macro
   ;;  :accessor text-object-opening-region
   ;;  :documentation "the match that starts the object")
   ;; (closing-region
   ;;  :initarg :opening-macro
   ;;  :accessor text-object-closing-region
   ;;  :documentation "the match that ends the object"
   ;;  :initform nil)
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

;; returns a plist or a string, if string, converted with recursion and no "reparsing".
;; if plist, plist can contain
;; the keyword
;; :text the text to convert,
;; :reparse - whether to reparse the given :text, or if :text isnt provided the result of text-object-text, the default behavior is to recurse,
;; :reparse-region - the region of the text that is given that is reparsed, if at all,
;; :recurse - whether to convert children as well.
(defgeneric text-object-convert (text-obj backend)
  (:documentation "function that takes a cltpt text-object and converts it to the specificed backend. this function is invoked when converting, it should return two values, a string and a boolean indicating whether to handle the converting of its children or not."))

(defmethod text-object-convert ((obj text-object) backend)
  "default convert function."
  (if (text-object-property obj :eval-result)
      (list :text (format nil "~A" (text-object-property obj :eval-result)))
      (list :text (text-object-text obj)
            :recurse t
            :escape t)))

;; default init function will just set the text slot of the object
;; we are currently using `subseq' to extract the region from the text and store
;; a new sequence for every object, this is both slow and memory-consuming
(defmethod text-object-init ((text-obj text-object) str1 match)
  ;; text of text-object should only be the text that it encloses in its parent
  (setf (text-object-text text-obj)
        (getf (car match) :match))
  (setf (text-object-text-region text-obj)
        (make-region :begin (getf (car match) :begin)
                     :end (getf (car match) :end))))

(defmethod text-object-adjust-to-parent ((child text-object) (parent text-object))
  (region-decf (text-object-text-region child)
               (text-object-begin parent)))

(defmethod text-object-begin ((text-obj text-object))
  "where the text object begins."
  (region-begin (text-object-text-region text-obj)))

(defmethod text-object-end ((text-obj text-object))
  "where the text object ends relative to its parent."
  (region-end (text-object-text-region text-obj)))

(defmethod text-object-set-parent ((child text-object) (parent text-object))
  (if (text-object-parent child)
      (delete child (text-object-children (text-object-parent child))))
  (setf (text-object-parent child) parent)
  (push child (text-object-children parent)))

(defmethod print-object ((obj text-object) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A -> ~A ~A"
            (type-of obj)
            (if (slot-boundp obj 'text-region) (text-object-text-region obj) nil)
            (if (slot-boundp obj 'text) (str:prune 10 (text-object-text obj)) nil))))

;; this is actually the slowest way to traverse siblings
(defmethod text-object-next-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (< idx (1- (length (text-object-children parent))))
          (elt (text-object-children parent) (1+ idx)))))))

;; this is actually the slowest way to traverse siblings
(defmethod text-object-prev-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (and (numberp idx) (> idx 0))
          (elt (text-object-children parent) (1- idx)))))))

(defmethod text-object-finalize ((obj text-object))
  "default finalize function, does nothing."
  )

(defclass document (text-object)
  ()
  (:documentation "top-level text element."))

(defclass text-block (text-object)
  ()
  (:documentation "a text block."))

(defun make-block (&rest kws &key &allow-other-keys)
  (let* ((type1 (getf kws :type))
         (obj (make-instance 'text-block)))
    (setf (text-object-property obj :type) type1)
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

;; a utility function to reduce boilerplate for `text-object-convert' implementations
(defun wrap-contents-for-convert (text-obj preamble postamble)
  (let* ((text (concatenate 'string
                            preamble
                            (text-object-contents text-obj)
                            postamble))
         (inner-region (make-region :begin (length preamble)
                                    :end (- (length text) (length postamble)))))
    (list :text text
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

;; (defmethod text-object-contents-begin ((text-obj text-object))
;;   (if (text-object-closing-region text-obj)
;;       (region-length (text-object-opening-region text-obj))
;;       0))

;; (defmethod text-object-contents-end ((text-obj text-object))
;;   (if (text-object-closing-region text-obj)
;;       (- (region-begin (text-object-closing-region text-obj))
;;          (region-begin (text-object-opening-region text-obj)))
;;       (region-length (text-object-opening-region text-obj))))

;; (defmethod text-object-contents ((obj text-object))
;;   (subseq (text-object-text obj)
;;           (text-object-contents-begin obj)
;;           (text-object-contents-end obj)))

(defmethod text-object-contents-begin ((text-obj text-object))
  0)

(defmethod text-object-contents-end ((text-obj text-object))
  (region-end (text-object-text-region text-obj)))

(defmethod text-object-contents ((obj text-object))
  (subseq (text-object-text obj)
          (text-object-text-begin obj)
          (text-object-text-end obj)))

(defclass text-macro (text-object)
  ((rule
    :allocation :class
    :initform '(cltpt/combinator::consec
                (cltpt/combinator::literal "#")
                (:pattern (cltpt/combinator::lisp-sexp)
                 :id lisp-code)))))

(defun is-not-before-parenthesis (str1 pos match-str)
  (not (char= (char str1 (+ pos (length match-str)))
              #\()))

(defclass post-lexer-text-macro (text-object)
  ((rule
    :allocation :class
    :initform '(cltpt/combinator::consec
                (cltpt/combinator::literal "%")
                (:pattern (cltpt/combinator::lisp-sexp)
                 :id lisp-code)))))

(defun eval-post-lexer-macro (obj)
  ;; we cache results to avoid re-eval which could be slow
  (let ((eval-result (text-object-property obj :eval-result)))
    (if eval-result
        eval-result
        (let ((txt-to-eval (subseq (text-object-text obj) 1))
              (macro-eval-result)
              (*post-lexer-text-macro-dynamic-object* obj))
          (handler-case
              (eval-in-text-object-lexical-scope
               obj
               (lambda ()
                 (eval
                  (read-from-string
                   txt-to-eval))))
            (error (c)
              (format t "error while evaluating post-lexer macro ~A: ~A.~%" (text-object-text obj) c)
              (setf macro-eval-result 'broken))
            (:no-error (result1)
              (setf macro-eval-result result1)))
          (setf (text-object-property obj :eval-result) macro-eval-result)
          macro-eval-result))))

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
              :escape t
              :recurse nil))))

(defmethod text-object-convert ((obj post-lexer-text-macro) backend)
  (convert-post-lexer-macro-obj obj backend))

;; we need to "finalize" the classes to be able to use MOP, a temporary workaround..
(defparameter *finalized-map* (make-hash-table))
(defun ensure-finalized (mytype)
  (unless (gethash mytype *finalized-map*)
    (sb-mop:finalize-inheritance (find-class-faster mytype))
    (setf (gethash mytype *finalized-map*) t))
  t)

(defun text-object-rule-from-subclass (subclass)
  ;; we need to finalize it, otherwise it'll error out.
  (ensure-finalized subclass)
  ;; (sb-mop:finalize-inheritance (find-class-faster subclass))
  (let ((rule (slot-value (sb-mop:class-prototype (find-class-faster subclass)) 'rule)))
    (unless (plistp rule)
      (setf rule (list :pattern rule :id subclass)))
    (unless (getf rule :id)
      (setf (getf rule :id) subclass))
    rule))

(defun map-text-object (text-obj func)
  "traverse the text object tree starting at TEXT-OBJ."
  (funcall func text-obj)
  (dolist (child (text-object-children text-obj))
    (map-text-object child func)))

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