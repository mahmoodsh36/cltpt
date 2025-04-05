(in-package :cltpt)

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
   (opening-region
    :initarg :opening-macro
    :accessor text-object-opening-region
    :documentation "the match that starts the object")
   (closing-region
    :initarg :opening-macro
    :accessor text-object-closing-region
    :documentation "the match that ends the object"
    :initform nil)
   (rule
    :accessor text-object-rule
    :allocation :class
    :initform nil
    :documentation "the matching method from `*matching-methods*' used to match against the text object."))
  (:documentation "cltpt objects base class"))

(defgeneric text-object-init (text-obj str1 opening-region closing-region)
  (:documentation "this function is invoked by the parser,
STR1 is the string (or buffer) being parsed, OPENING-REGION is the region from STR1
that resulted in this object being constructed, if the object was matched by a pair-matching
algorithm, CLOSING-REGION would be the region in which the clsoing pair resides in STR1."))

(defgeneric text-object-finalize (text-obj)
  (:documentation "this function is invoked by the parser once it is done.
the text object should finalize initialization or any other functionality."))

(defgeneric text-object-ends-by (text-obj value)
  (:documentation "should return whether the value indicates the ending of the object's
region. you should just make it return a symbol like `end-type'."))

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

;; returns a plist or a string, if string, exported with recursion and no "reparsing", if plist, plist can contain
;; the keyword
;; :text the text to export,
;; :reparse - whether to reparse the given :text, or if :text isnt provided the result of text-object-text, the default behavior is to recurse,
;; :reparse-region - the region of the text that is given that is reparsed, if at all,
;; :recurse - whether to export children as well.
(defgeneric text-object-export (text-obj backend)
  (:documentation "function that takes a cltpt text-object and exports it to the specificed backend. this function is invoked when exporting, it should return two values, a string and a boolean indicating whether to handle the exporting of its children or not."))

(defmethod text-object-export ((obj text-object) backend)
  "default export function."
  (if (text-object-property obj :eval-result)
      (list :text (format nil "~A" (text-object-property obj :eval-result)))
      (list :text (text-object-text obj)
            :recurse t
            :escape t)))

;; default init function will just set the text slot of the object
;; we are currently using `subseq' to extract the region from the text and store
;; a new sequence for every object, this is both slow and memory-consuming
(defmethod text-object-init ((text-obj text-object) str1 opening-region closing-region)
  (setf (text-object-opening-region text-obj) opening-region)
  (setf (text-object-closing-region text-obj) closing-region)
  ;; text of text-object should only be the text that it encloses in its parent
  (setf (text-object-text text-obj)
        (subseq str1
                (region-begin opening-region)
                (if closing-region
                    (region-end closing-region)
                    (region-end opening-region)))))

(defmethod text-object-adjust-to-parent ((child text-object) (parent text-object))
  (region-decf (text-object-opening-region child)
               (text-object-begin parent))
  (when (text-object-closing-region child)
    (region-decf (text-object-closing-region child)
                 (text-object-begin parent))))

(defmethod text-object-begin ((text-obj text-object))
  "where the text object begins relative to its parent."
  (region-begin (text-object-opening-region text-obj)))

(defmethod text-object-end ((text-obj text-object))
  "where the text object ends relative to its parent."
  (if (text-object-closing-region text-obj)
      (region-end (text-object-closing-region text-obj))
      (region-end (text-object-opening-region text-obj))))

(defmethod text-object-set-parent ((child text-object) (parent text-object))
  (if (text-object-parent child)
      (delete child (text-object-children (text-object-parent child))))
  (setf (text-object-parent child) parent)
  (push child (text-object-children parent)))

(defmethod print-object ((obj text-object) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A -> ~A"
            (if (slot-boundp obj 'opening-region) (text-object-opening-region obj) nil)
            (if (slot-boundp obj 'closing-region) (text-object-closing-region obj) nil))))

;; this is actually the slowest way to traverse siblings
(defmethod text-object-next-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (< idx (1- (length (text-object-children parent))))
          (elt (text-object-children parent) (1+ idx)))))))

(defmethod text-object-prev-sibling ((obj text-object))
  (with-slots (parent) obj
    (when parent
      (let ((idx (position obj (text-object-children parent))))
        (when (> 0 idx)
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

(defmethod text-object-export ((obj text-block) backend)
  ;; use string on type to ensure its not a symbol
  (let ((type1 (string-downcase (string (text-object-property obj :type)))))
    (case backend
      ('latex
       (wrap-contents-for-export obj
                                 (format nil "\\begin{~A}" type1)
                                 (format nil "\\end{~A}" type1)))
      ('html
       (wrap-contents-for-export obj
                                 (format nil "<~A>" type1)
                                 (format nil "</~A>" type1))))))

;; aliases for blocks
(setf (symbol-function 'b) (symbol-function 'make-block))
(setf (symbol-function '/b) (symbol-function 'block-end))

(defmethod text-object-ends-by ((text-obj text-block) value)
  (and (symbolp value) (string= value 'block-end)))

;; a utility function to reduce boilerplate for `text-object-export' implementations
(defun wrap-contents-for-export (text-obj preamble postamble)
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
     (region-begin (text-object-opening-region obj)))))

(defmethod text-object-sorted-children (obj)
  "return the children of the text-obj, sorted by starting point."
  (sort-text-objects (text-object-children obj)))

;; if we have a closing region, contents are between opening and closing region,
;; otherwise contents are actually just the opening region.
;; opening-region is the opening region positions relative to parent text only. likewise
;; for closing-region.
(defmethod text-object-contents-begin ((text-obj text-object))
  (if (text-object-closing-region text-obj)
      (region-length (text-object-opening-region text-obj))
      0))

(defmethod text-object-contents-end ((text-obj text-object))
  (if (text-object-closing-region text-obj)
      (- (region-begin (text-object-closing-region text-obj))
         (region-begin (text-object-opening-region text-obj)))
      (region-length (text-object-opening-region text-obj))))

(defmethod text-object-contents ((obj text-object))
  (subseq (text-object-text obj)
          (text-object-contents-begin obj)
          (text-object-contents-end obj)))

;; for macro executions that return objects
(defvar *lexer-text-macro-char* #\#)
;; for macro executions that are supposed to run the code after tree construction (text objects
;; already constructed)
(defvar *post-lexer-text-macro-char* #\◊)

(defclass text-macro (text-object)
  ((rule
    :allocation :class
    :initform `(:begin (:pattern (%or (:pattern ,(format nil "~A(%W-)(" *lexer-text-macro-char*))
                                      (:string ,(format nil "~A(" *lexer-text-macro-char*))))
                :end (:string ")")
                :children ((:begin (:string "(")
                            :end (:string ")")
                            :id ,(gensym)
                            :children ((:begin (:string "\"")
                                        :end (:string "\"")
                                        :id ,(gensym)
                                        :disallow t)))
                           (:begin (:string "\"")
                            :end (:string "\"")
                            :id ,(gensym)
                            :disallow t))
                ;; :begin-to-hash t
                :end-to-hash t))))

(defun is-not-before-parenthesis (str1 pos match-str)
  (not (char= (char str1 (+ pos (length match-str)))
              #\()))

;; the "ref" thing is here for now because we cannot match all macros of the form #symbol(code) in one rule
(defclass text-macro-ref (text-object)
  ((rule
    :allocation :class
    :initform `(:text (:pattern ,(format nil "~A(%W-)" *lexer-text-macro-char*))
                :text-conditions (is-not-before-parenthesis)
                :disallow t))))

(defclass post-lexer-text-macro (text-object)
  ((rule
    :allocation :class
    :initform `(:begin (:string ,(format nil "~A(" *post-lexer-text-macro-char*))
                :end (:string ")")
                :begin-to-hash t
                :end-to-hash t
                :disallow t))))

(defclass post-lexer-text-macro-ref (text-object)
  ((rule
    :allocation :class
    :initform `(:text (:pattern ,(format nil "~A(%W-)" *post-lexer-text-macro-char*))
                :disallow t))))

(defun eval-post-lexer-macro (obj)
  (let ((txt-to-eval (subseq (text-object-text obj) 1))
        (macro-eval-result))
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
    macro-eval-result))

(defun eval-in-text-object-lexical-scope (obj func)
  (let ((parent (text-object-parent obj)))
    (if parent
        (eval-in-text-object-lexical-scope
         parent
         (lambda ()
           (let ((binds (text-object-property obj :let)))
             (bind-and-run binds func))))
        (let ((binds (text-object-property obj :let)))
          (bind-and-run binds func)))))

(defun export-post-lexer-macro-obj (obj)
  (let ((eval-result (princ-to-string (eval-post-lexer-macro obj))))
    (list :text eval-result
          :escape t
          :recurse nil)))

(defmethod text-object-export ((obj post-lexer-text-macro) backend)
  (export-post-lexer-macro-obj obj))

(defmethod text-object-export ((obj post-lexer-text-macro-ref) backend)
  (export-post-lexer-macro-obj obj))

;; define the inline-math subclass with its own default rule
(defclass inline-math (text-object)
  ((rule
    :allocation :class
    :initform '(:begin (:string "\\(")
                :end (:string "\\)")
                :begin-to-hash t
                :end-to-hash t))))

(defmethod text-object-export ((obj inline-math) backend)
  (case backend
    ('latex
     (list :text (text-object-text obj)
           :recurse t
           :escape nil))
    ('html
     (list :text (latex-fragment-to-html (text-object-text obj) t)
           :reparse nil
           :recurse nil
           :escape nil))))

(defclass display-math (text-object)
  ((rule
    :allocation :class
    :initform '(:begin (:string "\\[")
                :end (:string "\\]")
                :begin-to-hash t
                :end-to-hash t))))

(defmethod text-object-export ((obj display-math) backend)
  (case backend
    ('latex
     (list :text (text-object-text obj)
           :reparse nil
           :recurse t
           :escape nil))
    ('html
     (list :text (latex-fragment-to-html (text-object-text obj) nil)
           :reparse nil
           :recurse t
           :escape nil))))

(defclass latex-env-slow (text-object)
  ((rule
    :allocation :class
    :initform
    (list :begin '(:regex "\\\\begin{[a-z\\*]+}")
          :end '(:regex "\\\\end{[a-z\\*]+}")
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str (length "\\begin{"))
                                       (subseq end-str (length "\\end{"))))))))
  (:documentation "latex environment."))

(defclass latex-env (text-object)
  ((rule
    :allocation :class
    :initform
    (list :begin '(:pattern "\\begin{(%E:{})}")
          :end '(:pattern "\\end{(%E:{})}")
          :begin-to-hash t
          :end-to-hash t
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str (length "\\begin{"))
                                       (subseq end-str (length "\\end{"))))))))
  (:documentation "latex environment."))

(defmethod text-object-export ((obj latex-env) backend)
  (case backend
    ('latex
     (list :text (text-object-text obj)
           :reparse nil
           :recurse nil
           :escape nil))
    ('html
     (list :text (latex-fragment-to-html (text-object-text obj) nil)
           :reparse nil
           :recurse nil
           :escape nil))))

(defun text-object-rule-from-subclass (subclass)
  (slot-value (sb-mop:class-prototype (find-class-faster subclass)) 'rule))

(defun map-text-object (text-obj func)
  "traverse the text object tree starting at TEXT-OBJ."
  (funcall func text-obj)
  (dolist (child (text-object-children text-obj))
    (map-text-object child func)))

;; we need to "finalize" the classes to be able to use MOP
(defun ensure-finalized ()
  (dolist (mytype *org-mode-text-object-types*)
    (sb-mop:finalize-inheritance (find-class-faster mytype))))