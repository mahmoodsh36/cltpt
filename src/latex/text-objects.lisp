(in-package :cltpt/latex)

(defclass inline-math (text-object)
  ((rule
    :allocation :class
    :initform '(:begin (literal "\\(")
                :end (literal "\\)")
                :begin-to-hash #\\
                :end-to-hash #\\))))

(defmethod text-object-convert ((obj inline-math) backend)
  (pcase backend
    (latex
     (list :text (text-object-text obj)
           :recurse t
           :escape nil))
    (html
     (list :text (latex-fragment-to-html (text-object-text obj) t)
           :reparse nil
           :recurse nil
           :escape nil))))

(defclass display-math (text-object)
  ((rule
    :allocation :class
    :initform '(:begin (literal "\\[")
                :end (literal "\\]")
                :begin-to-hash #\\
                :end-to-hash #\\))))

(defmethod text-object-convert ((obj display-math) backend)
  (pcase backend
    (latex
     (list :text (text-object-text obj)
           :reparse nil
           :recurse t
           :escape nil))
    (html
     (list :text (latex-fragment-to-html (text-object-text obj) nil)
           :reparse nil
           :recurse t
           :escape nil))))

(defclass latex-env (text-object)
  ((rule
    :allocation :class
    :initform
    (list :begin "\\begin{%W}"
          :end "\\end{%W}"
          :begin-to-hash #\\
          :end-to-hash #\\
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str (length "\\begin{"))
                                       (subseq end-str (length "\\end{"))))))))
  (:documentation "latex environment."))

(defmethod text-object-convert ((obj latex-env) backend)
  (pcase backend
    (latex
     (list :text (text-object-text obj)
           :reparse nil
           :recurse nil
           :escape nil))
    (html
     (list :text (latex-fragment-to-html (text-object-text obj) nil)
           :reparse nil
           :recurse nil
           :escape nil))))