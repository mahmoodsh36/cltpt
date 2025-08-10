(in-package :cltpt/latex)

(defvar *inline-math-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:literal "\\(")
     (cltpt/combinator:literal "\\)"))
    :on-char #\\))
(defclass inline-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *inline-math-rule*)))

(defmethod cltpt/base:text-object-convert ((obj inline-math) (fmt (eql latex)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse t
        :escape nil))

(defvar *display-math-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:literal "\\[")
     (cltpt/combinator:literal "\\]"))
    :on-char #\\))
(defclass display-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *display-math-rule*)))

(defmethod cltpt/base:text-object-convert ((obj display-math)
                                           (fmt (eql latex)))
  (list :text (cltpt/base:text-object-text obj)
        :reparse nil
        :recurse t
        :escape nil))


(defvar *latex-env-rule*
  `(:pattern
    (cltpt/combinator:pair
     ,(cltpt/combinator:handle-rule-string "\\begin{%W}")
     ,(cltpt/combinator:handle-rule-string "\\end{%W}"))
    :on-char #\\))
(defclass latex-env (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *latex-env-rule*))
  (:documentation "latex environment."))

(defclass latex-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform '(:pattern
                (cltpt/combinator:consec
                 "\\ref{"
                 (:pattern (cltpt/combinator::symbol-matcher)
                  :id link-dest)
                 "}")
                :on-char #\\)))
  (:documentation "latex link."))

(defmethod cltpt/base:text-object-convert ((obj latex-env) (fmt (eql latex)))
  (list :text (cltpt/base:text-object-text obj)
        :reparse nil
        :recurse nil
        :escape nil))