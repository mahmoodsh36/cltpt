(in-package :cltpt/latex)

(defclass inline-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(cltpt/combinator:pair
                (cltpt/combinator:literal "\\(")
                (cltpt/combinator:literal "\\)")))))

(defmethod cltpt/base:text-object-convert ((obj inline-math) (fmt (eql latex)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse t
        :escape nil))

(defclass display-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(cltpt/combinator:pair
                (cltpt/combinator:literal "\\[")
                (cltpt/combinator:literal "\\]")))))

(defmethod cltpt/base:text-object-convert ((obj display-math)
                                           (fmt (eql latex)))
  (list :text (cltpt/base:text-object-text obj)
        :reparse nil
        :recurse t
        :escape nil))

(defclass latex-env (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform
    (list 'pair "\\begin{%W}" "\\end{%W}")))
  (:documentation "latex environment."))

(defclass latex-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform '(cltpt/combinator:consec
                "\\ref{"
                (:pattern (cltpt/combinator::symbol-matcher)
                 :id link-dest)
                "}")))
  (:documentation "latex link."))

;; do we need this?
;; (defmethod cltpt/base:text-object-convert ((obj latex-env) (fmt (eql latex)))
;;   (list :text (cltpt/base:text-object-text obj)
;;         :reparse nil
;;         :recurse nil
;;         :escape nil))