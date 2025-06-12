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
    (list 'pair "\\begin{%W}" "\\end{%W}"
          ;; :begin-to-hash #\\
          ;; :end-to-hash #\\
          ;; we need to make sure the text after begin_ and end_ is the same
          ;; :pair-predicate (lambda (str b-idx e-idx b-end e-end)
          ;;                   (let ((begin-str (subseq str b-idx b-end))
          ;;                         (end-str (subseq str e-idx e-end)))
          ;;                     (string= (subseq begin-str (length "\\begin{"))
          ;;                              (subseq end-str (length "\\end{")))))
          )))
  (:documentation "latex environment."))

;; do we need this?
;; (defmethod cltpt/base:text-object-convert ((obj latex-env) (fmt (eql latex)))
;;   (list :text (cltpt/base:text-object-text obj)
;;         :reparse nil
;;         :recurse nil
;;         :escape nil))