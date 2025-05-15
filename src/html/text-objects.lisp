(in-package :cltpt/html)

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:inline-math)
                                           (fmt (eql html)))
  (list :text (cltpt/latex:latex-fragment-to-html (cltpt/base:text-object-text obj) t)
        :reparse nil
        :recurse nil
        :escape nil))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:display-math)
                                           (fmt (eql html)))
  (list :text (cltpt/latex:latex-fragment-to-html
               (cltpt/base:text-object-text obj) nil)
        :reparse nil
        :recurse t
        :escape nil))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:latex-env)
                                           (fmt (eql html)))
  (list :text (cltpt/latex:latex-fragment-to-html
               (cltpt/base:text-object-text obj) nil)
        :reparse nil
        :recurse nil
        :escape nil))