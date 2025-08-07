(defpackage :cltpt/zoo
  (:use :cl)
  (:import-from :cltpt/base :text-block
                :text-object-property :wrap-contents-for-convert
                :text-object-convert :pcase)
  (:import-from :cltpt/latex :latex)
  (:import-from :cltpt/html :html)
  (:export))
(in-package :cltpt/zoo)

;; some stuff needs to be separated from the base code because it is dependent
;; on other modules, i will keep them here for now until i find a better way
;; to handle them.

(defmethod cltpt/base:text-object-convert ((obj text-block) backend)
  ;; use string on type to ensure its not a symbol
  (let ((type1 (string-downcase (string (text-object-property obj :type)))))
    (pcase backend
      (latex
       (wrap-contents-for-convert obj
                                  (format nil "\\begin{~A}" type1)
                                  (format nil "\\end{~A}" type1)))
      (html
       (wrap-contents-for-convert obj
                                  (format nil "<~A>" type1)
                                  (format nil "</~A>" type1))))))

(defun latex-fragment-to-html (latex-code is-inline)
  (case cltpt/html::*html-export-latex-method*
    (cltpt/html::svg
     (let ((img-filepath))
       (let ((img-filepath (cdar (cltpt/latex:generate-svgs-for-latex
                                  (list latex-code)))))
         (if is-inline
             (format nil
                     "<img src='~A' class='inline-math' />"
                     img-filepath)
             (format nil
                     "<br><img src='~A' class='display-math' /><br>"
                     img-filepath)))))))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:inline-math)
                                           (fmt (eql cltpt/html:html)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) t)
        :recurse nil
        :escape nil))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:display-math)
                                           (fmt (eql cltpt/html:html)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) nil)
        :recurse nil
        :escape nil))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:latex-env)
                                           (fmt (eql cltpt/html:html)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) nil)
        :recurse nil
        :escape nil))