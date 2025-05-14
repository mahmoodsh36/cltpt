(defpackage :cltpt/extras
  (:use :cl)
  (:import-from :cltpt/base :text-block
                :text-object-property :wrap-contents-for-convert
                :text-object-convert :pcase)
  (:import-from :cltpt/latex :latex)
  (:import-from :cltpt/html :html))
(in-package :cltpt/extras)

;; some stuff needs to be separated from the base code because it is dependent
;; on other modules, i will keep them here for now until i find a better way
;; to handle them.

(defmethod text-object-convert ((obj text-block) backend)
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