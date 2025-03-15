(in-package :cltpt)

;; A
(defun org-list-to-html (org-forest)
  (org-list-to-html-list org-forest))
(defun org-list-to-html-item (cons-item)
  (let* ((node (car cons-item))
         (text (getf node :text))
         (children (cdr cons-item)))
    (format nil "<li>~A~A</li>"
            text
            (if children (org-list-to-html-list children) ""))))
(defun org-list-to-html-list (forest)
  (format nil "<ul>~{~A~}</ul>" (mapcar #'org-list-to-html-item forest)))