(defpackage :cltpt/zoo
  (:use :cl)
  (:export :init))

(in-package :cltpt/zoo)

;; some stuff needs to be separated from the base code because it is dependent
;; on other modules, i will keep them here for now until i find a better way
;; to handle them.

(defun text-block-convert-helper (obj backend)
  (let ((type1 (cltpt/base:text-object-property obj :type)))
    (when type1
      (setf type1 (string-downcase (string type1))))
    (if type1
        (cltpt/base:rewrap-within-tags
         obj
         (format nil
                 (cltpt/base:pcase backend
                   (cltpt/latex:*latex*
                    "\\begin{~A}")
                   (cltpt/html:*html*
                    "<~A>"))
                 type1)
         (format nil
                 (cltpt/base:pcase backend
                   (cltpt/latex:*latex*
                    "\\end{~A}")
                   (cltpt/html:*html*
                    "</~A>"))
                 type1)
         :escape (cltpt/base:text-object-property obj :escape t))
        (cltpt/base:rewrap-within-tags
         obj
         ""
         ""
         :escape (cltpt/base:text-object-property obj :escape t)))))

;; TODO: conversion with :loop doesnt work for nested text-block's with :loop
(defmethod cltpt/base:text-object-convert ((obj cltpt/base:text-block)
                                           backend)
  ;; use string on type to ensure its not a symbol
  (let ((loop-expr (cltpt/base:text-object-property obj :loop))
        ;; we set :not-to-loop in a recursive call, so that we dont loop
        ;; indefinitely by checking if we have set it already or not.
        (not-to-loop (cltpt/base:text-object-property obj :not-to-loop)))
    (if (and loop-expr (not not-to-loop))
        (let* ((var-name (car loop-expr))
               (var-values
                 (cltpt/base:eval-in-text-object-lexical-scope
                  obj
                  (lambda ()
                    (eval (cadr loop-expr)))))
               (result
                 (loop for var-value in var-values
                       for i from 1
                       for clone = (cltpt/base:text-object-clone obj)
                       do (progn
                            (setf (cltpt/base:text-object-property
                                   clone
                                   :not-to-loop)
                                  t)
                            (setf (cltpt/base:text-object-property clone :let*)
                                  (list*
                                   ;; TODO: this is a hack, that we're wrapping
                                   ;; the var-value with a call to 'identity'
                                   ;; to prevent bind-and-eval from running
                                   ;; eval on var-value itself.
                                   (list var-name `,var-value)
                                   (cltpt/base:text-object-property
                                    clone
                                    :let*))))
                       collect (progn
                                 ;; (setf (cltpt/base:text-object-parent clone)
                                 ;;       (cltpt/base:text-object-parent obj))
                                 ;; (cltpt/base::text-object-change-text
                                 ;;  clone
                                 ;;  (cltpt/base:text-object-text obj)
                                 ;;  :propagate nil)
                                 (cltpt/base:convert-tree
                                  clone
                                  (getf cltpt/base:*convert-info* :src-fmt)
                                  backend)))))
          (list :text (cltpt/base:concat result)
                :recurse nil
                :reparse nil
                :escape nil))
        (text-block-convert-helper obj backend))))

(defun latex-fragment-to-html (latex-code is-inline)
  (case cltpt/html:*html-export-latex-method*
    (cltpt/html::svg
     (let ((img-filepath))
(let ((img-filepath (cdar (cltpt/latex-previews:generate-previews-for-latex
                                   (list latex-code)))))
         (if is-inline
             (format nil
                     "<img src='~A' class='inline-math' />"
                     img-filepath)
             (format nil
                     "<img src='~A' class='display-math' />"
                     img-filepath)))))))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:inline-math)
                                           (fmt (eql cltpt/html:*html*)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) t)
        :recurse t
        :reparse nil
        :escape nil))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:display-math)
                                           (fmt (eql cltpt/html:*html*)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) nil)
        :recurse nil
        :reparse nil
        :escape nil
        :remove-newlines-after t))

(defmethod cltpt/base:text-object-convert ((obj cltpt/latex:latex-env)
                                           (fmt (eql cltpt/html:*html*)))
  (list :text (latex-fragment-to-html (cltpt/base:text-object-text obj) nil)
        :recurse nil
        :reparse nil
        :escape nil
        :remove-newlines-after t))

(defmethod cltpt/base:text-object-convert ((obj cltpt/org-mode::org-latex-env)
                                           (fmt (eql cltpt/latex:*latex*)))
  (let* ((match (cltpt/base:text-object-match obj))
         (keywords-alist (cltpt/org-mode::handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption"))
         (latex-env-match (cltpt/combinator:find-submatch match 'cltpt/org-mode::latex-env-1))
         (latex-env-contents (cltpt/combinator:match-text latex-env-match)))
    ;; TODO: handle \caption and \label properly.
    (list :text latex-env-contents
          :reparse nil
          :recurse nil
          :escape nil)))

(defmethod cltpt/base:text-object-convert ((obj cltpt/org-mode::org-latex-env)
                                           (fmt (eql cltpt/html:*html*)))
  (let* ((match (cltpt/base:text-object-match obj))
         (keywords-alist (cltpt/org-mode::handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption"))
         (latex-env-match (cltpt/combinator:find-submatch match 'cltpt/org-mode::latex-env-1))
         (latex-env-contents (cltpt/combinator:match-text latex-env-match)))
    ;; TODO: handle \caption and \label properly.
    (list :text (latex-fragment-to-html latex-env-contents nil)
          :recurse nil
          :reparse nil
          :escape nil
          :remove-newlines-after t)))

(defmethod cltpt/base:convert-target-filepath ((target pathname))
  (cltpt/base:convert-target-filepath (cltpt/file-utils:ensure-filepath-string target)))

(defmethod cltpt/base:convert-target-filepath ((target string))
  (let* ((static-ext (append cltpt/base:*image-ext* cltpt/base:*video-ext*))
         (dest-filepath (cltpt/base:target-filepath target))
         (filepath-format-string (getf cltpt/base:*convert-info* :static-filepath-format))
         (new-filepath (if (and dest-filepath filepath-format-string)
                           (cltpt/base:filepath-format dest-filepath filepath-format-string)
                           dest-filepath)))
    new-filepath))

(defmethod cltpt/base:text-object-convert ((obj cltpt/base:text-link)
                                           (backend cltpt/base:text-format))
  (let* ((link (cltpt/base:text-link-link obj))
         (desc (cltpt/base:link-desc link))
         (dest (cltpt/base:link-dest link))
         (final-desc (or desc dest))
         (open-tag)
         (close-tag)
         (resolved (cltpt/base:text-link-resolve obj))
         (desc-match
           (cltpt/combinator:find-submatch
            (cltpt/base:text-object-normalized-match obj)
            'link-desc))
         (desc-begin (when desc-match
                       (cltpt/combinator:match-begin desc-match)))
         (desc-end (when desc-match
                     (cltpt/combinator:match-end desc-match)))
         (dest-filepath (when resolved
                          (cltpt/base:target-filepath resolved)))
         (new-filepath
           (when resolved
             (if (getf cltpt/base:*convert-info* :dest-dir)
                 (cltpt/file-utils:join-paths (getf cltpt/base:*convert-info* :dest-dir)
                                              (cltpt/base:convert-target-filepath resolved))
                 (cltpt/base:convert-target-filepath resolved))))
         ;; TODO/FIXME: this will not work when dest-filepath is a directory path that ends with
         ;; a forward slash.
         (inserted-filepath
           (or (when new-filepath
                 (if (and (eq backend cltpt/html:*html*) cltpt/html:*html-static-route*)
                     ;; if its html we should respect *html-static-route*
                     (cltpt/file-utils:join-paths
                      cltpt/html:*html-static-route*
                      (cltpt/file-utils:file-basename new-filepath))
                     (cltpt/file-utils:file-basename new-filepath)))
               dest-filepath))
         (static-ext (append cltpt/base:*image-ext* cltpt/base:*video-ext*)))
    (when dest-filepath
      ;; initialize the tags to the <a> tag, if its a video or an image,
      ;; it gets overwritten later.
      (setf open-tag
            (cltpt/base:pcase backend
              (cltpt/html:*html*
               (format nil
                       "<a href='~A'>"
                       inserted-filepath))
              (cltpt/latex:*latex*
               (format nil
                       "\\href{~A}{"
                       inserted-filepath))))
      (setf close-tag
            (cltpt/base:pcase backend
              (cltpt/html:*html* "</a>")
              (cltpt/latex:*latex* "}")))
      ;; TODO: copy the destination file to the destination dir
      (when (cltpt/file-utils:file-has-extension-p dest-filepath static-ext)
        ;; we should check if its the same file, otherwise copy-file will break
        (unless (string= dest-filepath new-filepath)
          (when (uiop:probe-file* dest-filepath)
            (ensure-directories-exist new-filepath)
            (uiop:copy-file dest-filepath new-filepath)))
        ;; if its an image or video we need to "display" it in html.
        (setf open-tag
              (cltpt/base:pcase backend
                (cltpt/html:*html*
                 (format nil
                         "<img src='~A' />"
                         inserted-filepath))
                (cltpt/latex:*latex*
                 (format nil
                         "\\href{~A}{"
                         inserted-filepath))))
        (setf close-tag
              (cltpt/base:pcase backend
                (cltpt/html:*html* "")
                (cltpt/latex:*latex* "}")))
        (setf final-desc ""))
      ;; when there's no description for the link the resulting text
      ;; would just be an html snippet with no need for escaping or recursing.
      (list :text (if desc-match
                      (cltpt/base:text-object-text obj)
                      (concatenate 'string
                                   open-tag
                                   final-desc
                                   close-tag))
            :changes (when desc-match
                       (list
                        (cons
                         open-tag
                         (cltpt/base:make-region
                          :begin 0
                          :end desc-begin))
                        (cons
                         final-desc
                         (cltpt/base:make-region
                          :begin desc-begin
                          :end desc-end))
                        (cons
                         close-tag
                         (cltpt/base:make-region
                          :begin desc-end
                          :end (length (cltpt/base:text-object-text obj))))))
            :recurse desc-match
            :escape-region (when desc-match
                             (cltpt/base:make-region
                              :begin desc-begin
                              :end desc-end))))))

(defun init ()
  "function to run any necessary initialization code for cltpt.

it may be necessary to call this function after modifying some customization
variables such as `cltpt/org-mode::*org-enable-macros*'."
  (cltpt/html:init)
  (cltpt/org-mode:init)
  (cltpt/latex:init))

(init)