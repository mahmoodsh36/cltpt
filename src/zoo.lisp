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
                 type1))
        (list :text (cltpt/base:text-object-contents obj)
              :escape nil
              :reparse t))))

(defmethod cltpt/base:text-object-convert ((obj cltpt/base:text-block)
                                           backend)
  ;; use string on type to ensure its not a symbol
  (let ((loop-expr (cltpt/base:text-object-property obj :loop))
        ;; we set :not-to-loop in a recursive call, so that we dont loop
        ;; indefinitely by checking if we have set it already or not.
        (not-to-loop (cltpt/base:text-object-property obj :not-to-loop)))
    (if (and loop-expr (not not-to-loop))
        (let* ((var-name (car loop-expr))
               (var-values (cadr loop-expr))
               (result
                 (loop for var-value in var-values
                       for clone = (cltpt/base:text-object-clone obj)
                       collect (progn
                                 (setf (cltpt/base:text-object-property
                                        clone
                                        :not-to-loop)
                                       t)
                                 (setf (cltpt/base:text-object-property clone :let*)
                                       (list*
                                        (list var-name var-value)
                                        (cltpt/base:text-object-property
                                         clone
                                         :let*)))
                                 ;; for now this only supports macros,
                                 ;; TODO: inherit all text objects from the source
                                 ;; format.
                                 ;; TODO: currently inheriting :loop variables
                                 ;; doesnt work for some reason.
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
       (let ((img-filepath (cdar (cltpt/latex:generate-previews-for-latex
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
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (keywords-alist (cltpt/org-mode::handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption"))
         (latex-env-match (car (cltpt/combinator:find-submatch match 'cltpt/org-mode::latex-env-1)))
         (latex-env-contents (cltpt/combinator:match-text latex-env-match)))
    ;; TODO: handle \caption and \label properly.
    (list :text latex-env-contents
          :reparse nil
          :recurse nil
          :escape nil)))

(defmethod cltpt/base:text-object-convert ((obj cltpt/org-mode::org-latex-env)
                                           (fmt (eql cltpt/html:*html*)))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (keywords-alist (cltpt/org-mode::handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption"))
         (latex-env-match (car (cltpt/combinator:find-submatch match 'cltpt/org-mode::latex-env-1)))
         (latex-env-contents (cltpt/combinator:match-text latex-env-match)))
    ;; TODO: handle \caption and \label properly.
    (list :text (latex-fragment-to-html latex-env-contents nil)
          :recurse nil
          :reparse nil
          :escape nil
          :remove-newlines-after t)))

(defmethod cltpt/base:text-object-convert ((obj cltpt/base:text-link)
                                           (backend cltpt/base:text-format))
  (let* ((link (cltpt/base:text-link-link obj))
         (desc (cltpt/base:link-desc link))
         (type (if (cltpt/base:link-type link)
                   (intern (cltpt/base:link-type link))
                   'cltpt/base::file))
         (dest (cltpt/base:link-dest link))
         (final-desc (or desc dest))
         (open-tag)
         (close-tag)
         (resolved (cltpt/base:link-resolve type dest desc))
         (desc-match
           (cltpt/combinator:find-submatch
            (cltpt/base:text-object-normalized-combinator-match obj)
            'link-desc))
         (desc-begin (getf (car desc-match) :begin))
         (desc-end (getf (car desc-match) :end))
         (dest-filepath))
    (when dest
      (cltpt/base:pcase (type-of resolved)
        ('pathname
         (setf dest-filepath
               (cltpt/file-utils:ensure-filepath-string resolved)))
        ('cltpt/roam:node
         (setf dest-filepath (cltpt/roam:node-file resolved))))
      (when dest-filepath
        ;; initialize the tags to the <a> tag, if its a video or an image,
        ;; it gets overwritten later.
        (setf open-tag
              (cltpt/base:pcase backend
                (cltpt/html:*html*
                 (format nil
                         "<a href='~A'>"
                         dest-filepath))
                (cltpt/latex:*latex*
                 (format nil
                         "\\href{~A}{"
                         dest-filepath))))
        (setf close-tag
              (cltpt/base:pcase backend
                (cltpt/html:*html* "</a>")
                (cltpt/latex:*latex* "}")))
        ;; TODO: copy the destination file to the destination dir
        (when (cltpt/file-utils:file-has-extension-p
               dest-filepath
               (append cltpt/base:*image-ext* cltpt/base:*video-ext*))
          ;; if its an image we need to "display" it in html.
          (let ((new-path (cltpt/base:pcase backend
                            (cltpt/html:*html*
                             (if cltpt/html:*html-static-dir*
                                 (cltpt/file-utils:change-dir
                                  dest-filepath
                                  cltpt/html:*html-static-dir*)
                                 dest-filepath))
                            (t dest-filepath))))
            ;; we should check if its the same file, otherwise copy-file will break
            (unless (string= dest-filepath new-path)
              (when (uiop:probe-file* dest-filepath)
                (uiop:copy-file dest-filepath new-path)))
            (setf open-tag
                  (cltpt/base:pcase backend
                    (cltpt/html:*html*
                     (format nil
                             "<img src='~A'>"
                             dest-filepath))
                    (cltpt/latex:*latex*
                     (format nil
                             "\\href{~A}{"
                             dest-filepath))))
            (setf close-tag
                  (cltpt/base:pcase backend
                    (cltpt/html:*html* "</img>")
                    (cltpt/latex:*latex* "}")))))
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
                                :end desc-end)))))))

(defun init ()
  "function to run any necessary initialization code for cltpt.

it may be necessary to call this function after modifying some customization
variables such as `cltpt/org-mode::*org-enable-macros*'."
  (cltpt/html:init)
  (cltpt/org-mode:init)
  (cltpt/latex:init))

(init)