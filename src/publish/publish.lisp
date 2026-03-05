(defpackage :cltpt/publish
  (:use :cl)
  (:export
   :title-to-filename
   :wrap-into-unescaping-obj
   :template-file-to-template-obj
   :render-post-list
   :render-post-items
   :node-to-href
   :publish
   :load-theme-by-name
   :*default-static-filepath-format*
   :*default-html-static-route*
   :*default-filepath-format*
   :convert-template* :convert-template))

(in-package :cltpt/publish)

(defvar *current-template-file* nil)

(defvar *theme-dir* nil)

(defun built-in-themes-dir ()
  "return the absolute path to the directory containing bundled themes."
  (uiop:native-namestring (asdf:system-relative-pathname :cltpt "src/publish/themes/")))

(defun shared-template-dir ()
  "return the absolute path to the shared template directory."
  (uiop:native-namestring (asdf:system-relative-pathname :cltpt "src/publish/template/")))

(defun load-theme-by-name (name)
  "find and return the directory path for the theme named NAME."
  (let ((theme-dir (cltpt/file-utils:join-paths (built-in-themes-dir) name)))
    (if (uiop:probe-file* theme-dir)
        theme-dir
        (error "theme '~A' not found in ~A" name (built-in-themes-dir)))))

(defun title-to-filename (title)
  (with-output-to-string (out)
    (loop for char across (string-downcase title)
          do (cond ((or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\.))
                    (write-char char out))
                   ((or (char= char #\space)
                        (char= char #\/)
                        (char= char #\\))
                    (write-char #\_ out))))))

(defun convert-template* (template-file)
  (let ((*current-template-file* (uiop:native-namestring (uiop:truename* template-file))))
    (cltpt/base:convert-tree
     (cltpt/base:parse
      cltpt/base:*simple-format*
      (cltpt/file-utils:read-file template-file))
     cltpt/base:*simple-format*
     cltpt/html:*html*
     :escape nil)))

(defun convert-template (dest-dir template-file)
  (cltpt/file-utils:write-file
   (cltpt/file-utils:change-dir template-file dest-dir)
   (convert-template* template-file)))

(defvar *default-filepath-format*
  "%(cltpt/publish:title-to-filename (or (getf *file-info* :root-title) (getf *file-info* :filename))).html")

(defvar *default-static-filepath-format*
  "static/%(getf *file-info* :filename)")

(defvar *default-html-static-route*
  "static/")

;; theme-dir is only needed for resolving templates later on.
(defun publish (output-dir files
                &key
                  include-files exclude-files
                  static-output-dir
                  (filepath-format *default-filepath-format*)
                  (static-filepath-format *default-static-filepath-format*)
                  templates
                  template-file
                  theme-dir
                  (html-static-route *default-html-static-route*))
  (cltpt/file-utils:ensure-dir-exists (cltpt/file-utils:as-dir-path output-dir))
  (let* ((*theme-dir* (or theme-dir *theme-dir*))
         (*current-template-file*
           (when template-file
             (uiop:native-namestring (uiop:truename* template-file))))
         (cltpt/html:*html-static-route* html-static-route)
         (rmr (cltpt/roam:from-files files))
         (files-to-convert
           (if include-files
               (loop for include-file in include-files
                     for node = (find include-file
                                      (cltpt/roam:roamer-nodes rmr)
                                      :key (lambda (node)
                                             (cltpt/roam:node-file node))
                                      :test 'string=)
                     append (cons include-file
                                  (cltpt/utils:find-linked-files rmr node exclude-files)))
               ;; if include-files wasnt provided we consider all files
               (mapcar #'cltpt/roam:node-file (cltpt/roam:roamer-nodes rmr))))
         (static-output-dir (or static-output-dir
                                (cltpt/file-utils:join-paths output-dir "static")))
         (cltpt/latex-previews:*latex-previews-cache-directory* static-output-dir)
         (cltpt/latex-previews:*latex-compiler-key* :lualatex)
         (cltpt/latex-previews:*latex-preview-pipeline-key* :dvisvgm)
         (file-predicate
           (lambda (filepath)
             (member filepath files-to-convert :test 'string=)))
         (cltpt/html:*html-template*
           (if template-file
               (uiop:read-file-string template-file)
               cltpt/html:*html-template*)))
    (let ((cltpt/roam:*roam-convert-data* (list :roamer rmr)))
      (loop for template in templates
            do (convert-template output-dir template)))
    ;; apparently it doesnt work unless theres a '/' at the end.
    (cltpt/file-utils:ensure-dir-exists
     (concatenate 'string static-output-dir "/"))
    (cltpt/utils:compile-all-latex-previews rmr file-predicate)
    (cltpt/utils:convert-all
     :dest-format-name "html"
     :roamer rmr
     :dest-dir output-dir
     :filepath-format filepath-format
     :static-filepath-format static-filepath-format
     :predicate file-predicate)))

(defclass template (cltpt/base:text-object)
  ((file
    :initarg :file
    :accessor template-file)))

(defmethod cltpt/base:text-object-convert ((obj template)
                                           (backend cltpt/base:text-format))
  (let ((*current-template-file* (template-file obj))
        (cltpt/base:*convert-info* (cltpt/base:merge-plist
                                    cltpt/base:*convert-info*
                                    (list :src-fmt cltpt/base:*simple-format*
                                          :dest-fmt backend))))
    (list :text (cltpt/base:convert-tree
                 (cltpt/base:parse
                  cltpt/base:*simple-format*
                  (cltpt/file-utils:read-file (template-file obj)))
                 cltpt/base:*simple-format*
                 cltpt/html:*html*
                 :escape nil)
          :escape nil)))

(defun resolve-template-path (relative-path)
  "resolve a relative template path.

tries in order:
1. *theme-dir*/template/
2. shared template directory (src/publish/template/)
3. *current-template-file*'s directory"
  (if (uiop:absolute-pathname-p relative-path)
      relative-path
      (let ((theme-path (when *theme-dir*
                          (cltpt/file-utils:join-paths *theme-dir* "template" relative-path)))
            (shared-path (cltpt/file-utils:join-paths (shared-template-dir) relative-path))
            (current-path (when *current-template-file*
                            (cltpt/file-utils:join-paths
                             (cltpt/file-utils:file-dirpath *current-template-file*)
                             relative-path))))
        (cond ((and theme-path (uiop:probe-file* theme-path)) theme-path)
              ((uiop:probe-file* shared-path) shared-path)
              ((and current-path (uiop:probe-file* current-path)) current-path)
              (t (or theme-path current-path relative-path))))))

(defun read-template-file (template-file)
  (uiop:read-file-string (resolve-template-path template-file)))

(defun make-template (&key file)
  (let ((obj (make-instance 'template)))
    (setf (template-file obj) (cltpt/file-utils:ensure-absolute (resolve-template-path file)))
    obj))

(defun template-file-to-template-obj (template-file)
  (make-template :file template-file))

(defclass unescaping-text-obj (cltpt/base:text-object)
  ())

;; we have to do this because by default a dummy text-object is created that doesnt have a special
;; text-object-convert assigned to it and so its results get :escape t by the default one.
(defmethod cltpt/base:text-object-convert ((obj unescaping-text-obj)
                                           (backend cltpt/base:text-format))
  (list :text (cltpt/buffer:buffer-own-text obj)
        :escape nil))

(defun wrap-into-unescaping-obj (txt)
  (let ((obj (make-instance 'unescaping-text-obj)))
    (setf (cltpt/buffer:buffer-own-text obj) txt)
    obj))

(defun node-to-href (node)
  (let* ((title (cltpt/roam:node-title node))
         (filename-fallback (cltpt/file-utils:file-basename (cltpt/roam:node-file node)))
         (file (title-to-filename (or title filename-fallback))))
    (concatenate 'string file ".html")))

(defun %render-post-items ()
  (let ((rmr (cltpt/roam:current-roamer)))
    (when rmr
      (let ((nodes (remove-if-not
                    (lambda (node)
                      (typep (cltpt/roam:node-text-obj node) 'cltpt/base:document))
                    (cltpt/roam:roamer-nodes rmr))))
        (with-output-to-string (out)
          (let* ((template-item-file (resolve-template-path "post-item.html"))
                 (template-item (template-file-to-template-obj template-item-file)))
            (dolist (node nodes)
              (let ((cltpt/roam:*roam-convert-data* (list :roamer rmr :node node))
                    (cltpt/base:*convert-info*
                      (list :src-fmt cltpt/base:*simple-format*
                            :dest-fmt cltpt/html:*html*)))
                (format out
                        "~A~%"
                        (cltpt/base:convert-tree
                         template-item
                         cltpt/base:*simple-format*
                         cltpt/html:*html*
                         :escape nil))))))))))

(defun render-post-items ()
  (wrap-into-unescaping-obj (%render-post-items)))

(defun render-post-list ()
  (let* ((template-list-file (resolve-template-path "post-list.html"))
         (template-list (template-file-to-template-obj template-list-file)))
    (wrap-into-unescaping-obj
     (cltpt/base:convert-tree
      template-list
      cltpt/base:*simple-format*
      cltpt/html:*html*))))