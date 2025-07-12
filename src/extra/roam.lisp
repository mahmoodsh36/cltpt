(defpackage :cltpt/roam
  (:use :cl :cltpt/base)
  (:export :from-files :roamer-rescan :roamer-nodes))

(in-package :cltpt/roam)

(defstruct node
  id
  title
  description
  todo ;; should we have this?
  text-obj
  file ;; we can do this better
  )

(defclass roamer ()
  ((nodes
    :initform nil
    :accessor roamer-nodes
    :documentation "collection of nodes.")
   (files
    :initarg :files
    :initform nil
    :accessor roamer-files
    :documentation "files/directories we load the nodes from.")))

;; example of files:
#|
(:path '("dir1" "dir2" "file.md")
 :recurse nil
 :regex nil ;; ".*\.(org|tex|md)"
 :ext nil ;; '("org" "tex" "md")
 :format "org-mode"
       )
(:path '("dir1" "dir2" "file.m")
 :recurse nil
 :regex nil ;; ".*\.(org|tex|md)"
 :ext nil ;; '("org" "tex" "md")
 :format "latex"
       )
|#

;; takes a set of files, returns a roamer
(defun from-files (files)
  (let ((rmr (make-roamer :files files)))
    (roamer-rescan rmr)
    rmr))

(defun make-roamer (&rest args)
  (make-instance 'roamer
                 :files (getf args :files)))

;; get the backlinks to a specific node
(defmethod backlinks-to-node ((rm roamer) (nd node))
  )

;; get a node using its id (e.g. ids can be used in links)
(defmethod find-node-by-id ((rm roamer) node-id)
  )

(defun find-files (file-rules)
  (let ((file-rule-alist)) ;; maps a raw filepath to the rule it was found for
    (loop for file-rule in file-rules
          for paths = (getf file-rule :path)
          for regex = (getf file-rule :regex)
          for ext = (getf file-rule :ext)
          for recurse = (getf file-rule :recurse)
          for format = (getf file-rule :format)
          do (loop
               for path in (if (consp paths) paths (cons paths nil))
               do (if (uiop:directory-pathname-p (uiop:ensure-pathname path))
                      (if recurse
                          (cl-fad:walk-directory
                           path
                           (lambda (path)
                             (if ext
                                 (when (cltpt/base::file-has-extension-p
                                        path
                                        (if (consp ext)
                                            ext
                                            (cons ext nil)))
                                   (push (cons path file-rule) file-rule-alist))
                                 (push (cons path file-rule) file-rule-alist)))
                           :match regex)
                          (loop
                            for path in (cltpt/base::directory-files-matching
                                         path
                                         regex)
                            do (if ext
                                   (when (cltpt/base::file-has-extension-p
                                          path
                                          (if (consp ext)
                                              ext
                                              (cons ext nil)))
                                     (push (cons path file-rule) file-rule-alist))
                                   (push (cons path file-rule) file-rule-alist)))))))
    file-rule-alist))

(defmethod roamer-rescan ((rmr roamer))
  (setf (roamer-nodes rmr) nil)
  (let ((file-rule-alist (find-files (roamer-files rmr))))
    (loop for (file . file-rule) in file-rule-alist
          for fmt = (cltpt/base::text-format-by-name (getf file-rule :format))
          do (let* ((parsed (cltpt/base:parse-file
                            file
                            fmt))
                    (node (make-node
                           :id nil
                           :title nil
                           :description nil
                           :todo nil
                           :text-obj parsed
                           :file file)))
               (push (roamer-nodes rmr) node)))))

(defmethod text-object-roam-data ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :roam-node))