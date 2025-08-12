(defpackage :cltpt/roam
  (:use :cl :cltpt/base)
  (:export :from-files :roamer-rescan :roamer-nodes
   :node-id :node-title :node-desc :node-file :node-text-obj
   :node-file-rule :roamer-node-id-hashtable :get-node-by-id :convert-all
   :node-format :node-info-format-str
           :make-node :text-object-roam-data :roamer :*convert-roamer*
           :*roam-convert-data*))

(in-package :cltpt/roam)

(defstruct node
  id
  title
  desc
  text-obj
  file ;; we can do this better
  file-rule ;; file rule from which the node was constructed we can do this better
  format
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
    :documentation "files/directories we load the nodes from.")
   (node-id-hashtable
    :initform (make-hash-table :test 'equal)
    :accessor roamer-node-id-hashtable
    :documentation "map a node to its id.")))

(defvar *roam-convert-data*
  nil
  "dynamically bound metadata to pass down from roamer to conversion functions of objects.
at the very least has to include a `roamer' instance for conversion functions to make use of. e.g. for retrieval of node by id using `get-node-by-id'.
form:
  `(:output-file-format nil
    :roamer nil)'")

(defun from-files (files)
  "see documentation of `find-files' for FILES. takes a set of rules, returns a
`roamer' object."
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
  "takes a list of rules for files to find.
each rule is a plist that can contain the following params.
:path - path of file/directory,
:regex - a regex to match against the files found,
:recurse - if :path is a directory, this says whether to recursively look for files,
:format - unused here, but tells us which format to use to parse the files found."
  (let (;; maps a raw filepath to the rule it was found for
        (file-rule-alist)
        ;; strings are just filepaths of individual files, nothing smart about it
        (file-rules-string (remove-if-not 'stringp file-rules))
        (file-rules-plist (remove-if-not 'plistp file-rules)))
    (labels ((handle-file (filepath file-rule)
               (let ((ext (getf file-rule :ext)))
                 (if ext
                     (when (cltpt/base:file-has-extension-p
                            filepath
                            (if (consp ext)
                                ext
                                (cons ext nil)))
                       (push (cons filepath file-rule) file-rule-alist))
                     (push (cons filepath file-rule) file-rule-alist)))))
      (loop for file-rule in file-rules-plist
            for paths = (getf file-rule :path)
            for regex = (getf file-rule :regex)
            for recurse = (getf file-rule :recurse)
            do (loop
                 for path in (if (consp paths) paths (cons paths nil))
                 do (if (uiop:directory-pathname-p (uiop:ensure-pathname path))
                        (if recurse
                            (cl-fad:walk-directory
                             path
                             (lambda (path)
                               (handle-file path file-rule))
                             :match regex)
                            (loop
                              for path in (cltpt/base::directory-files-matching
                                           path
                                           regex)
                              do (handle-file path file-rule)))
                        (handle-file path file-rule))))
      (loop for filepath in file-rules-string
            do (push (cons filepath filepath) file-rule-alist)))
    file-rule-alist))

(defmethod roamer-rescan ((rmr roamer))
  (setf (roamer-nodes rmr) nil)
  (setf (roamer-node-id-hashtable rmr)
        (make-hash-table :test 'equal))
  (let ((file-rule-alist (find-files (roamer-files rmr))))
    (loop for (file . file-rule) in file-rule-alist
          do (let* ((fmt (if (plistp file-rule)
                             (cltpt/base:text-format-by-name
                              (getf file-rule :format))
                             (cltpt/base:text-format-from-alias
                              (cltpt/base:file-ext file-rule))))
                   (parsed (cltpt/base:parse-file file fmt)))
               (cltpt/base:map-text-object
                parsed
                (lambda (text-obj)
                  (let ((node (text-object-roam-data text-obj)))
                    (when node
                      (setf (node-text-obj node) text-obj)
                      (setf (node-file node) file)
                      (setf (node-file-rule node) file-rule)
                      (setf (node-format node) fmt)
                      (push node (roamer-nodes rmr))
                      (when (node-id node)
                        (setf
                         (gethash
                          (node-id node)
                          (roamer-node-id-hashtable rmr))
                         node))))))))))

(defmethod text-object-roam-data ((obj cltpt/base:text-object))
  (cltpt/base:text-object-property obj :roam-node))

(defmethod get-node-by-id ((rmr roamer) id)
  (gethash
   id
   (roamer-node-id-hashtable rmr)))

(defmethod convert-all ((rmr roamer)
                        (dest-format cltpt/base:text-format)
                        output-file-format)
  (let ((files-done (make-hash-table :test 'equal))
        (*roam-convert-data*
          (list :roamer rmr
                :output-file-format output-file-format)))
    (loop for node in (roamer-nodes rmr)
          do (let* ((in-file (node-file node))
                    (is-done (gethash in-file files-done))
                    (out-file (node-info-format-str node output-file-format)))
               (unless is-done
                 (format t "converting ~A to ~A~%" in-file out-file)
                 (cltpt/base:convert-file
                  (node-format node)
                  dest-format
                  in-file
                  out-file)
                 (setf (gethash in-file files-done) t))))))

(defmethod node-info-format-str ((node node) format-str)
  "takes a roam node and a string, returns a new string with some 'placeholders'replaced."
  (cltpt/base:bind-and-eval
   `((title ,(cltpt/roam:node-title node))
     (file ,(cltpt/roam:node-file node))
     (id ,(cltpt/roam:node-id node))
     (file-no-ext ,(cltpt/base:path-without-extension (cltpt/roam:node-file node)))
     (basename ,(cltpt/base:base-name-no-ext (cltpt/roam:node-file node))))
   (lambda ()
     ;; need to use in-package to access the variables bound above
     (in-package :cltpt/roam)
     (let ((result
             (cltpt/base:convert-tree
              (cltpt/base:parse
               format-str
               (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))
              (cltpt/base:text-format-by-name "org-mode") ;; just use org for now
              (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))))
       result))))