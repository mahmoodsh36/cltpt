(defpackage :cltpt/roam
  (:use :cl :cltpt/base)
  (:export
   :from-files :roamer-rescan :roamer-nodes
   :node-id :node-title :node-desc :node-file :node-text-obj :node-file-rule
   :roamer-node-id-hashtable :get-node-by-id :convert-all
   :node-format :node-info-format-str :make-node :text-object-roam-data
   :roamer :*convert-roamer* :*roam-convert-data* :convert-link
   :resolve-link :link-dest-node))

(in-package :cltpt/roam)

(defstruct node
  id
  title
  desc
  text-obj
  file ;; we can do this better
  file-rule ;; file rule from which the node was constructed we, can do this better
  format ;; text-format, do we need this?
  )

(defvar *roam-convert-data*
  nil
  "dynamically bound metadata to pass down from roamer to conversion functions of objects.
at the very least has to include a `roamer' instance for conversion functions to make use of. e.g. for retrieval of node by id using `get-node-by-id'.
form:
  `(:filepath-format nil
    :roamer nil
    :node nil)'")

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

(defstruct link
  src-node ;; source roam node
  dest-node ;; destination roam node
  src-text-obj ;; the text object which created the link (e.g. an instance of 'org-link')
  type ;; link type, should be a symbol
  )

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
                        filepath-format)
  (let ((files-done (make-hash-table :test 'equal)))
    (loop for node in (roamer-nodes rmr)
          do (let* ((in-file (node-file node))
                    (*roam-convert-data*
                      (list :roamer rmr
                            :filepath-format filepath-format
                            :node node))
                    (is-done (gethash in-file files-done))
                    (out-file (node-info-format-str node filepath-format)))
               (when (and (typep (node-text-obj node) 'document) (not is-done))
                 (when (eql cltpt:*debug* 2)
                   (format t "converting ~A to ~A~%" in-file out-file))
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
     (let* (;; need to make it execute in-package to access the variables bound above
            (*package* (find-package :cltpt/roam))
            (result
              (cltpt/base:convert-tree
               (cltpt/base:parse
                format-str
                (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))
               (cltpt/base:text-format-by-name "org-mode") ;; just use org for now
               (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))))
       result))))

(defmethod convert-link ((rmr roamer)
                         (src-node node)
                         (link-obj cltpt/base:text-object)
                         filepath-format
                         backend)
  "when converting links, we need to be aware of the roamer we're working with,
in order to be able to get the destination of the link incase it is an id-link.
this function handles an object that should have the properties of a link,
which may include the destination, description and type."
  (let* ((desc (cltpt/base:text-object-property link-obj :desc))
         (dest (cltpt/base:text-object-property link-obj :dest))
         (type (cltpt/base:text-object-property link-obj :type)))
    ;; type is a string originally, we convert it to a symbol.
    (when type
      (setf type (intern type)))
    (if type
        (let* ((link (resolve-link rmr src-node link-obj type dest))
               (dest-node (when link (link-dest-node link)))
               (dest-file (when dest-node (cltpt/roam:node-file dest-node))))
          (if dest-file
              (progn
                (setf dest-file (node-info-format-str dest-node filepath-format))
                (let ((new-obj (cltpt/base:text-object-clone link-obj))
                      ;; set this dynamically to make the conversion function
                      ;; not call this function again
                      (cltpt/roam:*roam-convert-data*))
                  (setf (cltpt/base:text-object-property new-obj :dest) dest-file)
                  (cltpt/base:text-object-convert new-obj backend)))
              (format nil "broken link: ~A:~A" type dest)))
        ;; set this dynamically to make the conversion function
        ;; not call this function again
        (let ((cltpt/roam:*roam-convert-data*))
          (cltpt/base:text-object-convert link-obj backend)))))

(defgeneric resolve-link (rmr src-node src-text-obj link-type dest)
  (:documentation "LINK-TYPE is a symbol indicating the type of the link. DEST is the destination, specified as a string. it should return a `link'."))

(defmethod make-id-link ((rmr roamer)
                         (src-node node)
                         (src-text-obj cltpt/base:text-object)
                         link-type
                         dest-id)
  (let* ((dest-node (cltpt/roam:get-node-by-id rmr dest-id))
         (new-link (make-instance 'link)))
    (when dest-node
      (setf (link-src-text-obj new-link) src-text-obj)
      (setf (link-dest-node new-link) dest-node)
      (setf (link-src-node new-link) src-node)
      (setf (link-type new-link) link-type)
      new-link)))

(defmethod resolve-link ((rmr roamer)
                         (src-node node)
                         (src-text-obj cltpt/base:text-object)
                         (link-type symbol)
                         dest)
  (make-id-link rmr src-node src-text-obj link-type dest))

(defmethod resolve-link ((rmr roamer)
                         (src-node node)
                         (src-text-obj cltpt/base:text-object)
                         (link-type (eql 'denote))
                         dest)
  (make-id-link rmr src-node src-text-obj link-type dest))

(defmethod resolve-link ((rmr roamer)
                         (src-node node)
                         (src-text-obj cltpt/base:text-object)
                         (link-type (eql 'blk))
                         dest)
  (make-id-link rmr src-node src-text-obj link-type dest))

(defmethod resolve-link ((rmr roamer)
                         (src-node node)
                         (src-text-obj cltpt/base:text-object)
                         (link-type (eql 'id))
                         dest)
  (make-id-link rmr src-node src-text-obj link-type dest))