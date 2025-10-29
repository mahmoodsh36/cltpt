(defpackage :cltpt/roam
  (:use :cl :cltpt/base)
  (:export
   :from-files :roamer-rescan :roamer-nodes
   :node-id :node-title :node-desc :node-file :node-text-obj :node-file-rule
   :roamer-node-id-hashtable :get-node-by-id :convert-all
   :node-format :node-info-format-str :make-node :text-object-roam-data
   :roamer :*convert-roamer* :*roam-convert-data* :*roam-parse-data*
   :current-roamer :node))

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

(defvar *roam-parse-data*
  nil
  "dynamically bound metadata to pass down from roamer to initialization functions of objects.
at the very least has to include a `roamer' instance for conversion functions to make use of. e.g. for retrieval of node by id using `get-node-by-id'. although
note that this metadata may not be available during parsing because the roamer
doesnt handle dependencies between files, so some functionality must be \"postponed\"
to the time when the roamer is done collecting/parsing.
form:
  `(:roamer nil)'")

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

(defmethod find-files (file-rules)
  "takes a list of rules for files to find.
each rule is a plist that can contain the following params.
:path - path of file/directory,
:glob - a glob to match against the files found,
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
                     (when (cltpt/file-utils:file-has-extension-p
                            filepath
                            (if (consp ext)
                                ext
                                (cons ext nil)))
                       (push (cons filepath file-rule) file-rule-alist))
                     (push (cons filepath file-rule) file-rule-alist)))))
      (loop for file-rule in file-rules-plist
            for paths = (getf file-rule :path)
            for glob = (getf file-rule :glob)
            for recurse = (getf file-rule :recurse)
            do (loop
                 for path in (if (consp paths) paths (cons paths nil))
                 do (if (uiop:directory-pathname-p (uiop:ensure-pathname path))
                        ;; if its a dir, walk it and apply the handler to each file
                        (cltpt/file-utils:walk-dir
                         path
                         :handle-file-fn (lambda (path)
                                           (handle-file path file-rule))
                         :glob glob
                         :recurse recurse)
                        ;; if its a file, just execute the handler on it.
                        (handle-file path file-rule))))
      (loop for filepath in file-rules-string
            do (push (cons filepath filepath) file-rule-alist)))
    file-rule-alist))

(defmethod roamer-rescan ((rmr roamer))
  (setf (roamer-nodes rmr) nil)
  (setf (roamer-node-id-hashtable rmr)
        (make-hash-table :test 'equal))
  (let* ((file-rule-alist (find-files (roamer-files rmr)))
         (*roam-parse-data*
           (list :roamer rmr)))
    (loop for (file . file-rule) in file-rule-alist
          do (let* ((fmt (if (plistp file-rule)
                             (cltpt/base:text-format-by-name
                              (getf file-rule :format))
                             (cltpt/base:text-format-from-alias
                              (cltpt/file-utils:file-ext file-rule))))
                    (parsed (cltpt/base:parse-file
                             fmt
                             file
                             :text-object-types (text-format-roam-types fmt))))
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
                        filepath-format
                        &optional (convert-file-predicate (lambda (x) t)))
  (let ((files-done (make-hash-table :test 'equal)))
    (loop for node in (roamer-nodes rmr)
          do (let* ((in-file (node-file node))
                    (*roam-convert-data*
                      (list :roamer rmr
                            :filepath-format filepath-format
                            :node node))
                    (is-done (gethash in-file files-done))
                    (out-file (node-info-format-str node filepath-format)))
               (when (and (typep (node-text-obj node) 'document)
                          (not is-done)
                          (funcall convert-file-predicate in-file))
                 (when (getf cltpt:*debug* :roam)
                   (format t "converting ~A to ~A~%" in-file out-file))
                 (let ((cltpt/base:*convert-info*
                         (cltpt/base:merge-plist
                          cltpt/base:*convert-info*
                          (list :filepath-format filepath-format))))
                   (cltpt/base:convert-file
                    (node-format node)
                    dest-format
                    in-file
                    out-file))
                 (setf (gethash in-file files-done) t))))))

(defmethod node-info-format-str ((node node) format-str)
  "takes a roam node and a string, returns a new string with some 'placeholders'replaced."
  (let* ((root (cltpt/tree:tree-root (node-text-obj node)))
         ;; TODO: this directly grabs :title property which defeats the whole
         ;; purpose of the 'node' type. we should be keeping track of the roam-node
         ;; that points to the root text object of the current `node' instead and
         ;; grab the title from there.
         ;; TODO: also this is slow anyway, takes log(n) time, easy to optimize tho.
         (root-title (cltpt/base:text-object-property root :title)))
    (cltpt/base:bind-and-eval
     `((title ,(cltpt/roam:node-title node))
       (root-title ,root-title)
       (file ,(cltpt/roam:node-file node))
       (id ,(cltpt/roam:node-id node))
       (file-no-ext ,(cltpt/file-utils:path-without-extension
                      (cltpt/roam:node-file node)))
       (basename ,(cltpt/file-utils:base-name-no-ext (node-file node))))
     (lambda ()
       (let* ((result
                (cltpt/base:convert-tree
                 (cltpt/base:parse
                  (cltpt/base:make-text-format "dummy")
                  format-str
                  :text-object-types (list 'cltpt/base:text-macro
                                           'cltpt/base:post-lexer-text-macro))
                 (cltpt/base:make-text-format "dummy")
                 (cltpt/base:make-text-format "dummy")
                 :text-object-types (list 'cltpt/base:text-macro
                                          'cltpt/base:post-lexer-text-macro))))
         result)))))

(defmethod link-resolve ((link-type (eql 'cltpt/base::id))
                         dest
                         desc)
  (let* ((rmr (getf cltpt/roam:*roam-convert-data* :roamer))
         (dest-node
           (when rmr
             (cltpt/roam:get-node-by-id rmr dest))))
    dest-node))

;; (defmethod convert-link ((rmr roamer)
;;                          (src-node node)
;;                          (link-obj cltpt/base:text-object)
;;                          filepath-format
;;                          backend)
;;   "when converting links, we need to be aware of the roamer we're working with,
;; in order to be able to get the destination of the link incase it is an id-link.
;; this function handles an object that should have the properties of a link,
;; which may include the destination, description and type."
;;   ;; TODO: we must ensure that LINK-OBJ contains these properties. perhaps
;;   ;; we need to write a class inheriting from `text-object' that is for links
;;   ;; that has these properties as slots.
;;   (let* ((desc (cltpt/base:text-object-property link-obj :desc))
;;          (dest (cltpt/base:text-object-property link-obj :dest))
;;          (type (cltpt/base:text-object-property link-obj :type)))
;;     ;; type is a string originally, we convert it to a symbol.
;;     (when type
;;       (setf type (intern type)))
;;     (if type
;;         (let* ((link (resolve-link rmr src-node link-obj type dest))
;;                (dest-node (when link (link-dest-node link)))
;;                (dest-file (if dest-node
;;                               (node-file dest-node)
;;                               (when link (link-dest-file link)))))
;;           (if dest-file
;;               (progn
;;                 ;; when its a node, the destination file needs to be determined
;;                 ;; using filepath-format. if its just a static file, we dont
;;                 ;; change the destination filepath here.
;;                 (when dest-node
;;                   (setf dest-file (node-info-format-str dest-node filepath-format)))
;;                 (let ((new-obj (cltpt/base:text-object-clone link-obj))
;;                       ;; set this dynamically to make the conversion function
;;                       ;; not call this function again
;;                       (*roam-convert-data*))
;;                   (setf (cltpt/base:text-object-property new-obj :dest) dest-file)
;;                   (cltpt/base:text-object-convert new-obj backend)))
;;               (format nil "broken link: ~A:~A" type dest)))
;;         ;; set this dynamically to make the conversion function
;;         ;; not call this function again
;;         (let ((*roam-convert-data*))
;;           (cltpt/base:text-object-convert link-obj backend)))))

;; TODO: hacky function to get current dynamically bound roamer.
(defun current-roamer ()
  (or (getf *roam-convert-data* :roamer)
      (getf *roam-parse-data* :roamer)))

(defmethod text-format-roam-types ((tf cltpt/base:text-format))
  "names of sub-classes of `text-object' that should be considered when parsing for roam."
  (cltpt/base:text-format-text-object-types tf))