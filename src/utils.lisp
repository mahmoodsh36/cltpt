(defpackage :cltpt/utils
  (:use :cl)
  (:export :convert-all
           :compile-all-latex-previews
           :find-linked-files))

(in-package :cltpt/utils)

(defun convert-all (&key
                      src-format dest-format
                      src-format-name dest-format-name
                      files rules
                      filepath-format static-filepath-format
                      dest-dir dest-dir-static
                      roamer
                      predicate)
  (let* ((src-format (or src-format (cltpt/base:text-format-by-name src-format-name)))
         (dest-format (or dest-format (cltpt/base:text-format-by-name dest-format-name)))
         (roamer (or roamer (cltpt/roam:roamer-from-files (or files rules)))))
    (when (and roamer filepath-format dest-format)
      (cltpt/roam:convert-all
       roamer
       dest-format
       filepath-format
       :dest-dir dest-dir
       :dest-dir-static dest-dir-static
       :static-filepath-format static-filepath-format
       :convert-file-predicate predicate))))

;; this is a hack to gather all latex previews and compile them all at once
;; before exporting. this is to speed things up. one latex conversion commands
;; is faster than running it once for every file independently.
(defun compile-all-latex-previews (rmr file-predicate)
  (let ((all-snippets)
        ;; snippets-at-once is how many snippets should be compiled at the same time.
        ;; one case in which this is helpful is when there are too many errors (>100)
        ;; and latex refuses to compile all previews at once and exits. so we
        ;; break them into batches, which make it more likely that more latex
        ;; previews will be rendered overall.
        (snippets-at-once 1000))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for this-tree = (cltpt/roam:node-text-obj node)
          for node-file = (cltpt/roam:node-file node)
          when (funcall file-predicate node-file)
            do (cltpt/base:map-text-object
                this-tree
                (lambda (obj)
                  (when (or (typep obj 'cltpt/latex:inline-math)
                            (typep obj 'cltpt/latex:display-math)
                            (typep obj 'cltpt/latex:latex-env))
                    (pushnew (cltpt/base:text-object-contents obj)
                             all-snippets
                             :test 'string=)))))
    (setf all-snippets (reverse all-snippets))
    (loop for i from 0 to (length all-snippets) by snippets-at-once
          do (cltpt/latex-previews:generate-previews-for-latex
              (subseq all-snippets
                      i
                      (min (+ i snippets-at-once)
                           (length all-snippets)))))))

;; exclude-files is important here because it includes files we shouldnt recurse from, not just
;; "exclude".
(defun find-linked-files (rmr root exclude-files)
  "helper function to find all linked files from a node."
  (let ((linked-files)
        (nodes-left (list root)))
    (loop
      while nodes-left
      for node = (pop nodes-left)
      for text-obj = (cltpt/roam:node-text-obj node)
      do (loop
           for link-obj
             in (cltpt/base:find-children-recursively
                 text-obj
                 (lambda (child)
                   (typep child 'cltpt/base:text-link)))
           do (let* (;; we have to bind this so resolve-link can use the roamer
                     (cltpt/roam:*roam-convert-data*
                       (list :roamer rmr
                             :filepath-format nil
                             :node node))
                     (result (cltpt/base:text-link-resolve link-obj))
                     (linked-file
                       (when result
                         (cltpt/base:target-filepath result))))
                (when (and linked-file
                           (not (member linked-file
                                        exclude-files
                                        :test 'string=)))
                  (unless (member linked-file
                                  (cons (cltpt/roam:node-file node) linked-files)
                                  :test 'string=)
                    (push linked-file linked-files)
                    (when (typep result 'cltpt/roam:node)
                      (push result nodes-left)))))))
    linked-files))