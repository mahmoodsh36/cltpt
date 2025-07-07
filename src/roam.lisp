(defpackage :cltpt/roam
  (:use :cl :cltpt/base)
  (:export))

(in-package :cltpt/roam)

(defstruct node
  id
  title
  description
  todo ;; should we have this?
  src-text-obj
  src-file ;; we can do this better
  )

(defclass roamer ()
  ((nodes
    :initform nil
    :accessor roamer-nodes
    :documentation "collection of nodes.")
   (files
    :initform nil
    :accessor roamer-files
    :documentation "files we load the nodes from.")))

;; takes a set of files, returns a roamer
(defun from-files (files)
  )

;; get the backlinks to a specific node
(defmethod backlinks-to-node ((rm roamer) (nd node))
  )

;; get a node using its id (e.g. ids can be used in links)
(defmethod find-node-by-id ((rm roamer) node-id)
  )