(defpackage :cltpt
  (:use :cl)
  (:export :*debug*))

(in-package :cltpt)

;; takes values 1-2
(defvar *debug* nil)