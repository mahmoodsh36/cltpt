(defpackage :cltpt
  (:use :cl)
  (:export :*debug*))

(in-package :cltpt)

(defvar *debug*
  (list :convert nil
        :parse nil
        :roam nil)
  "different debugging settings, if some property is set to `t', some corresponding debugging functionality throughout the source code will be activated.")

(defvar *author*
  nil
  "author name used when generating a document.")