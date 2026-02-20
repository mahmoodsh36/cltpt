(defpackage :cltpt/utils
  (:use :cl)
  (:export :convert-all))

(in-package :cltpt/utils)

(defun convert-all (&key
                      src-format dest-format
                      src-format-name dest-format-name
                      files rules
                      filepath-format static-filepath-format
                      dest-dir)
  (let* ((src-format (or src-format (cltpt/base:text-format-by-name src-format-name)))
         (dest-format (or dest-format (cltpt/base:text-format-by-name dest-format-name)))
         (roamer (cltpt/roam:from-files (or files rules))))
    (when (and roamer filepath-format dest-format)
      (cltpt/roam:convert-all
       roamer
       dest-format
       filepath-format
       :dest-dir dest-dir
       :static-filepath-format static-filepath-format))))