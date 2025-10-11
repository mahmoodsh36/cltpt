(defpackage :cltpt/babel
  (:use :cl)
  (:export :init))

(in-package :cltpt/babel)

(defgeneric babel-eval (lang code))

(defgeneric babel-eval* (lang code assignments result-type))

(defgeneric babel-lexical-wrap (lang code assignments)
  (:documentation "given CODE and variable ASSIGNMENTS, generate code in which ASSIGNMENTS (list of variable and value conses) are lexically bound."))

(defgeneric babel-encode (lang type value)
  (:documentation "given VALUE, convert it into a form representing an object of type TYPE, that will be readable by language LANG.

possible examples for TYPE are: list, string, dictionary."))

(defgeneric babel-decode (lang type str)
  (:documentation "given STR, which is suppsoedly an object that was \"stringified\" and returned from the target LANG, read it as an object of type TYPE.

possible examples for TYPE are: list, string, dictionary."))

;; handles "lexical" bindings (simply variables) for python
(defmethod babel-lexical-wrap ((lang (eql 'python))
                               code
                               assignments)
  (with-output-to-string out
    (loop for (name . value) in assignments
          (format out "~A = ~A~%" name value))
    (write-string code out)))

(defmethod babel-decode (lang
                         (type (eql 'string))
                         str)
  str)

(defmethod babel-encode (lang
                         (type (eql 'string))
                         value)
  (format t "~S" value))