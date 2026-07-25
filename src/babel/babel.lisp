(defpackage :cltpt/babel
  (:use :cl)
  (:export :babel-eval :babel-eval* :babel-encode :babel-decode :babel-value-rule
           :babel-lexical-wrap))

(in-package :cltpt/babel)

(defgeneric babel-eval (lang code))

(defgeneric babel-eval* (lang code assignments result-type &key &allow-other-keys))

(defgeneric babel-lexical-wrap (lang code assignments)
  (:documentation "given CODE and variable ASSIGNMENTS, generate code in which ASSIGNMENTS (list
of variable and value conses) are lexically bound."))

(defgeneric babel-value-rule (lang)
  (:documentation "combinator rule describing LANG's values.
drives `babel-encode' through `cltpt/transform:generate'."))

(defgeneric babel-encode (lang value)
  (:documentation "given VALUE, convert it into a string readable by language LANG."))

;; this is mostly a placeholder. will need to replace it with proper escape handling and whatnot.
(defvar *python-string-rule*
  '(:pattern (cltpt/combinator:consec "'" (cltpt/combinator:all-but "'") "'")
    :get list))

(defvar *python-list-rule*
  '(cltpt/combinator:any
    (:pattern "[]" :value nil)
    (:pattern
     (cltpt/combinator:consec
      "["
      (cltpt/combinator:separated-atleast-one
       (cltpt/combinator:any ", " ",")
       *python-value-rule*)
      "]")
     :get list)))

(defvar *python-value-rule*
  '(cltpt/combinator:any
    *python-list-rule*
    *python-string-rule*
    (cltpt/combinator:number-matcher)))

(defmethod babel-value-rule ((lang (eql 'python)))
  '*python-value-rule*)

(defmethod babel-encode (lang value)
  (cltpt/transform:generate (babel-value-rule lang) value))

(defmethod babel-decode (lang text)
  "parse TEXT as a LANG value and recover the lisp value it represents (the inverse of
`babel-encode'). returns NIL if TEXT does not parse as a value."
  (let* ((reader (cltpt/reader:reader-from-string text))
         (rule (babel-value-rule lang))
         (match (car (cltpt/combinator:parse reader (list rule)))))
    (when match
      (values (cltpt/transform:decode reader match rule)))))