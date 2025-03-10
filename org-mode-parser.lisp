(in-package :cltpt)
(asdf:load-system :str)

(defvar *org-mode-text-object-types*
  '(org-block org-keyword inline-math org-header org-link))

(defclass org-block (text-object)
  ((rule
    :allocation :class
    :initform `(:method pair
                   :data (:begin (:regex "#\\+begin_[a-z]+")
                          :end (:regex "#\\+end_[a-z]+")
                          ;; we need to make sure the text after begin_ and end_ is the same
                          :predicate ,(lambda (b e)
                                        (string= (subseq b 8) (subseq e 6)))))))
  (:documentation "org-mode block."))

(defclass org-header (text-object)
  ((rule
    :allocation :class
    :initform '(:method regex
                :data "^\\*+\\s.*")))
  (:documentation "org-mode header."))

(defclass org-keyword (text-object)
  ((rule
    :allocation :class
    :initform '(:name org-keyword
                :method regex
                :data "^#\\+[a-z]+: .*")))
  (:documentation "org-mode file-level keyword."))

(defmethod text-object-init :after ((obj org-keyword) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind (kw val)
                     ("\\+([a-z]+): (.*)" (region-text opening-region str1))
                   (cons kw val)))
         (keyword (car result))
         (value (cdr result)))
    (setf (text-object-property obj :value) value)
    (setf (text-object-property obj :keyword) keyword)))

(defmethod text-object-init :after ((obj org-block) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind (type1)
                     ("#\\+begin_([a-z]+)" (region-text opening-region str1))
                   type1)))
    (setf (text-object-property obj :type) result)))

(defmethod text-object-export ((obj org-keyword) backend)
  (format nil
          "keyword: ~A, val: ~A"
          (text-object-property obj :keyword)
          (text-object-property obj :value)))

(defmethod text-object-export ((obj org-block) backend)
  (let ((block-type (text-object-property obj :type)))
    (cond
      ((string= backend 'latex)
       (list :text (format nil "\\begin{~A}~A\\end{~A}"
                           block-type
                           (text-object-contents obj)
                           block-type)
             :recurse t
             :reparse t))
      ((string= backend 'html)
       (let ((open-tag (format nil "<~A>" block-type))
             (close-tag (format nil "</~A>" block-type))
             (text (format nil
                           "~A~A~A"
                           open-tag
                           (text-object-contents obj)
                           close-tag)))
         (list :text text
               :recurse t
               :reparse-region (make-region :begin (length open-tag)
                                            :end (- (length text) (length close-tag)))
               :reparse t))))))

(defclass org-link (text-object)
  ((rule
    :allocation :class
    :initform '(:method regex
                :data "\\[\\[.*?\\]\\]")))
  (:documentation "org-mode link."))

(defmethod text-object-init :after ((obj org-link) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind
                     (desc dest)
                     ("\\[(\\[.*?\\])*\\[(.*?)\\]\\]" (region-text opening-region str1))
                   (list desc dest)))
         (desc (car result))
         (dest (cadr result)))
    (setf (text-object-property obj :desc) desc)
    (setf (text-object-property obj :dest) dest)))

(defmethod text-object-export ((obj org-link) backend)
  (cond
    ((string= backend 'latex)
     (format nil "\\ref{~A}" (text-object-property obj :dest)))
    ((string= backend 'html)
     (format nil "<a href='~A'></a>" (text-object-property obj :dest)))))

(defun test2 ()
  (let ((mytypes (list 'text-macro 'inline-math 'org-keyword)))
    (export-tree (parse "hello#(test) \\(math\\) 0 #(cltpt::make-block :title \"hi\") hello #(identity \"there\") #(identity 'end-block) what #(identity 'hello)"
                        mytypes)
                 'latex
                 mytypes)))

(defclass org-document (document)
  ()
  (:documentation "org-mode document."))

(defmethod text-object-export ((obj org-document) backend)
  (case backend
    ('latex
     (list :text (format nil "org-latex-doc~%~A" (text-object-text obj))
           :reparse t))
    ('html (values (concatenate 'string "org-html-doc" (string #\newline) (text-object-text obj)) t))))

(defun parse-org-file (filepath)
  (let* ((result (parse (uiop:read-file-string filepath)
                        *org-mode-text-object-types*
                        :as-doc t
                        :relative-positions t
                        :doc-type 'org-document)))
    ;; this doesnt work
    (dolist (mytype *org-mode-text-object-types*)
      (sb-mop:finalize-inheritance (find-class mytype)))
    result))

(defun export-org-doc (org-doc backend)
  (export-tree org-doc
               backend
               *org-mode-text-object-types*))

(defun export-org-file (src dest &optional (backend 'latex))
  (with-open-file (f dest
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
    (write-sequence (export-org-doc (parse-org-file src) backend) f)))

(defun test1 ()
  (export-org-file
   "/home/mahmooz/brain/notes/1707341577.org"
   "/home/mahmooz/work/cl-text/test.out"))