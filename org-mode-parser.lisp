(in-package :cltpt)

(defvar *org-mode-text-object-types*
  '(org-block org-keyword inline-math org-header org-link org-drawer))

(defvar *org-dir* (uiop:native-namestring "~/brain/notes/"))

(defun parse-keyword-string (s)
  "given a string S of the form \":kw1 val1 :kw2 :kw3 val3\", returns
a list of (KEY . VALUE) pairs. if a keyword is not followed by a value,
its value is NIL."
  (let ((tokens (str:split " " s))
        (result)
        (current-key nil))
    (dolist (token tokens)
      (if (and (> (length token) 0)
               (char= (char token 0) #\:))
          (progn
            (setf current-key (intern (string-upcase token) :keyword))
            (push (cons current-key nil) result))
          (when current-key
            (setf (cdr (assoc current-key result :test #'eq)) token)
            (setf current-key nil))))
    (nreverse result)))

(defclass org-block (text-object)
  ((rule
    :allocation :class
    :initform
    (list :method 'line-pair
          :data (list :begin '(:regex "^#\\+begin_[a-z]+")
                      :end '(:regex "^#\\+end_[a-z]+")
                      ;; we need to make sure the text after begin_ and end_ is the same
                      :predicate (lambda (b e)
                                   (string= (subseq b 8) (subseq e 6)))))))
  (:documentation "org-mode block."))

(defclass org-header (text-object)
  ((rule
    :allocation :class
    :initform '(:method line-regex
                :data "^\\*+\\s.*")))
  (:documentation "org-mode header."))

(defclass org-keyword (text-object)
  ((rule
    :allocation :class
    :initform '(:method line-regex
                :data "^#\\+[a-z]+: .*")))
  (:documentation "org-mode file-level keyword."))

;; i need to think of a good way to do this.
;; org-babel results could contain a drawer which itself contains newlines. if we simply look
;; for text started by #+results and ended by a newline it wont be sufficient. actually
;; sometimes #+results dont end with a newline but with a new element (header, block, etc..)
;; (defclass org-babel-results (text-object)
;;   ((rule
;;     :allocation :class
;;     :initform `(:method pair
;;                    :data (:begin (:regex "#\\+results:")
;;                           :end (:regex "")))))
;;   (:documentation "org-babel evaluation results."))

;; unused
(defclass org-drawer-slow (text-object)
  ((rule
    :allocation :class
    :initform '(:method pair
                :data (:begin (:regex "(?m):[a-zA-Z]+:$") ;; we need multiline mode (?m) to match end of line ($)
                       :end (:regex "(?i):end:")))))
  (:documentation "org-mode drawer."))

(defclass org-drawer (text-object)
  ((rule
    :allocation :class
    :initform '(:method line-pair
                :data (:begin (:regex "^:[a-zA-Z]+:$")
                       :end (:regex "(?i)^:end:$")))))
  (:documentation "org-mode drawer."))

;; simply dont export drawers
(defmethod text-object-export ((obj org-drawer) backend)
  "")

(defmethod text-object-init :after ((obj org-keyword) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind (kw val)
                     ("\\+([a-z]+): (.*)" (region-text opening-region str1))
                   (cons kw val)))
         (keyword (car result))
         (value (cdr result)))
    (setf (text-object-property obj :value) value)
    (setf (text-object-property obj :keyword) keyword)))

(defmethod text-object-init :after ((obj org-block) str1 opening-region closing-region)
  ;; grab the "type" of the block
  (let ((result (cl-ppcre:register-groups-bind (type1)
                    ("#\\+begin_([a-z]+)" (region-text opening-region str1))
                  type1)))
    (setf (text-object-property obj :type) result))
  ;; we need to grab the keywords after the #+begin_block statement, which come in the form
  ;; of :keyword value up to the next newline character
  (let* ((begin (region-begin opening-region))
         (newline-pos (+ begin
                         (position (string #\newline) (subseq str1 begin) :test 'string=)))
         (space-pos (+ begin (position " " (subseq str1 begin) :test 'string=)))
         (line (when (< space-pos newline-pos)
                 (subseq str1 space-pos newline-pos)))
         (entries (unless (str:emptyp line) (parse-keyword-string line))))
    (dolist (entry entries)
      (setf (text-object-property obj (car entry))
            (or (cdr entry) t)))
    (when (< space-pos newline-pos)
      (setf (region-end (text-object-opening-region obj)) newline-pos))))

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

(defclass org-document (document)
  ()
  (:documentation "org-mode document."))

(defmethod text-object-export ((obj org-document) backend)
  (case backend
    ('latex
     (list :text (format nil "org-latex-doc~%~A" (text-object-text obj))
           :reparse t))
    ('html
     (concatenate 'string "org-html-doc" (string #\newline) (text-object-text obj)) t)))

(defun parse-org-file (filepath)
  (dolist (mytype *org-mode-text-object-types*)
    (sb-mop:finalize-inheritance (find-class mytype)))
  (let* ((result (parse (uiop:read-file-string filepath)
                        *org-mode-text-object-types*
                        :as-doc t
                        :relative-positions t
                        :doc-type 'org-document)))
    result))

(defun export-org-doc (org-doc backend)
  (export-tree org-doc
               backend
               *org-mode-text-object-types*))

(defun export-org-file (src dest &optional (backend 'latex))
  (with-open-file (f dest
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (export-org-doc (parse-org-file src) backend) f)))

(defun list-org-files ()
  (directory (format nil "~A*.org" *org-dir*)))

(defun grab-titles ()
  (remove-if-not
   'identity
   (loop for org-file in (list-org-files)
         collect (let* ((result (parse (uiop:read-file-string org-file)
                                       (list 'org-keyword)
                                       :as-doc nil
                                       :relative-positions t))
                        (title))
                   (mapc
                    (lambda (entry)
                      (when (string= (text-object-property entry :keyword) "title")
                        (setf title (text-object-property entry :value))))
                    result)
                   title))))