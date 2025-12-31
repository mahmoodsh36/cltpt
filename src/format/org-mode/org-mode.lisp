(defpackage :cltpt/org-mode
  (:use :cl)
  (:export
   :org-list-matcher :*org-mode* :org-mode-text-object-types :init :*org-enable-macros*
   :org-link :org-header :org-block :org-list :org-table :org-block :org-src-block
   :org-example-block))

(in-package :cltpt/org-mode)

(defvar *org-enable-macros* nil)

(defvar *org-enable-babel* nil)

(defun init ()
  (setf (cltpt/base:text-format-text-object-types *org-mode*)
        (concatenate
         'list
         ;; crucial to keep the order of types, especially types like
         ;; org-src-block and org-block because we want org-src-block
         ;; to be attempted first, otherwise org-block might match first
         '(org-list
           org-table
           org-header
           org-link
           org-timestamp
           org-src-block
           org-example-block
           org-export-block
           org-block
           org-prop-drawer
           org-drawer
           org-latex-env
           org-keyword ;; has to be after org-latex-env and before cltpt/latex:latex-env
           cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env
           org-italic
           org-emph
           org-inline-code
           org-comment
           web-link)
         (when *org-enable-macros*
           ;; TODO: make it so that *org-enable-macros* is a non-org-specific var
           ;; and use it for all formats. handle it in text-format.lisp
           '(cltpt/base:text-macro
             cltpt/base:post-lexer-text-macro))))
  ;; (setf (cltpt/base:text-format-name *org-mode*)
  ;;       "org-mode")
  (setf (cltpt/base:text-format-document-type *org-mode*)
        'org-document))

(defvar *org-mode*
  (cltpt/base:make-text-format "org-mode")
  "`text-format' instance of the org-mode format.")

(defun org-mode-text-object-types ()
  (cltpt/base:text-format-text-object-types *org-mode*))

(defun org-mode-inline-text-object-types ()
  (intersection
   '(org-link web-link org-inline-code org-emph org-italic
     cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env)
   (org-mode-text-object-types)))

(defun org-mode-inline-text-object-rule ()
  (mapcar
   (lambda (subclass-name)
     (cltpt/combinator:copy-rule
      (cltpt/base:text-object-rule-from-subclass subclass-name)
      subclass-name))
   (org-mode-inline-text-object-types)))

(defun org-mode-text-object-types-except (exceptions)
  (reverse
   (set-difference (org-mode-text-object-types)
                   exceptions
                   :test 'equal)))

(defvar *org-inline-text-objects-rule*
  '(eval
    (org-mode-inline-text-object-rule)))

;; this is in order to try and reduce the work required to parse files
;; when fetching metadata through roam.
;; TODO: we dont really need most of these object types to be scanned for
;; on their own, for example org-table shouldnt be scanned for by the roamer
;; unless its in the results section of an org-src-block.
;; for this reason this doesnt improve speed by much unfortunately.
;; (defmethod cltpt/roam:text-format-roam-types ((tf (eql *org-mode*)))
;;   "names of sub-classes of `text-object' that should be considered when parsing for roam."
;;   (intersection
;;    '(org-link
;;      org-table
;;      org-header
;;      org-link
;;      org-timestamp
;;      org-src-block
;;      org-export-block
;;      org-block
;;      org-prop-drawer
;;      org-latex-env
;;      org-drawer
;;      org-latex-env
;;      org-keyword
;;      cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env
;;      org-comment
;;      cltpt/base::text-macro
;;      cltpt/base::post-lexer-text-macro)
;;    (cltpt/base:text-format-text-object-types *org-mode*)))

;; even though a property drawer can be considered just a drawer with a special treatment, we implement it as a distinct object from `org-drawer'
(defvar *org-prop-drawer-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:followed-by
      (:pattern (cltpt/combinator:literal-casein ":properties:")
       :id drawer-open-tag)
      cltpt/combinator:at-line-end-p)
     (cltpt/combinator:when-match
      (cltpt/combinator:literal-casein ":end:")
      cltpt/combinator:at-line-start-p)
     ((:pattern
       (cltpt/combinator:consec
        (cltpt/combinator:literal ":")
        (:pattern (cltpt/combinator:symbol-matcher)
         :id drawer-key)
        (cltpt/combinator:literal ":")
        (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
        (:pattern (cltpt/combinator:all-but-newline)
         :id drawer-value))
       :id drawer-entry)))
    :on-char #\:))
(defclass org-prop-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-prop-drawer-rule*))
  (:documentation "org-mode properties drawer."))

;; ideally we should be using a hashmap, but it probably doesnt matter here.
(defmethod org-prop-drawer-get ((obj org-prop-drawer) key)
  (cdr (assoc key (cltpt/base:text-object-property obj :alist)
              :test 'equal)))

(defmethod (setf org-prop-drawer-get) (value (obj org-prop-drawer) key)
  (push (cons key value) (cltpt/base:text-object-property obj :alist)))

(defmethod org-prop-drawer-set ((obj org-prop-drawer) key value)
  (setf (org-prop-drawer-get obj key) value))

(defmethod org-prop-drawer-alist ((obj org-prop-drawer))
  (cltpt/base:text-object-property obj :alist))

(defmethod org-prop-drawer-keys ((obj org-prop-drawer))
  (mapcar 'car (org-prop-drawer-alist obj)))

(defmethod cltpt/base:text-object-init :after ((obj org-prop-drawer) str1 match)
  (loop for submatch in (cltpt/combinator:find-submatch-all match 'drawer-entry)
        do (let ((key (cltpt/combinator:find-submatch submatch 'drawer-key))
                 (val (cltpt/combinator:find-submatch submatch 'drawer-value)))
             (org-prop-drawer-set obj
                                  (cltpt/combinator:match-text key str1)
                                  (cltpt/combinator:match-text val str1)))))

;; simply dont convert drawers (this isnt the correct org-mode behavior tho)
;; TODO: properly convert drawers. drawers can include any elements but headers.
(defmethod cltpt/base:text-object-convert ((obj org-prop-drawer)
                                           (backend cltpt/base:text-format))
  "")

(defvar *org-keyword-rule*
  `(:pattern
    (cltpt/combinator:consec-atleast-one
     (cltpt/combinator:consec
      (cltpt/combinator:literal "#+")
      (:pattern (cltpt/combinator:symbol-matcher)
       :id keyword)
      (cltpt/combinator:literal ":"))
     (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
     (cltpt/combinator:any
      ;; TODO: we shouldnt be enabling text-macros by default, even for keywords.
      ,(cltpt/combinator:copy-rule
        cltpt/base::*post-lexer-text-macro-rule*
        'cltpt/base::post-lexer-text-macro)
      ;; capture an arbitrary sequence until an EOL (or EOF ofc)
      (:pattern (cltpt/combinator:all-but-newline)
       :id value)))
    :on-char #\#))
(defclass org-keyword (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-keyword-rule*))
  (:documentation "org-mode file-level keyword."))

(defun rule-with-org-keywords (rule &optional must-have-keywords)
  "take a parser rule, \"wrap\" it with other rules to match org-like keywords for the object.

by org-like keywords we mean things like '#+name: name-here'.
MUST-HAVE-KEYWORDS determines whether keywords must exist for a match to succeed."
  (if must-have-keywords
      `(cltpt/combinator:consec
        (cltpt/combinator:separated-atleast-one
         ,(string #\newline)
         ,(cltpt/combinator:copy-rule *org-keyword-rule* 'org-keyword))
        ,(string #\newline)
        ,rule)
      `(cltpt/combinator:any
        (cltpt/combinator:consec
         (cltpt/combinator:separated-atleast-one
          ,(string #\newline)
          ,(cltpt/combinator:copy-rule *org-keyword-rule* 'org-keyword))
         ,(string #\newline)
         ,rule)
        ,rule)))

(defmethod cltpt/base:text-object-init :after ((obj org-keyword) str1 match)
  (let* ((match (cltpt/base:text-object-match obj))
         (value-match (cltpt/combinator:find-submatch match 'value))
         (keyword-match (cltpt/combinator:find-submatch match 'keyword))
         (value (and value-match (cltpt/combinator:match-text value-match str1))))
    (setf (cltpt/base:text-object-property obj :value) value)
    (setf (cltpt/base:text-object-property obj :keyword)
          (cltpt/combinator:match-text keyword-match str1))))

(defmethod cltpt/base:text-object-finalize :after ((obj org-keyword))
  (let* ((value (cltpt/base:text-object-property obj :value))
         (child (first (cltpt/base:text-object-children obj))))
    (unless value
      (when (typep child 'cltpt/base:post-lexer-text-macro)
        ;; if we get here, then the value is meant to be the evaluation result
        ;; of the post-lexer text macro.
        (setf (cltpt/base:text-object-property obj :value)
              (cltpt/base::eval-post-lexer-macro child))))))

(defmethod cltpt/base:text-object-convert ((obj org-keyword)
                                           (backend cltpt/base:text-format))
  (let* ((kw (cltpt/base:text-object-property obj :keyword))
         (value (cltpt/base:text-object-property obj :value))
         (final-result))
    ;; TODO: adapt transclusion functionality to changes
    ;; handle transclusions
    ;; (when (and kw (string= kw "transclude") cltpt/roam:*roam-convert-data*)
    ;;   (let* ((org-link-parse-result (cltpt/base:parse *org-mode* value))
    ;;          (first-child (car (cltpt/base:text-object-children
    ;;                             org-link-parse-result))))
    ;;     (when (typep first-child 'org-link)
    ;;       (let* ((dest (cltpt/base:text-object-property first-child :dest))
    ;;              (type (cltpt/base:text-object-property first-child :type))
    ;;              (rmr (getf cltpt/roam:*roam-convert-data* :roamer))
    ;;              (roam-link (cltpt/roam:resolve-link
    ;;                          rmr
    ;;                          (getf cltpt/roam:*roam-convert-data* :node)
    ;;                          obj
    ;;                          ;; if link doesnt have a type, treat it as an 'id' link
    ;;                          (if type
    ;;                              (intern type :cltpt/roam)
    ;;                              'cltpt/roam::file)
    ;;                          dest)))
    ;;         ;; just convert the linked node's object and return that
    ;;         (when roam-link
    ;;           (let* ((dest-node (cltpt/roam:link-dest-node roam-link))
    ;;                  (dest-text-obj (cltpt/roam:node-text-obj dest-node)))
    ;;             (let ((result (cltpt/base:convert-tree
    ;;                            dest-text-obj
    ;;                            *org-mode*
    ;;                            backend)))
    ;;               ;; TODO: for some reason redundant newlines in transcluded
    ;;               ;; documents dont get trimmed.
    ;;               (setf final-result
    ;;                     (list :text result
    ;;                           :reparse nil
    ;;                           :recurse nil
    ;;                           :escape nil)))))))))
    ;; discard everything for now
    (or final-result
        (list :text ""
              :recurse nil))))

(defvar *org-comment-rule*
  '(:pattern
    (cltpt/combinator:when-match
     (cltpt/combinator:consec
      (cltpt/combinator:literal "# ")
      (cltpt/combinator:all-but-newline))
     cltpt/combinator:at-line-start-p)
    :on-char #\#))
(defclass org-comment (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-comment-rule*))
  (:documentation "comment line in org-mode."))

(defmethod cltpt/base:text-object-convert ((obj org-comment)
                                           (backend cltpt/base:text-format))
  "")

(defvar *org-single-timestamp-rule*
  '(cltpt/combinator:consec-with-optional
    "<"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id year)
    "-"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id month)
    "-"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id day)
    (:pattern
     (cltpt/combinator:consec
      " "
      (:pattern (cltpt/combinator:word-matcher)
       :id weekday))
     :optional t)
    (:pattern
     (cltpt/combinator:consec
      " "
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id hour)
      (cltpt/combinator:consec-atleast-one
       ":"
       (:pattern (cltpt/combinator:natural-number-matcher)
        :id minute)
       ":"
       (:pattern (cltpt/combinator:natural-number-matcher)
        :id second)))
     :optional t)
    (:pattern
     (cltpt/combinator:consec-atleast-one
      " +"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id repeat-num)
      (:pattern (cltpt/combinator:word-matcher)
       :id repeat-word))
     :optional t
     :id repeat)
    ">"))
(defvar *org-timestamp-rule*
  `(:pattern
    (cltpt/combinator:consec-atleast-one
     (:pattern ,*org-single-timestamp-rule*
      :id begin)
     "--"
     (:pattern ,*org-single-timestamp-rule*
      :id end))
    :on-char #\<))
(defclass org-timestamp (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-timestamp-rule*))
  (:documentation "a timestamp/date. e.g. <2023-12-28 Thu 18:30:00>."))

(defun org-timestamp-match-to-time (obj match)
  (let* ((day (parse-integer
               (cltpt/base:text-object-match-text
                obj
                (cltpt/combinator:find-submatch match 'day))
               :junk-allowed t))
         (second-str-match (cltpt/combinator:find-submatch match 'second))
         (second-str (when second-str-match
                       (cltpt/base:text-object-match-text obj second-str-match)))
         (second (when second-str
                   (parse-integer second-str :junk-allowed t)))
         (year (parse-integer
                (cltpt/base:text-object-match-text
                 obj
                 (cltpt/combinator:find-submatch match 'year))
                :junk-allowed t))
         (month (parse-integer
                 (cltpt/base:text-object-match-text
                  obj
                  (cltpt/combinator:find-submatch match 'month))
                 :junk-allowed t))
         (hour-match (cltpt/combinator:find-submatch match 'hour))
         (hour-str (when hour-match
                     (cltpt/base:text-object-match-text
                      obj
                      hour-match)))
         (hour (when hour-str
                 (parse-integer hour-str :junk-allowed t)))
         (minute-match (cltpt/combinator:find-submatch match 'minute))
         (minute-str (when minute-match
                       (cltpt/base:text-object-match-text
                        obj
                        minute-match)))
         (minute (when minute-str
                   (parse-integer minute-str :junk-allowed t)))
         (weekday (cltpt/combinator:find-submatch match 'weekday)))
    (local-time:encode-timestamp 0
                                 (or second 0)
                                 (or minute 0)
                                 (or hour 0)
                                 day
                                 month
                                 year)))

(defvar *org-timestamp-bracket-rule*
  '(:pattern
    (cltpt/combinator:consec
     "["
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id year)
     "-"
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id month)
     "-"
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id day)
     " "
     (:pattern (cltpt/combinator:word-matcher)
      :id weekday)
     (cltpt/combinator:consec-atleast-one
      " "
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id hour)
      ":"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id minute)
      ":"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id second))
     "]")
    :on-char #\[))
(defclass org-timestamp-bracket (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-timestamp-bracket-rule*))
  (:documentation "a timestamp/date. e.g. [2023-12-28 Thu 18:30]."))

(defvar *org-list-rule*
  `(org-list-matcher
    ,*org-inline-text-objects-rule*))
(defclass org-list (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-list-rule*))
  (:documentation "org-mode list."))

(defun find-direct-child-by-id (node target-id)
  (when node
    (find target-id
          (cltpt/combinator:match-children node)
          :key 'cltpt/combinator:match-id)))

(defun get-list-type-from-obj (obj list-match)
  (let* ((children (cltpt/combinator:match-children list-match))
         (first-item (when children (first children)))
         (bullet-node (when first-item (find-direct-child-by-id first-item 'list-item-bullet)))
         (marker (when bullet-node (cltpt/base:text-object-match-text obj bullet-node))))
    (if (and marker (string= (string-trim " " marker) "-"))
        :ul
        :ol)))

(defun generate-list-changes (match backend obj &optional base-offset (depth 0))
  "generate changes for converting an org-list match to HTML/LaTeX.
BASE-OFFSET is the absolute position of the top-level text-object's start,
used for all region-decf calculations to get positions relative to the text-object."
  (let* ((changes)
         (match-begin (cltpt/combinator:match-begin-absolute match))
         ;; use provided base-offset, or match-begin for top-level list
         (offset (or base-offset match-begin))
         (list-type (get-list-type-from-obj obj match))
         ;; get bullet marker for ordered list type detection
         (first-item (first (cltpt/combinator:match-children match)))
         (bullet-node (when first-item (find-direct-child-by-id first-item 'list-item-bullet)))
         (bullet-marker (when bullet-node (cltpt/base:text-object-match-text obj bullet-node)))
         (open-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html*
              (if (eq list-type :ul)
                  (format nil "<ul>~%")
                  (let ((ol-type (get-html-ol-type bullet-marker)))
                    (format nil "<ol~@[ type=\"~A\"~]>~%" ol-type))))
             (cltpt/latex:*latex*
              (if (eq list-type :ul)
                  (format nil "\\begin{itemize}~%")
                  (let ((renew-cmd (get-latex-label-command bullet-marker depth)))
                    (format nil "\\begin{enumerate}~%~@[~A~%~]" renew-cmd))))))
         (close-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html*
              (if (eq list-type :ul)
                  (format nil "</ul>~%")
                  (format nil "</ol>~%")))
             (cltpt/latex:*latex*
              (if (eq list-type :ul)
                  (format nil "~%\\end{itemize}~%")
                  (format nil "~%\\end{enumerate}~%"))))))
    ;; schedule change for opening tag first
    (setf changes
          (list (cltpt/buffer:make-change
                 :region (cltpt/buffer:region-decf
                          (cltpt/buffer:make-region
                           :begin match-begin
                           :end match-begin)
                          offset)
                 :operator open-tag)))
    ;; process list items in order
    (dolist (child (cltpt/combinator:match-children match))
      (when (eq (cltpt/combinator:match-id child) 'list-item)
        (let* ((bullet-match (find-direct-child-by-id child 'list-item-bullet))
               (content-match (find-direct-child-by-id child 'list-item-content))
               (bullet-begin (when bullet-match (cltpt/combinator:match-begin-absolute bullet-match)))
               (bullet-end (when bullet-match (cltpt/combinator:match-end-absolute bullet-match)))
               (item-end (cltpt/combinator:match-end-absolute child)))
          ;; bullet replacement
          (when bullet-match
            (setf changes
                  (nconc changes
                         (list (cltpt/buffer:make-change
                                :region (cltpt/buffer:region-decf
                                         (cltpt/buffer:make-region :begin bullet-begin :end bullet-end)
                                         offset)
                                :operator (cltpt/base:pcase backend
                                            (cltpt/html:*html* "<li>")
                                            (cltpt/latex:*latex* "\\item ")))))))
          ;; nested lists (recurse)
          (when content-match
            (dolist (content-child (cltpt/combinator:match-children content-match))
              (when (eq (cltpt/combinator:match-id content-child) 'org-list)
                (setf changes
                      (nconc changes
                             (generate-list-changes content-child backend obj offset (1+ depth)))))))
          ;; item close
          (setf changes
                (nconc changes
                       (list (cltpt/buffer:make-change
                              :region (cltpt/buffer:region-decf
                                       (cltpt/buffer:make-region :begin item-end :end item-end)
                                       offset)
                              :operator (cltpt/base:pcase backend
                                          (cltpt/html:*html* "</li>")
                                          (cltpt/latex:*latex* "")))))))))
    ;; closing tag last
    (let ((match-end (cltpt/combinator:match-end-absolute match)))
      (setf changes
            (nconc changes
                   (list (cltpt/buffer:make-change
                          :region (cltpt/buffer:region-decf
                                   (cltpt/buffer:make-region :begin match-end :end match-end)
                                   offset)
                          :operator close-tag)))))
    changes))

(defmethod cltpt/base:text-object-convert ((obj org-list)
                                           (backend cltpt/base:text-format))
  (let ((changes (generate-list-changes (cltpt/base:text-object-match obj) backend obj)))
    (list :changes changes
          :recurse t
          :escape nil)))

;; matching an org-list after a header we should only match if the list items
;; start with "State". otherwise its not a list that should be part of the
;; header's metadata.
(defun is-header-metadata-list (ctx reader pos rule list-match)
  (uiop:string-prefix-p
   "State"
   (cltpt/combinator:match-text
    (cltpt/combinator:find-submatch
     list-match
     'list-item-content)
    reader)))

;; TODO: we may want to match tags-rule only if its before a newline
;; (let ((tags-rule
;;         '(cltpt/combinator:consec
;;           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;           (cltpt/combinator:literal ":")
;;           (cltpt/combinator:separated-atleast-one
;;            ":"
;;            (:pattern
;;             (cltpt/combinator:symbol-matcher)
;;             :id tag))
;;           (cltpt/combinator:literal ":")))
;;       (todo-rule
;;         '(:pattern (cltpt/combinator:upcase-word-matcher)
;;           :id todo-keyword)))
;;   (defvar *org-header-rule*
;;     `(:pattern
;;       (cltpt/combinator:consec-atleast-one
;;        (cltpt/combinator:when-match
;;         (cltpt/combinator:consec-with-optional
;;          (:pattern
;;           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
;;           :id stars)
;;          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;          (:optional ,todo-rule)
;;          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;          (:pattern
;;           (cltpt/combinator:all-upto-without
;;            ,tags-rule
;;            ,(string #\newline))
;;           :id title)
;;          (:optional ,tags-rule))
;;         cltpt/combinator:at-line-start-p)
;;        ;; the following is for detecting metadata following an org header
;;        (cltpt/combinator:atleast-one
;;         (cltpt/combinator:consec
;;          (cltpt/combinator:literal ,(string #\newline))
;;          (cltpt/combinator:any
;;           (cltpt/combinator:separated-atleast-one
;;            " "
;;            (cltpt/combinator:any
;;             (:pattern
;;              (cltpt/combinator:consec
;;               (:pattern (cltpt/combinator:upcase-word-matcher)
;;                :id name)
;;               ": "
;;               ,(copy-rule *org-timestamp-rule* 'timestamp))
;;              :id action-active)
;;             (:pattern
;;              (cltpt/combinator:consec
;;               (:pattern (cltpt/combinator:upcase-word-matcher)
;;                :id name)
;;               ": "
;;               ,(copy-rule *org-timestamp-bracket-rule* 'timestamp))
;;              :id action-inactive)))
;;           ,(copy-rule *org-timestamp-rule* 'todo-timestamp)
;;           ,(copy-rule *org-prop-drawer-rule* 'org-prop-drawer)))))
;;       :on-char #\*)))
(let ((tags-rule
        '(cltpt/combinator:consec
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (cltpt/combinator:literal ":")
          (cltpt/combinator:separated-atleast-one
           ":"
           (:pattern
            (cltpt/combinator:symbol-matcher)
            :id tag))
          (cltpt/combinator:literal ":")))
      (todo-rule
        '(:pattern (cltpt/combinator:upcase-word-matcher)
          :id todo-keyword)))
  (defvar *org-header-rule*
    `(:pattern
      (cltpt/combinator:consec-atleast-one
       (cltpt/combinator:when-match
        (cltpt/combinator:any
         ;; capture header with TODO keyword and tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          ,todo-rule
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern
           (cltpt/combinator:all-upto-without
            ,tags-rule
            ,(string #\newline))
           :id title)
          ,tags-rule)
         ;; capture header without todo keyword but with tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern
           (cltpt/combinator:all-upto-without
            ,tags-rule
            ,(string #\newline))
           :id title)
          ,tags-rule)
         ;; capture header with todo keyword but without tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          ,todo-rule
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern (cltpt/combinator:all-but-newline)
           :id title))
         ;; capture header without todo keyword or tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern (cltpt/combinator:all-but-newline)
           :id title)))
        cltpt/combinator:at-line-start-p)
       ;; the following is for detecting metadata following an org header
       (cltpt/combinator:atleast-one
        (cltpt/combinator:consec
         (cltpt/combinator:literal ,(string #\newline))
         (cltpt/combinator:any
          (cltpt/combinator:separated-atleast-one
           " "
           (cltpt/combinator:any
            (:pattern
             (cltpt/combinator:consec
              (:pattern (cltpt/combinator:upcase-word-matcher)
               :id name)
              ": "
              ,(cltpt/combinator:copy-rule *org-timestamp-rule* 'timestamp
                          :type 'org-timestamp))
             :id action-active)
            (:pattern
             (cltpt/combinator:consec
              (:pattern (cltpt/combinator:upcase-word-matcher)
               :id name)
              ": "
              ,(cltpt/combinator:copy-rule *org-timestamp-bracket-rule* 'timestamp))
             :id action-inactive)))
          ,(cltpt/combinator:copy-rule *org-timestamp-rule* 'todo-timestamp
                      :type 'org-timestamp)
          (cltpt/combinator:when-match-after
           ,(cltpt/combinator:copy-rule *org-list-rule* 'org-list)
           is-header-metadata-list)
          ,(cltpt/combinator:copy-rule *org-prop-drawer-rule* 'org-prop-drawer)))))
      :on-char #\*)))
(defclass org-header (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-header-rule*))
  (:documentation "org-mode header."))

(defmethod cltpt/base:text-object-init :after ((obj org-header) str1 match)
  (let* ((stars (cltpt/combinator:find-submatch match 'stars))
         (title (cltpt/combinator:find-submatch match 'title)))
    (setf (cltpt/base:text-object-property obj :level)
          (length (cltpt/combinator:match-text stars str1)))
    (setf (cltpt/base:text-object-property obj :title)
          (cltpt/combinator:match-text title str1))
    (setf (cltpt/base:text-object-property obj :tags)
          (loop for match in (cltpt/combinator:find-submatch-all match 'tag)
                collect (cltpt/combinator:match-text match str1)))))

(defun get-repeat-interval (repeat-num repeat-word)
  (let ((repeat-num (parse-integer repeat-num :junk-allowed t)))
    (cond
      ((equal repeat-word "w")
       (list :week repeat-num))
      ((equal repeat-word "d")
       (list :day repeat-num))
      ((equal repeat-word "h")
       (list :hour repeat-num)))))

(defun repeat-interval-from-timestamp-match (obj ts-match)
  (let* ((repeat-num-match (cltpt/combinator:find-submatch ts-match 'repeat-num))
         (repeat-word-match (cltpt/combinator:find-submatch ts-match 'repeat-word))
         (repeat-num (when repeat-num-match
                       (cltpt/base:text-object-match-text obj repeat-num-match)))
         (repeat-word (when repeat-word-match
                        (cltpt/base:text-object-match-text obj repeat-word-match))))
    (when (and repeat-num repeat-word)
      (get-repeat-interval repeat-num repeat-word))))

(defun handle-time-match (obj ts-match &optional (record (cltpt/agenda:make-task-record)))
  (let* ((begin-ts-match
           (cltpt/combinator:find-submatch ts-match 'begin))
         (end-ts-match
           (cltpt/combinator:find-submatch ts-match 'end))
         (begin-time (org-timestamp-match-to-time obj begin-ts-match))
         (end-time
           (when end-ts-match
             (org-timestamp-match-to-time obj end-ts-match)))
         (repeat-interval
           (repeat-interval-from-timestamp-match obj begin-ts-match)))
    (if end-time
        (setf (cltpt/agenda:task-record-time record)
              (cltpt/agenda:make-time-range
               :begin begin-time
               :end end-time))
        (setf (cltpt/agenda:task-record-time record)
              begin-time))
    (setf (cltpt/agenda:task-record-repeat record) repeat-interval)
    record))

(defmethod cltpt/base:text-object-finalize ((obj org-header))
  (let* ((match (cltpt/base:text-object-match obj))
         (title-match (cltpt/combinator:find-submatch match 'title))
         (header-id)
         (task-records)
         (todo-keyword-match
           (cltpt/combinator:find-submatch match 'todo-keyword))
         (timestamp-matches
           (cltpt/combinator:find-submatch-all match 'todo-timestamp))
         (action-active-matches
           (cltpt/combinator:find-submatch-all match 'action-active))
         (action-inactive-matches
           (cltpt/combinator:find-submatch-all match 'action-inactive)))
    (loop for ts-match in timestamp-matches
          do (let ((new-record (handle-time-match obj ts-match)))
               (push new-record task-records)))
    (labels ((is-drawer (obj2)
               (typep obj2 'org-prop-drawer)))
      (let ((drawers (cltpt/base:find-children obj #'is-drawer)))
        (loop for drawer in drawers
              do (loop for (key . value) in (org-prop-drawer-alist drawer)
                       do (cond
                            ((string-equal key "id")
                             (setf header-id value))
                            ((string-equal key "last_repeat")
                             ;; TODO
                             )
                            )))))
    (loop for action-match in action-active-matches
          do (let ((action-name
                     (cltpt/base:text-object-match-text
                      obj
                      (cltpt/combinator:find-submatch action-match 'name)))
                   (action-timestamp
                     (cltpt/combinator:find-submatch action-match 'timestamp)))
               (cond
                 ((string-equal action-name "scheduled")
                  (push (handle-time-match
                         obj
                         action-timestamp
                         (cltpt/agenda:make-record-scheduled))
                        task-records))
                 ((string-equal action-name "deadline")
                  (push (handle-time-match
                         obj
                         action-timestamp
                         (cltpt/agenda:make-record-deadline))
                        task-records))
                 ((string-equal action-name "closed")
                  ))))
    (setf (cltpt/base:text-object-property obj :id) header-id)
    ;; initialize roam data
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node
           :id header-id
           :title (cltpt/base:text-object-match-text obj title-match)
           :desc nil
           :text-obj obj))
    (when todo-keyword-match
      (let ((task (cltpt/agenda:make-task
                   :title (cltpt/base:text-object-match-text obj title-match)
                   :description nil
                   :state (or (cltpt/agenda:state-by-name
                               (cltpt/base:text-object-match-text obj todo-keyword-match))
                              (cltpt/agenda:state-by-name "TODO"))
                   :tags nil
                   :records task-records)))
        (loop for record in task-records
              do (setf (cltpt/agenda:task-record-task record) task))
        (setf (cltpt/base:text-object-property obj :task) task)))))

(defmethod cltpt/base:text-object-convert ((obj org-header)
                                           (backend cltpt/base:text-format))
  (let ((to-not-export
          (member "noexport"
                  (cltpt/base:text-object-property
                   obj
                   :tags)
                  :test 'equal)))
    (if to-not-export
        (list :changes (list (cltpt/buffer:make-change
                              :operator ""
                              :region (cltpt/buffer:make-region
                                       :begin 0
                                       :end (length (cltpt/base:text-object-text obj)))
                              :args '(:discard-contained t))))
        (let* ((obj-text (cltpt/base:text-object-text obj))
               (changes)
               (match (cltpt/base:text-object-match obj))
               (title-match (cltpt/combinator:find-submatch match 'title))
               (match-base (cltpt/combinator:match-begin-absolute match))
               (match-end-abs (cltpt/combinator:match-end-absolute match))
               (close-tag
                 (cltpt/base:pcase backend
                   (cltpt/html:*html*
                    (format nil
                            "</h~A>"
                            ;; we want to start from h2, not h1 in html.
                            (1+ (cltpt/base:text-object-property obj :level))))
                   (cltpt/latex:*latex* "}")))
               (open-tag
                 (cltpt/base:pcase backend
                   (cltpt/html:*html*
                    (format nil
                            "<h~A>"
                            ;; we want to start from h2, not h1 in html.
                            (1+ (cltpt/base:text-object-property obj :level))))
                   (cltpt/latex:*latex*
                    (format nil
                            "\\~Asection{"
                            (cltpt/str-utils:str-dupe
                             "sub"
                             (cltpt/base:text-object-property obj :level))))))
               (postfix-begin (- (cltpt/combinator:match-end-absolute title-match)
                                 match-base))
               ;; use 1+ to account for the extra newline at the end which
               ;; we want removed
               (postfix-end (min (1+ (- match-end-abs match-base))
                                 (length obj-text)))
               (prefix-end (- (cltpt/combinator:match-begin-absolute title-match)
                              match-base))
               ;; the "old postfix" region is the region containing the
               ;; tags, and the metadata after the tags+newline
               (old-postfix-region
                 (cltpt/buffer:make-region
                  :begin postfix-begin
                  :end postfix-end))
               ;; the "old prefix" region is the region containing the TODO
               ;; keyword
               (old-prefix-region
                 (cltpt/buffer:make-region
                  :begin 0
                  :end prefix-end)))
          ;; remove the children in the metadata region
          (loop for child in (copy-seq (cltpt/base:text-object-children obj))
                do (when (cltpt/buffer:region-contains
                          old-postfix-region
                          (cltpt/base:text-object-begin child))
                     (setf (cltpt/base:text-object-children obj)
                           (delete child
                                   (cltpt/base:text-object-children obj)))))
          ;; change the "postfix" text after the title (tags etc)
          (push (cltpt/buffer:make-change :operator close-tag
                                          :region old-postfix-region)
                changes)
          ;; change the "prefix" text before the title (stars etc)
          (push (cltpt/buffer:make-change :operator open-tag :region old-prefix-region) changes)
          (list
           :text obj-text
           :remove-newline-after t
           :changes changes
           :escape t
           ;; escape regions are the title, and the contents of the header
           :escape-regions
           (list
            (cltpt/buffer:make-region
             :begin (cltpt/buffer:region-end old-prefix-region)
             :end (cltpt/buffer:region-begin old-postfix-region))
            (cltpt/buffer:make-region
             :begin (cltpt/buffer:region-end old-postfix-region)
             :end (length obj-text)))
           :recurse t)))))

(defvar *org-link-rule*
  '(:pattern
    (cltpt/combinator:any
     ;; [[type:dest][desc]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
      ":"
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]["
      (:pattern (cltpt/combinator:all-but "[]") :id link-desc)
      "]]")
     ;; [[type:dest]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
      ":"
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]]")
     ;; [[dest]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]]"))
    :on-char #\[))
(defclass org-link (cltpt/base:text-link)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-link-rule*))
  (:documentation "org-mode link."))

;; we're not being clever about it
(defvar *web-link-rule*
  `(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:any
      "https://"
      "http://")
     (cltpt/combinator:all-but-whitespace))
    :on-char #\h))
(defclass web-link (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *web-link-rule*))
  (:documentation "a web link."))

(defmethod cltpt/base:text-object-init :after ((obj web-link) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-convert ((obj web-link) (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/html:*html*)
     (list :text
           (format nil
                   "<a href='~A'>~A</a>"
                   (cltpt/base:text-object-text obj)
                   (cltpt/base:text-object-text obj))
           :escape nil))))

(defvar *org-inline-code-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
     (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
     nil nil nil)
    :on-char #\~))
(defclass org-inline-code (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-inline-code-rule*))
  (:documentation "org-mode inline code (surrounded by tildes)."))

(defmethod cltpt/base:text-object-init :after ((obj org-inline-code) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline) t))

(defmethod cltpt/base:text-object-finalize ((obj org-inline-code))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-inline-code)
                                           (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags
      obj
      "\\verb{"
      "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags
      obj
      "<code>"
      "</code>"))))

(defclass org-table (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (org-table-matcher
                 ;; TODO: currently enabling some text objects like org-italic
                 ;; inside tables causes issues. this happens when there
                 ;; is something like `| cell / 1 | cell / 2 |` where the
                 ;; forward slashes may correspond to an org-italic instance
                 ;; which might break the text-object tree.
                 ;; TODO: its not good that we're including all rules as
                 ;; symbols here. it would cause alot of hashmap lookups
                 ;; during parsing.
                 (eval
                  (set-difference
                   (org-mode-inline-text-object-types)
                   '(org-italic org-emph))))
                :on-char #\|)))
  (:documentation "org-mode table."))

;; TODO: this uses nconc all the time which causes squared complexity
(defmethod cltpt/base:text-object-convert ((obj org-table)
                                           (backend cltpt/base:text-format))
  (let* ((match (cltpt/base:text-object-match obj))
         (changes)
         (match-begin (cltpt/combinator:match-begin-absolute match))
         (escape-regions)
         (open-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html* "<table>")
             (cltpt/latex:*latex*
              (let* ((first-row-match
                       (cltpt/combinator:find-submatch
                        match
                        'table-row))
                     (num-cols
                       (length
                        (cltpt/combinator:find-submatch-all
                         first-row-match
                         'table-cell))))
                (format nil
                        "\\begin{tabular} { |~{~a~^|~}| } \\hline~%"
                        (loop repeat num-cols collect "l"))))))
         (close-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html* "</table>")
             (cltpt/latex:*latex* "\\\\ \\hline\\end{tabular}")))
         ;; if separators are defined, they should be used along with
         ;; open/close tags. so the behavior may differ from one format
         ;; to another.
         (row-open-tag (cltpt/base:pcase backend
                         (cltpt/html:*html* "<tr>")))
         (row-close-tag (cltpt/base:pcase backend
                          (cltpt/html:*html* "</tr>")))
         (cell-open-tag (cltpt/base:pcase backend
                          (cltpt/html:*html* "<td>")))
         (cell-close-tag (cltpt/base:pcase backend
                           (cltpt/html:*html* "</td>")))
         (cell-separator (cltpt/base:pcase backend
                           (cltpt/latex:*latex* "&")))
         (row-separator (cltpt/base:pcase backend
                          (cltpt/latex:*latex* "\\\\ \\hline"))))
    ;; opening tag
    (setf changes
          (list (cltpt/buffer:make-change
                 :operator open-tag
                 :region (cltpt/buffer:make-region
                          :begin 0
                          :end 0))))
    ;; handle table cells
    (loop for row-match in (cltpt/combinator:match-children match)
          for is-first-row = t then nil
          do (case (cltpt/combinator:match-id row-match)
               ('table-row
                (let ((final-open-tag
                        (cltpt/base:concat
                         (remove nil
                                 (list (unless is-first-row row-separator)
                                       row-open-tag))
                         'string)))
                  (nconc changes
                         (list (cltpt/buffer:make-change
                                :operator final-open-tag
                                :region (cltpt/buffer:region-decf
                                         (cltpt/buffer:make-region
                                          :begin (cltpt/combinator:match-begin-absolute
                                                  row-match)
                                          :end (cltpt/combinator:match-begin-absolute
                                                row-match))
                                         match-begin)))))
                (loop for cell-match
                        in (cltpt/combinator:find-submatch-all
                            row-match
                            'table-cell)
                      for is-first-cell = t then nil
                      do (let ((final-open-tag
                                 (cltpt/base:concat
                                  (remove
                                   nil
                                   (list (unless is-first-cell cell-separator)
                                         cell-open-tag))
                                  'string)))
                           (nconc changes
                                  (list
                                   (cltpt/buffer:make-change
                                    :operator final-open-tag
                                    :region (cltpt/buffer:region-decf
                                             (cltpt/buffer:make-region
                                              ;; we use 1- to get the | at the start replaced
                                              :begin (1- (cltpt/combinator:match-begin-absolute cell-match))
                                              :end (cltpt/combinator:match-begin-absolute cell-match))
                                             match-begin)))))
                         (when cell-close-tag
                           (nconc
                            changes
                            (list
                             (cltpt/buffer:make-change
                              :operator cell-close-tag
                              :region (cltpt/buffer:region-decf
                                       (cltpt/buffer:make-region
                                        :begin (cltpt/combinator:match-end-absolute cell-match)
                                        :end (cltpt/combinator:match-end-absolute cell-match))
                                       match-begin))))))
                (nconc changes
                       (list (cltpt/buffer:make-change
                              :operator (or row-close-tag
                                            "")
                              :region (cltpt/buffer:region-decf
                                       (cltpt/buffer:make-region
                                        ;; we add 1- to get the | at the end of the row
                                        :begin (1- (cltpt/combinator:match-end-absolute row-match))
                                        :end (cltpt/combinator:match-end-absolute row-match))
                                       match-begin)))))
               ('table-hrule
                (nconc changes
                       (list (cltpt/buffer:make-change
                              :operator ""
                              :region (cltpt/buffer:region-decf
                                       (cltpt/buffer:make-region
                                        :begin (cltpt/combinator:match-begin-absolute row-match)
                                        :end (cltpt/combinator:match-end-absolute row-match))
                                       match-begin)))))))
    ;; ending tag
    (nconc changes
           (list (cltpt/buffer:make-change
                  :operator close-tag
                  :region (cltpt/buffer:make-region
                           :begin (length (cltpt/base:text-object-text obj))
                           :end (length (cltpt/base:text-object-text obj))))))
    (list :text (cltpt/base:text-object-text obj)
          :changes changes
          :escape nil
          :recurse t)))

(defclass org-document (cltpt/base:document)
  ()
  (:documentation "org-mode document."))

(defmethod handle-parsed-org-keywords ((obj cltpt/base:text-object))
  "takes a text object that was matched with instances of `org-keyword', collect the keywords and their values into an alist and return it."
  (let ((result-alist)
        ;; is-first is only for `org-document' which may contain a drawer
        ;; as the first element. even though this would also affect other elements
        ;; in which the org keywords are meant to be at the top, it is probably
        ;; harmless, but maybe we should fix it anyway.
        ;; TODO: only skip first for `org-document'.
        (is-first t))
    (loop for child in (cltpt/base:text-object-children obj)
          while (or (typep child 'org-keyword) is-first)
          when (typep child 'org-keyword)
            do (let ((kw-name (cltpt/base:text-object-property child :keyword))
                     (kw-value (cltpt/base:text-object-property child :value)))
                 (push (cons kw-name kw-value) result-alist)
                 (setf is-first nil)))
    result-alist))

;; often we need to grab the value from the org-keyword object itself. because
;; the value is dynamic, it might change after parsing. storing it might
;; not be enough for now.
;; TODO: optimize.
(defmethod text-object-org-keyword-value ((obj cltpt/base:text-object) kw-name)
  (let ((my-alist (handle-parsed-org-keywords obj)))
    (cltpt/base:alist-get my-alist kw-name)))

(defmethod cltpt/base:text-object-finalize ((obj org-document))
  (let* ((doc-title)
         (doc-id)
         (doc-desc)
         (doc-date)
         (doc-tags)
         (first-child (first (cltpt/base:text-object-children obj)))
         (first-child-is-drawer (typep first-child 'org-prop-drawer)))
    ;; detect id and title of document, either through property drawer at the top
    ;; or using keywords
    (when first-child-is-drawer
      (setf doc-id (org-prop-drawer-get first-child "ID")))
    (setf (cltpt/base:text-object-property obj :keywords-alist)
          (handle-parsed-org-keywords obj))
    (setf doc-title
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                                "title"))
    (setf doc-desc
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                                "description"))
    ;; denote-style identifier
    (setf doc-id
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                                "identifier"))
    (setf doc-date
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                                "date"))
    (let ((tags-str (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                                          "filetags")))
      ;; avoid first and last ':', split by ':' to get tags
      (setf doc-tags (cltpt/str-utils:str-split (cltpt/base:subseq* tags-str 1 -1) ":")))
    ;; set metadata in the object itself
    (setf (cltpt/base:text-object-property obj :title) doc-title)
    (setf (cltpt/base:text-object-property obj :tags) doc-tags)
    (setf (cltpt/base:text-object-property obj :id) doc-id)
    (setf (cltpt/base:text-object-property obj :date) doc-date)
    (setf (cltpt/base:text-object-property obj :desc) doc-desc)
    ;; initialize roam data
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node
           :id doc-id
           :title doc-title
           :desc doc-desc
           :text-obj obj))
    (let ((header-stack))
      (loop for child in (cltpt/base:text-object-children obj)
            for last-header = (car header-stack)
            do (if (typep child 'org-header)
                   (let ((popped-header child)
                         (this-header-level (cltpt/base:text-object-property
                                             child
                                             :level))
                         (last-header-level
                           (when last-header
                             (cltpt/base:text-object-property last-header :level))))
                     ;; if any headers have a "higher" or equal level, we should
                     ;; discard them and reveal a parent fit to inherit this new
                     ;; header (if any). also, the new header should be at the front
                     ;; of the stack.
                     (loop while (and last-header-level
                                      (<= this-header-level last-header-level))
                           do ;; TODO: optimize this, we are running subseq repeatedly here.
                              (cltpt/base:text-object-extend-in-parent
                               last-header
                               (cltpt/base:text-object-begin child))
                              (setf popped-header (pop header-stack))
                              (setf last-header (car header-stack))
                              (setf last-header-level
                                    (when last-header
                                      (cltpt/base:text-object-property
                                       last-header
                                       :level))))
                     ;; the contents of the header needs to start where the metadata
                     ;; and the title text end (i.e., at the end of the initial match).
                     (setf (cltpt/base:text-object-property child :contents-region-spec)
                           (list :begin-submatch :self
                                 :begin-side :end))
                     (push child header-stack))
                   ;; if this object not a header, and we have a last header, just
                   ;; set this as a child of this last header.
                   (when last-header
                     ;; TODO: optimize this, this is very slow.
                     (cltpt/base:text-object-move child last-header))))
      ;; at the end, the text after the last header (if any) needs to be moved
      ;; into the region of the last header (again, if any. as in, if any headers
      ;; exist at all).
      ;; TODO: note that this behavior is similar to org-mode's in that there is no
      ;; way to prevent some text to be detached from a specific header if the header
      ;; precedes it in the original text. while this should be default behavior,
      ;; maybe we can make it so that its not the only behavior, maybe we should
      ;; support different and independent header hierarchies in the same file,
      ;; because i have personally seen many complaints about this not being possible
      ;; in org-mode itself. this is feasible but will require more work, but for
      ;; now this will be sufficient.
      (when header-stack
        ;; first extend the last header to the end of the document
        (let ((last-last-header (cltpt/base:last-atom header-stack)))
          (cltpt/base:text-object-extend-in-parent
           last-last-header
           (cltpt/base:text-object-end obj)))
        ;; then ensure all headers encompass their children
        (dolist (header (reverse header-stack)) ; process from deepest to shallowest
          (let ((children (cltpt/base:text-object-children header)))
            (when children
              (let* ((header-begin (cltpt/base:text-object-begin header))
                     (header-end (cltpt/base:text-object-end header))
                     (max-child-abs-end
                       (reduce #'max
                               children
                               :key (lambda (child)
                                      (+ (cltpt/base:text-object-begin header)
                                         (cltpt/base:text-object-end child))))))
                (when (> max-child-abs-end header-end)
                  (cltpt/base:text-object-extend-in-parent
                   header
                   max-child-abs-end))))))))))

(defun ensure-latex-previews-generated (org-doc)
  (let ((mylist))
    (cltpt/base:map-text-object
     org-doc
     (lambda (obj)
       (when (or (typep obj 'cltpt/latex:inline-math)
                 (typep obj 'cltpt/latex:display-math)
                 (typep obj 'cltpt/latex:latex-env))
         (push (cltpt/base:text-object-contents obj) mylist))))
    (cltpt/latex-previews:generate-previews-for-latex mylist)))

(defmethod cltpt/base:text-object-convert ((obj org-document)
                                           (backend cltpt/base:text-format))
  ;; by ensuring the previews are generated before conversion we avoid having
  ;; to compile every preview individually later on.
  (when (eq backend cltpt/html:*html*)
    (ensure-latex-previews-generated obj))
  (list :text (cltpt/base:text-object-text obj)
        :escape t
        :recurse t))

(defclass org-emph (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (cltpt/combinator:flanked-by-whitespace-or-punctuation
                 (cltpt/combinator:pair
                  (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                  (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                  nil
                  nil
                  nil))
                :on-char #\*)))
  (:documentation "org-mode emphasized text (surrounded by asterisks)."))

(defmethod cltpt/base:text-object-init :after ((obj org-emph) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

;; this function shouldnt have a repeated effect on repeated calls, since
;; it is called in finalization methods and those are called when
;; handling incremental changes.
(defun compress-contents-region-by-one (obj)
  ;; set contents region to span the entire object text, compressed by 1 on each side
  (setf (cltpt/base:text-object-property obj :contents-region-spec)
        (list :compress 1)))

(defmethod cltpt/base:text-object-finalize ((obj org-emph))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-emph) (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags obj "\\textbf{" "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags obj "<b>" "</b>"))))

;; TODO: in org-mode, slashes are interpreted as italic text only if they are
;; preceded/succeeded by spaces.
(defvar *org-italic-rule*
  '(:pattern
    (cltpt/combinator:flanked-by-whitespace-or-punctuation
     (cltpt/combinator:pair
      (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
      (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
      nil
      nil
      nil))
    :on-char #\/))
(defclass org-italic (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-italic-rule*))
  (:documentation "org-mode italicized text (surrounded by forward slahes)."))

(defmethod cltpt/base:text-object-init :after ((obj org-italic) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-finalize ((obj org-italic))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-italic)
                                           (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags obj "\\textit{" "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags obj "<i>" "</i>"))))

;; we dont want a keyword to be matched as a value, e.g. ":keyword1 :keyword2".
;; so we discard any match starting with ":".
(let ((not-starting-with-colon
        (lambda (ctx input pos)
          (not (char= (elt input pos) #\:)))))
  (defvar *keywords-rule*
    `(:pattern
      (cltpt/combinator:separated-atleast-one
       (cltpt/combinator:literal " ")
       (cltpt/combinator:any
        ;; this should capture strings or lisp expressions provided
        ;; as a value of a keyword
        (:pattern
         (cltpt/combinator:consec
          (cltpt/combinator:literal ":")
          (:pattern (cltpt/combinator:symbol-matcher)
           :id keyword)
          (cltpt/combinator:literal " ")
          (cltpt/combinator:when-match
           (cltpt/combinator:succeeded-by
            ,(cltpt/combinator:copy-rule
              cltpt/base::*post-lexer-text-macro-rule*
              'cltpt/base::post-lexer-text-macro)
            ;; we make sure to capture lisp expressions only if they are succeeded
            ;; by " :" because otherwise we might capture a word that is part
            ;; of a sequence of words, like ":title my title", which is a case
            ;; that should be handled by the following rule, not this one.
            (cltpt/combinator:any
             (cltpt/combinator:literal " :")
             ;; or its followed by a new line
             (cltpt/combinator:consec-with-optional
              (:pattern (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
               :optional t)
              (cltpt/combinator:literal ,(string #\newline)))))
           ,not-starting-with-colon))
         :id keywords-entry)
        ;; a keywords with a value
        (:pattern
         (cltpt/combinator:consec-atleast-one
          ;; keyword
          (cltpt/combinator:consec
           (cltpt/combinator:literal ":")
           (:pattern (cltpt/combinator:symbol-matcher)
            :id keyword))
          ;; value
          (cltpt/combinator:consec
           (cltpt/combinator:literal " ")
           ;; capture all until the next " :", or until the line ends.
           (:pattern
            (cltpt/combinator:when-match
             (cltpt/combinator:all-upto
              (cltpt/combinator:any
               (cltpt/combinator:literal ,(string #\newline))
               (cltpt/combinator:literal " :")))
             ,not-starting-with-colon)
            :id value)))
         :id keywords-entry)))
      :id keywords)))

(defvar *org-babel-results-rule*
  `(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:any
      (cltpt/combinator:literal-casein "#+results:")
      (cltpt/combinator:consec
       (cltpt/combinator:literal-casein "#+results[")
       (cltpt/combinator:symbol-matcher)
       (cltpt/combinator:literal "]:")))
     (cltpt/combinator:literal ,(string #\newline))
     (:pattern
      (cltpt/combinator:any
       #| detect syntax like
       #+RESULTS[c419e84a898b0cdd18b49e14c750b71743921b86]:
       : (5 1 7 4)
       : more text
       : more text
       |#
       (cltpt/combinator:separated-atleast-one
        ,(string #\newline)
        (cltpt/combinator:consec
         (:pattern
          (cltpt/combinator:any
           (cltpt/combinator:consec
            (cltpt/combinator:literal ": ")
            (cltpt/combinator:all-but-newline))
           (cltpt/combinator:literal ": "))
          :id output-line)))
       #| detect syntax like
       #+RESULTS[ca08ab2a6a58662675694033105ab0b331611fa2]:
       [[file:/tmp/jyBtMrE.svg]]

       the possible elements could be org-link, org-table, org-block
       |#
       (cltpt/combinator:any
        org-export-block
        ,(cltpt/combinator:copy-rule *org-list-rule* 'org-list)
        org-table
        org-block
        org-drawer
        ,(cltpt/combinator:copy-rule *org-link-rule* 'org-link)))
      :id results-content))
    :id results))
(defvar *org-src-block-no-kw-rule*
  `(cltpt/combinator:pair
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (:pattern
         (cltpt/combinator:literal-casein "#+begin_src")
         :id open-tag)
        (cltpt/combinator:literal " ")
        (:pattern
         (cltpt/combinator:symbol-matcher)
         :id lang)
        (cltpt/combinator:literal " ")
        ,*keywords-rule*)
       (cltpt/combinator:consec
        (:pattern
         (cltpt/combinator:literal-casein "#+begin_src")
         :id open-tag)
        (cltpt/combinator:literal " ")
        (:pattern
         (cltpt/combinator:symbol-matcher)
         :id lang)))
      :id begin))
    (cltpt/combinator:unescaped
     (:pattern (cltpt/combinator:literal-casein "#+end_src")
      :id end))
    ;; unlike an `org-block', org-src-block shouldnt contain children (for now)
    nil))
(defvar *org-src-block-rule*
  `(:pattern
    (cltpt/combinator:any
     ;; block with keywords and execution results
     (cltpt/combinator:consec
      ,(cltpt/combinator:copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-src-block-no-kw-rule*
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:literal ,(string #\newline))
      ,*org-babel-results-rule*)
     ;; block with keywords but no execution results
     (cltpt/combinator:consec
      ,(cltpt/combinator:copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-src-block-no-kw-rule*)
     ;; block with results but no keywords
     (cltpt/combinator:consec
      ,*org-src-block-no-kw-rule*
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:literal ,(string #\newline))
      ,*org-babel-results-rule*)
     ;; block with no keywords and no results
     ,*org-src-block-no-kw-rule*)
    :on-char #\#))
(defclass org-src-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-src-block-rule*))
  (:documentation "org-mode src block."))

(defun init-org-src-block (obj str1)
  (setf (cltpt/base:text-object-property obj :contents-region-spec)
        (list :begin-submatch 'begin
              :begin-side :end
              :end-submatch 'end
              :end-side :start))
  (handle-block-keywords obj str1))

(defmethod cltpt/base:text-object-init :after ((obj org-src-block) str1 match)
  (init-org-src-block obj str1))

(defmethod convert-block ((obj cltpt/base:text-object)
                          (backend cltpt/base:text-format)
                          block-type
                          is-code
                          &optional is-verbatim)
  (let* ((exports-keyword (org-block-keyword-value obj "exports"))
         (export-code (or (string= exports-keyword "code")
                          (string= exports-keyword "both")))
         (export-results (or (string= exports-keyword "results")
                             (string= exports-keyword "both")))
         (text (cltpt/base:text-object-text obj))
         (match (cltpt/base:text-object-match obj))
         ;; TODO: code-end-match is really just block-end-match, should be renamed
         ;; when its a regular block, we want to find the last end delimiter
         ;; in this match because there could be nested blocks that have
         ;; their own end delimiters. when its a code block we only
         ;; want the first since there cant be nested code blocks, and we dont
         ;; wanna catch the delimiters inside the results regions (if any)
         (code-end-match
           (if is-code
               (cltpt/combinator:find-submatch match 'end)
               (cltpt/combinator:find-submatch-last match 'end)))
         (results-match (cltpt/combinator:find-submatch match 'results)))
    ;; export "both" by default, if :exports wasnt provided.
    ;; TODO: this shouldnt be the default behavior. we should have it customizable.
    (when (and is-code (not exports-keyword))
      (setf export-code t)
      (setf export-results t))
    (cond
      ;; if we have `:exports none', we shouldnt export
      ((or (and exports-keyword (string= exports-keyword "none"))
           (member block-type (list "comment" "my_comment") :test 'string=))
       (list :changes (list (cltpt/buffer:make-change
                             :operator ""
                             :region (cltpt/buffer:make-region
                                      :begin 0
                                      :end (length (cltpt/base:text-object-text obj)))
                             :args '(:discard-contained t)))))
      (t
       (let* ((all-keywords
                (concatenate
                 'list
                 `(,(cons "type" (cltpt/base:text-object-property obj :type))
                   ,(cons "lang"
                          (when (cltpt/combinator:find-submatch match 'lang)
                            (cltpt/base:text-object-match-text
                             obj
                             (cltpt/combinator:find-submatch match 'lang)))))
                 (cltpt/base:text-object-property obj :keywords-alist)))
              (props
                (loop for (key . value) in all-keywords
                      for result = (unless (or (member key '("exports" "results")
                                                       :test 'string=)
                                               (null value))
                                     (format nil
                                             "data-~A='~A'"
                                             key
                                             value))
                      when result collect result))
              (props-str (cltpt/str-utils:str-join props " "))
              ;; pass :discard-contained to make it discard
              ;; any macros constructed over the opening tag of
              ;; the block.
              (open-tag-args '(:discard-contained t))
              (code-open-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html*
                   (cond
                     (is-code
                      (format nil
                              "<div class='org-src' ~A><pre><code>"
                              props-str))
                     (is-verbatim
                      (format nil
                              "<div class='org-example' ~A><pre>"
                              props-str))
                     (t
                      (format nil
                              "<div class='~A org-block' ~A>"
                              block-type
                              props-str))))
                  (cltpt/latex:*latex*
                   (cond
                     (is-code
                      (format nil "\\begin{~A}" cltpt/latex:*latex-code-env*))
                     (is-verbatim
                      "\\begin{verbatim}")
                     (t
                      (format nil "\\begin{~A}" block-type))))))
              (code-close-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html*
                   (cond
                     (is-code "</code></pre></div>")
                     (is-verbatim "</pre></div>")
                     (t "</div>")))
                  (cltpt/latex:*latex*
                   (cond
                     (is-code
                      (format nil "\\end{~A}" cltpt/latex:*latex-code-env*))
                     (is-verbatim
                      "\\end{verbatim}")
                     (t
                      (format nil "\\end{~A}" block-type))))))
              (changes)
              (results-open-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html* "<div class='org-babel-results'>")
                  (cltpt/latex:*latex*
                   (format nil
                           "\\begin{~A}"
                           cltpt/latex:*latex-code-env*))))
              (results-close-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html* "</div>")
                  (cltpt/latex:*latex*
                   (format nil
                           "\\end{~A}"
                           cltpt/latex:*latex-code-env*))))
              (escape-regions))
         ;; if we wanna export results, we will have to apply some modifications
         ;; to some of the text accordingly so that the #+RESULTS part
         ;; is ommitted and so that the children after it in the results
         ;; block are handled correctly (although it code be "raw" and
         ;; contain no children).
         ;; if its not code, we surround the block's contents with tags
         ;; and convert it.
         (when (and is-code export-results results-match)
           (let* ((match-base (cltpt/combinator:match-begin-absolute match))
                  ;; lines starting with ": "
                  (match-raw-lines
                    (cltpt/combinator:find-submatch-all
                     results-match
                     'output-line))
                  (results-content-match
                    (cltpt/combinator:find-submatch
                     results-match
                     'results-content))
                  (results-text)
                  (results-begin (- (cltpt/combinator:match-begin-absolute results-match)
                                    match-base))
                  (results-end (- (cltpt/combinator:match-end-absolute results-match)
                                  match-base))
                  ;; here results-content-begin is relative to text-object's text region
                  (results-content-begin
                    (- (cltpt/combinator:match-begin-absolute results-content-match)
                       match-base))
                  ;; code-*-tag-region is for the regions related
                  ;; to the code block /before/ the incremental changes.
                  (code-open-tag-region
                    (cltpt/buffer:make-region
                     :begin 0
                     ;; we use 1+ to get rid of the new line at the end of the opening tag
                     :end (1+ (cltpt/buffer:region-begin
                               (cltpt/base:text-object-contents-region obj)))))
                  (code-close-tag-region
                    (cltpt/buffer:make-region
                     :begin (cltpt/buffer:region-end
                             (cltpt/base:text-object-contents-region
                              obj))
                     :end (- (cltpt/combinator:match-end-absolute code-end-match)
                             match-base)))
                  ;; those regions are also /before/ incremental changes
                  (results-content-region
                    (cltpt/buffer:make-region
                     :begin results-content-begin
                     :end results-end))
                  (results-region
                    (cltpt/buffer:make-region
                     :begin results-begin
                     :end results-end))
                  (code-region
                    (cltpt/buffer:make-region
                     :begin (cltpt/buffer:region-end code-open-tag-region)
                     :end (cltpt/buffer:region-begin code-close-tag-region)
                     :props '(:escape-newlines nil)))
                  (raw-results
                    (when match-raw-lines
                      (cltpt/str-utils:str-join
                       (mapcar
                        (lambda (raw-line-match)
                          (subseq (cltpt/base:text-object-match-text obj raw-line-match)
                                  2))
                        match-raw-lines)
                       (string #\newline)))))
             ;; both regions of results and code need escaping.
             ;; but the region for the code shouldnt have the newlines escaped.
             ;; but we need to account for the changes in the preceding
             ;; region because :escape-regions is applied after the
             ;; incremental changes are applied.
             (push
              (cltpt/buffer:make-region
               :begin results-content-begin
               :end results-end)
              escape-regions)
             (push code-region escape-regions)
             ;; we have to push the changes in the correct order. otherwise
             ;; the incremental parser will not function properly.
             ;; changes in the results region
             (push
              (cltpt/buffer:make-change
               :operator results-close-tag
               :region (cltpt/buffer:make-region
                        :begin results-end
                        :end results-end))
              changes)
             (when match-raw-lines
               ;; if its the raw lines (": ") we need to convert, just subseq
               ;; them accordingly to get rid of the colon and space.
               ;; in the other case, its just a collection of org elements
               ;; so we just need to convert them all in the given text region
               ;; of the "results".
               ;; :escape t marks this change as needing escaping (won't be excluded
               ;; from escape-regions in convert-tree)
               (push (cltpt/buffer:make-change :operator raw-results
                                               :region results-content-region
                                               :args '(:escape t))
                     changes))
             (push
              (cltpt/buffer:make-change
               :operator results-open-tag
               :region (cltpt/buffer:make-region
                        :begin results-begin
                        :end results-content-begin))
              changes)
             ;; changes in the code block region
             (push (cltpt/buffer:make-change :operator code-close-tag
                                             :region code-close-tag-region)
                   changes)
             (push (cltpt/buffer:make-change :operator code-open-tag
                                             :region code-open-tag-region
                                             :args open-tag-args)
                   changes)))
         (if changes
             (list :changes changes
                   :recurse t
                   :escape-regions escape-regions
                   :escape t)
             (cltpt/base:rewrap-within-tags
              obj
              code-open-tag
              code-close-tag
              :escape t
              :compress-region (cltpt/buffer:make-region :begin 1 :end 1)
              :escape-region-options (when (or is-code is-verbatim)
                                       (list :escape-newlines nil))
              :open-tag-args open-tag-args)))))))

(defmethod org-src-block-code ((obj org-src-block))
  (let ((code (cltpt/base:text-object-contents obj)))
    (cltpt/str-utils:unindent code)))

(defmethod handle-block-keywords ((obj cltpt/base:text-object) str1)
  (let* ((match (cltpt/base:text-object-match obj))
         (entries (cltpt/combinator:find-submatch-all
                   (cltpt/combinator:find-submatch match 'keywords)
                   'keywords-entry)))
    (loop for entry in entries
          for kw-match = (cltpt/combinator:find-submatch entry 'keyword)
          for val-match = (cltpt/combinator:find-submatch entry 'value)
          for macro-match = (cltpt/combinator:find-submatch
                             entry
                             'cltpt/base:post-lexer-text-macro)
          do (let ((kw (cltpt/combinator:match-text kw-match str1))
                   (val (cond
                          (val-match (cltpt/combinator:match-text val-match str1))
                          (macro-match
                           ;; TODO: we shouldnt be evaluating the macros here. we should be using the value
                           ;; that was already evaluated at the time the object was initialized.
                           (ignore-errors (eval (read-from-string (subseq (cltpt/combinator:match-text macro-match str1) 1)))))
                          (t ""))))
               (push (cons kw val) (cltpt/base:text-object-property obj :keywords-alist))))))

(defmethod cltpt/base:text-object-convert ((obj org-src-block)
                                           (backend cltpt/base:text-format))
  (convert-block obj backend nil t))

(defvar *org-example-block-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped
      (:pattern
       (cltpt/combinator:any
        (cltpt/combinator:consec
         (:pattern
          (cltpt/combinator:literal-casein "#+begin_example")
          :id open-tag)
         (cltpt/combinator:literal " ")
         ,*keywords-rule*)
        (:pattern
         (cltpt/combinator:literal-casein "#+begin_example")
         :id open-tag))
       :id begin))
     (cltpt/combinator:unescaped
      (:pattern (cltpt/combinator:literal-casein "#+end_example")
       :id end))
     nil)
    :id org-example-block
    :on-char #\#))
(defclass org-example-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-example-block-rule*))
  (:documentation "org-mode example block."))

(defmethod cltpt/base:text-object-init :after ((obj org-example-block) str1 match)
  (init-org-src-block obj str1))

(defmethod cltpt/base:text-object-convert ((obj org-example-block)
                                           (backend cltpt/base:text-format))
  (convert-block obj backend "example" nil t))

(defvar *org-block-no-kw-rule*
  `(cltpt/combinator:pair
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (cltpt/combinator::unsucceeded-by
         (cltpt/combinator:literal-casein "#+begin_")
         (cltpt/combinator:any
          (cltpt/combinator:literal-casein "src")
          (cltpt/combinator:literal-casein "example")))
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)
        (cltpt/combinator:literal " ")
        ,*keywords-rule*)
       (cltpt/combinator:consec
        (cltpt/combinator::unsucceeded-by
         (cltpt/combinator:literal-casein "#+begin_")
         (cltpt/combinator:any
          (cltpt/combinator:literal-casein "src")
          (cltpt/combinator:literal-casein "example")))
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)))
      :id begin))
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:consec
       (cltpt/combinator::unsucceeded-by
        (cltpt/combinator:literal-casein "#+end_")
        (cltpt/combinator:any
         (cltpt/combinator:literal-casein "src")
         (cltpt/combinator:literal-casein "example")))
       (:pattern (cltpt/combinator:symbol-matcher)
        :id end-type))
      :id end))
    ;; an org-block can contain every other object except headers
    (eval
     (org-mode-text-object-types-except '(org-header)))))
(defvar *org-block-rule*
  `(:pattern
    (cltpt/combinator:any
     (cltpt/combinator:consec
      ,(cltpt/combinator:copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-block-no-kw-rule*)
     ,*org-block-no-kw-rule*)
    :on-char #\#))
(defclass org-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-block-rule*))
  (:documentation "org-mode block."))

(defmethod org-block-keyword-value ((obj cltpt/base:text-object) kw)
  "return the value associated with a keyword of an `org-block'."
  (cdr (assoc kw
              (cltpt/base:text-object-property obj :keywords-alist)
              :test 'equal)))

(defmethod cltpt/base:text-object-init :after ((obj org-block) str1 match)
  ;; grab the "type" of the block, set content boundaries, need to grab keywords
  (let* ((begin-type-match (cltpt/combinator:find-submatch match 'begin-type))
         (begin-type (cltpt/combinator:match-text begin-type-match str1)))
    (setf (cltpt/base:text-object-property obj :type) begin-type)
    ;; we look for the last instance of 'end because otherwise
    ;; we might capture the 'end of another block nested within this one
    (setf (cltpt/base:text-object-property obj :contents-region-spec)
          (list :begin-submatch 'begin
                :begin-side :end
                :end-submatch 'end
                :end-side :start
                :find-last-end t))
    ;; handle keywords
    (handle-block-keywords obj str1)
    (let ((block-title (org-block-keyword-value obj "title"))
          (block-name (org-block-keyword-value obj "name")))
      ;; handle :name <id> as an anchor for blocks. construct a roam node for each.
      (when block-name
        (setf (cltpt/base:text-object-property obj :roam-node)
              (cltpt/roam:make-node
               :id block-name
               :title block-title
               :desc nil
               :text-obj obj))))))

(defmethod cltpt/base:text-object-convert ((obj org-block)
                                           (backend cltpt/base:text-format))
  (convert-block obj backend (cltpt/base:text-object-property obj :type) nil))

;; an "export block" is very similar to an src-block, just some slight differences.
(defvar *org-export-block-rule*
  `(:pattern
    ,(cltpt/combinator:copy-modify-rule
      *org-src-block-no-kw-rule*
      '((open-tag . "#+begin_export")
        (end . "#+end_export")))
    :id org-export-block
    :on-char #\#))
(defclass org-export-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-export-block-rule*))
  (:documentation "org-mode export block."))

(defmethod cltpt/base:text-object-init :after ((obj org-export-block) str1 match)
  (init-org-src-block obj str1))

(defmethod cltpt/base:text-object-convert ((obj org-export-block)
                                           (backend cltpt/base:text-format))
  (let* ((match (cltpt/base:text-object-match obj))
         (lang-match (cltpt/combinator:find-submatch match 'lang))
         (lang (cltpt/base:text-object-match-text obj lang-match)))
    ;; we only export if the destination matches the lang set by the export block
    (if (string= (cltpt/base:text-format-name backend) lang)
        (list :text (cltpt/base:text-object-contents obj)
              :recurse nil
              :escape nil)
        (list :text ""
              :reparse t))))

(defvar *org-drawer-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:followed-by
      (:pattern ,(cltpt/combinator:handle-rule-string ":%w:")
       :id drawer-open-tag)
      cltpt/combinator:at-line-end-p)
     (cltpt/combinator:when-match
      (cltpt/combinator:literal-casein ":end:")
      cltpt/combinator:at-line-start-p)
     (eval (org-mode-text-object-types-except '(org-header org-drawer))))
    :on-char #\:))
(defclass org-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-drawer-rule*))
  (:documentation "org-mode drawer."))

;; we need an org-specific flavor of latex-env that may accept keywords like #+name
(defvar *org-latex-env-rule*
  `(:pattern
    ,(rule-with-org-keywords
      (cltpt/combinator:copy-rule
       cltpt/latex:*latex-env-rule*
       ;; we cant use an id 'latex-env' because that will cause a child of type
       ;; cltpt/latex:latex-env to be matched. we use 'latex-env-1'
       'latex-env-1)
      t)
    :on-char #\#))
(defclass org-latex-env (cltpt/latex:latex-env)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-latex-env-rule*))
  (:documentation "type for org-mode-specific latex environments (latex-env that may be preceded with keywords like #+name)."))

(defmethod cltpt/base:text-object-finalize ((obj org-latex-env))
  (let* ((keywords-alist (handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption")))
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node :id name
                                :title nil
                                :desc nil
                                :text-obj obj))))

(defmethod cltpt/base:text-object-init :after ((obj org-latex-env) str1 match)
  (declare (ignore str1 match))
  ;; region-in-tags with include-tags=t means we want the entire region from
  ;; the beginning of open-tag to the end of close-tag
  (setf (cltpt/base:text-object-property obj :contents-region-spec)
        (list :begin-submatch 'open-tag
              :begin-side :start
              :end-submatch 'close-tag
              :end-side :end
              :find-last-end t)))