(in-package :cltpt/org-mode)

(defun org-src-block-lang (obj)
  (let ((lang-match (cltpt/combinator:find-submatch
                     (cltpt/base:text-object-match obj)
                     'lang)))
    (cltpt/base:text-object-match-text obj lang-match)))

(defun org-src-block-result-type (obj)
  (let ((results (org-block-keyword-value obj "results")))
    (if (stringp results)
        (intern (string-upcase results) :keyword)
        :output)))

(defun parse-babel-var-spec (str)
  "parse a :var value like \"a=blk1\" into a (NAME . VALUE) cons."
  (let ((eq-pos (and str (position #\= str))))
    (when eq-pos
      (cons (subseq str 0 eq-pos)
            (subseq str (1+ eq-pos))))))

(defun org-src-block-var-specs (obj)
  "all (NAME . VALUE) var bindings declared across OBJ's :var keyword(s)."
  (loop for (kw . val) in (cltpt/base:text-object-property obj :keywords-alist)
        for spec = (and (equal kw "var") (stringp val) (parse-babel-var-spec val))
        when spec collect spec))

(defun find-org-src-block-by-name (root name)
  "the src-block under ROOT whose :name is NAME, or NIL."
  (let ((found))
    (cltpt/base:map-text-object
     root
     (lambda (obj)
       (when (and (not found)
                  (typep obj 'org-src-block)
                  (equal name (org-block-keyword-value obj "name")))
         (setf found obj))))
    found))

(defun babel-ref-value (obj val)
  "resolve a :var right-hand side VAL to a lisp value, for the block OBJ that declared it.
a VAL naming another src-block in OBJ's document yields that block's computed value, otherwise
VAL is read as a lisp value."
  (let ((blk (find-org-src-block-by-name (cltpt/base:text-object-root obj) val)))
    (if blk
        (org-src-block-value blk)
        (read-from-string val))))

(defun org-src-block-assignments (blk)
  "the (NAME . VALUE) assignments to bind before BLK's code, resolving each var's referenced block (if any)."
  (loop for (name . ref) in (org-src-block-var-specs blk)
        collect (cons name (babel-ref-value blk ref))))

(defun org-src-block-value (obj)
  "the value OBJ produces, to be consumed by another block's :var."
  (let ((result (eval-block obj)))
    (when result
      (cltpt/reader:reader-fully-consume result)
      (let ((text (coerce result 'string)))
        (if (eq (org-src-block-result-type obj) :value)
            (cltpt/babel:babel-decode
             (intern (string-upcase (org-src-block-lang obj)) :cltpt/babel)
             text)
            text)))))

(defmethod eval-block ((obj org-src-block))
  (let* ((code (org-src-block-code obj))
         (lang (org-src-block-lang obj))
         (eval-property (org-block-keyword-value obj "eval"))
         (should-eval (not (member eval-property
                                   (list "no" "no-export")
                                   :test #'string=)))
         (results-property (org-block-keyword-value obj "results"))
         (result-type (org-src-block-result-type obj))
         (reconstruct-property (org-block-keyword-value obj "reconstruct"))
         (transform-property (org-block-keyword-value obj "transform"))
         (results-rule (cond
                         ((consp results-property) results-property)
                         ;; ((equal results-property "file")
                         ;;  )
                         ;; ((equal results-property "output")
                         ;;  )
                         (t '(cltpt/combinator:atleast-one-discard (cltpt/combinator:all-but nil)))))
         (reconstruct-rule (when (consp reconstruct-property)
                             reconstruct-property)))
    (when (and should-eval
               (member lang '("python" "c") :test #'string=))
      (multiple-value-bind (out-rdr err-rdr)
          (cltpt/babel:babel-eval*
           (intern (string-upcase lang) :cltpt/babel)
           code
           (org-src-block-assignments obj)
           result-type
           :main (not (equal (org-block-keyword-value obj "main") "no")))
        ;; ideally we should be working with streams.. transformer should work in an "async" manner
        ;; with the parser.
        (let* ((match (car (cltpt/combinator:parse out-rdr (list results-rule))))
               (result (when match
                         (or (when reconstruct-rule
                               (cltpt/transform:reconstruct out-rdr match reconstruct-rule))
                             (when transform-property
                               (funcall transform-property out-rdr match))
                             (cltpt/combinator:match-text match out-rdr)))))
          (when result
            (cltpt/reader:reader-from-string result)))))))

(defmethod eval-blocks ((doc org-document))
  "evaluate the code of org-src-block instances in DOC and register the results as scheduled changes."
  (labels ((handle-obj (obj)
             (when (typep obj 'org-src-block)
               (let* ((result (eval-block obj))
                      (base-begin (cltpt/base:text-object-begin-in-root obj))
                      (results-match (cltpt/base:text-object-find-submatch
                                      obj
                                      'results))
                      ;; if the code block already contains results, we want results-begin and
                      ;; results-end to point to the region of the pre-existing results, causing
                      ;; a replacement operation. otherwise, we want them to point to the end of the
                      ;; src-block, causing an insertion operation.
                      (results-begin (if results-match
                                         (cltpt/combinator:match-begin-absolute results-match)
                                         (+ base-begin (cltpt/base:text-object-text-length obj))))
                      (results-end (if results-match
                                       (cltpt/combinator:match-end-absolute results-match)
                                       (+ base-begin (cltpt/base:text-object-text-length obj)))))
                 (when result
                   (cltpt/reader:reader-fully-consume result)
                   (cltpt/buffer:schedule-change*
                    doc
                    (cltpt/buffer:make-change
                     :region (cltpt/buffer:make-region :begin results-begin :end results-end)
                     :operator (concatenate 'string
                                            (format nil (if results-match
                                                            "#+RESULTS:~%"
                                                            "~%~%#+RESULTS:~%"))
                                            (coerce result 'string))
                     :args '(:delegate nil :reparse t))))))))
    (cltpt/base:map-text-object
     doc
     #'handle-obj)
    (cltpt/buffer:apply-scheduled-changes
     doc
     :on-apply (cltpt/base:make-reparse-callback doc *org-mode*))))

(defmethod cltpt/base:convert-tree :before ((doc org-document) fmt-src fmt-dest &rest args)
  ;; evaluate blocks to prepare them for conversion.
  (when *org-enable-babel*
    (eval-blocks doc)))