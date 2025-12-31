(in-package :cltpt/org-mode)

(defmethod eval-block ((obj org-src-block))
  (let* ((code (org-src-block-code obj))
         (lang-match (cltpt/combinator:find-submatch (cltpt/base:text-object-match obj) 'lang))
         (lang (cltpt/base:text-object-match-text obj lang-match))
         (eval-property (org-block-keyword-value obj "eval"))
         (should-eval (not (member eval-property
                                   (list "no" "on-export")
                                   :test #'string=)))
         (results-property (org-block-keyword-value obj "results"))
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
               (member lang '("python") :test #'string=))
      (multiple-value-bind (out-rdr err-rdr)
          (cltpt/babel:babel-eval (intern (string-upcase lang) :cltpt/babel) code)
        ;; ideally we should be working with streams.. transformer should work in an "async" manner
        ;; with the parser.
        (let* ((match (car (cltpt/combinator:parse out-rdr (list results-rule))))
               (result (when match
                         (or (when reconstruct-rule
                               (cltpt/transform:reconstruct out-rdr match reconstruct-rule))
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
                                            (format nil "~%#+RESULTS:~%")
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