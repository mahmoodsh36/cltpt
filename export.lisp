(in-package :cltpt)

(defun escape-text (text backend)
  (case backend
    ('latex (latex-escape-chars text))
    ('html text)
    (t text)))

(defun export-tree (text-obj backend text-object-types)
  (let* ((result (text-object-export text-obj backend))
         (result-is-string (typep result 'string))
         (to-escape (or result-is-string (getf result :escape t)))
         (to-reparse (unless result-is-string (getf result :reparse)))
         (region-to-reparse (when (and to-reparse (not result-is-string))
                              (getf result :reparse-region)))
         (export-text
           (if result-is-string
               result
               (getf result :text)))
         (region-to-escape (when (and to-escape (not result-is-string))
                             (getf result :escape-region)))
         (to-recurse (or result-is-string (getf result :recurse) to-reparse)))
    ;; (region-text region-to-reparse (getf result :text))
    (if to-recurse
        (let ((final-result "")
              (idx 0)
              (children (sort-text-objects
                         (if to-reparse
                             (parse (if region-to-reparse
                                        (region-text region-to-reparse export-text)
                                        export-text)
                                    text-object-types
                                    :as-doc nil
                                    :relative-positions t)
                             (text-object-children text-obj))))
              (child-offset (if region-to-reparse
                                (region-begin region-to-reparse)
                                0)))
          (loop for child in children
                do (let* ((child-result (export-tree child
                                                     backend
                                                     text-object-types))
                          (text-in-between-begin idx)
                          (text-in-between-end (+ child-offset (text-object-begin child)))
                          ;; text up to next child.
                          ;; escape only the region that is requested for escaping in
                          ;; text-in-between.
                          (text-in-between
                            (if to-escape
                                (if region-to-escape
                                    (extract-modified-substring
                                     export-text
                                     (lambda (my-substr)
                                       (escape-text my-substr backend))
                                     (make-region :begin text-in-between-begin
                                                  :end text-in-between-end)
                                     region-to-escape)
                                    (escape-text
                                     (subseq export-text
                                             text-in-between-begin
                                             text-in-between-end)
                                     backend))
                                (subseq export-text
                                        text-in-between-begin
                                        text-in-between-end))))
                     ;; if we reparsed only a specific region, we need to offset the regions of children
                     (setf final-result
                           (concatenate 'string
                                        final-result
                                        text-in-between
                                        child-result))
                     (setf idx (+ child-offset (text-object-end child)))))
          ;; we need to handle region-to-reparse properly on the remaining text after
          ;; the region of the last child
          (let ((final-text-in-between
                  (if to-escape
                      (if region-to-escape
                          (extract-modified-substring
                           export-text
                           (lambda (my-substr)
                             (escape-text my-substr backend))
                           (make-region :begin idx
                                        :end (length export-text))
                           region-to-escape)
                          (escape-text (subseq export-text idx) backend))
                      (subseq export-text idx))))
            (setf final-result (concatenate 'string
                                            final-result
                                            final-text-in-between)))
          final-result)
        export-text)))