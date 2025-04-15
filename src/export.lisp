(in-package :cltpt)

(defun escape-text (text backend)
  (case backend
    ('latex (latex-escape-chars text))
    ('html (html-escape-chars text))
    (t text)))

(defun export-tree (text-obj backend text-object-types)
  (let* ((result (text-object-export text-obj backend))
         (result-is-string (typep result 'string))
         (to-escape (or result-is-string
                        (getf result :escape)
                        (getf result :escape-region)))
         (to-reparse (unless result-is-string
                       (or (getf result :reparse)
                           (getf result :reparse-region))))
         (region-to-reparse (when (and to-reparse (not result-is-string))
                              (getf result :reparse-region)))
         (export-text
           (if result-is-string
               result
               (getf result :text)))
         (region-to-escape (when (and to-escape (not result-is-string))
                             (getf result :escape-region)))
         (to-recurse (or (unless result-is-string (getf result :recurse)) to-reparse)))
    (when *debug*
      (format t "exporting object ~A~%" text-obj))
    (if to-recurse
        ;; we store the results as fragments (`final-result-fragments') to avoid concatenating all the time
        (let* ((final-result-fragments)
               (idx 0)
               (original-children (text-object-children text-obj))
               (children (sort-text-objects
                          (if to-reparse
                              (text-object-children
                               (parse (if region-to-reparse
                                          (region-text region-to-reparse export-text)
                                          export-text)
                                      text-object-types))
                              original-children)))
               (child-offset (if region-to-reparse
                                 (region-begin region-to-reparse)
                                 0)))
          ;; if we reparsed, we need to reset the parent of the new copies of "children"
          (when to-reparse
            (format t "woor8 ~A~%" (length (text-object-children text-obj)))
            (setf (text-object-children text-obj) children)
            ;; (setf child-offset 0)
            (dolist (child children)
              (setf (text-object-parent child) text-obj)
              ;; (setf (text-object-parent child) nil)
              ;; (text-object-set-parent child text-obj)
              ;; (text-object-adjust-to-parent child text-obj)
              ))
          (loop for child in children
                do (when *debug*
                     (format t "exporting child ~A~%" child))
                   (let* ((child-result (export-tree child
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
                     (push text-in-between final-result-fragments)
                     (push child-result final-result-fragments)
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
            (push final-text-in-between final-result-fragments)
            (apply 'str:concat (nreverse final-result-fragments))))
        (if to-escape
            (if region-to-escape
                (extract-modified-substring
                 export-text
                 (lambda (my-substr)
                   (escape-text my-substr backend))
                 (make-region :begin 0
                              :end (length export-text))
                 region-to-escape)
                (escape-text export-text backend))
            export-text))))