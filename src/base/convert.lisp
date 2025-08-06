(in-package :cltpt/base)

(defun escape-text (text backend escapable-chars)
  (text-format-escape backend text escapable-chars))

(defun collect-escapables (text-object-types)
  (remove-if-not
   'identity
   (loop for type1 in text-object-types
         collect (let ((rule (text-object-rule-from-subclass type1)))
                   (getf rule :escapable)))))

(defun text-object-convert-helper (text-obj backend)
  ;; if the text-object type has a :shared-name, we may want to treat it
  ;; in a special way.
  (let* ((shared-name (text-object-shared-name-from-subclass
                       (class-name (class-of text-obj))))
         (dest-text-obj-type
           (find shared-name
                 (text-format-text-object-types backend)
                 :key (lambda (entry)
                        (text-object-shared-name-from-subclass entry))))
         (dest-rule (when dest-text-obj-type
                      (text-object-rule-from-subclass dest-text-obj-type)))
         (src-match (text-object-property text-obj :combinator-match))
         (src-str (getf (car src-match) :match)))
    (if (and shared-name dest-rule)
        (let ((transformed-string
                (cltpt/transformer:reconstruct-string-from-rule
                 dest-rule
                 src-match)))
          (list :text transformed-string
                :recurse nil
                :reparse nil
                :escape nil))
        (text-object-convert text-obj backend))))

(defun convert-tree (text-obj backend text-object-types)
  (let* ((result (text-object-convert-helper text-obj backend))
         ;; (result (text-object-convert text-obj backend))
         (result-is-string (typep result 'string))
         (to-escape (or result-is-string
                        (getf result :escape)))
         (to-reparse (unless result-is-string
                       (getf result :reparse)))
         (region-to-reparse (when (and to-reparse (not result-is-string))
                              (getf result :reparse-region)))
         (convert-text
           (if result-is-string
               result
               (getf result :text)))
         (region-to-escape (when (and to-escape (not result-is-string))
                             (getf result :escape-region)))
         (to-recurse (or (unless result-is-string (getf result :recurse))
                         to-reparse))
         (escapables (collect-escapables text-object-types)))
    (when *debug*
      (format t "converting object ~A~%" text-obj))
    (if to-recurse
        ;; we store the results as fragments (`final-result-fragments') to avoid concatenating all the time
        (let* ((final-result-fragments)
               (idx 0)
               (original-children (text-object-children text-obj))
               (children (sort-text-objects
                          (if to-reparse
                              (text-object-children
                               (parse (if region-to-reparse
                                          (region-text region-to-reparse convert-text)
                                          convert-text)
                                      text-object-types))
                              original-children)))
               (child-offset (if region-to-reparse
                                 (region-begin region-to-reparse)
                                 0)))
          ;; if we reparsed, we need to reset the parent of the new copies of "children"
          (when to-reparse
            (setf (text-object-children text-obj) children)
            (dolist (child children)
              (setf (text-object-parent child) text-obj)
              ;; (setf (text-object-parent child) nil)
              ;; (text-object-set-parent child text-obj)
              ;; (text-object-adjust-to-parent child text-obj)
              ))
          (loop for child in children
                do (when *debug*
                     (format t "converting child ~A~%" child))
                   (let* ((child-result (convert-tree child
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
                                     convert-text
                                     (lambda (my-substr)
                                       (escape-text my-substr
                                                    backend
                                                    escapables))
                                     (make-region :begin text-in-between-begin
                                                  :end text-in-between-end)
                                     region-to-escape)
                                    (escape-text
                                     (subseq convert-text
                                             text-in-between-begin
                                             text-in-between-end)
                                     backend
                                     escapables))
                                (subseq convert-text
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
                           convert-text
                           (lambda (my-substr)
                             (escape-text my-substr backend escapables))
                           (make-region :begin idx
                                        :end (length convert-text))
                           region-to-escape)
                          (escape-text (subseq convert-text idx) backend escapables))
                      (subseq convert-text idx))))
            (push final-text-in-between final-result-fragments)
            (apply 'str:concat (nreverse final-result-fragments))))
        (if to-escape
            (if region-to-escape
                (extract-modified-substring
                 convert-text
                 (lambda (my-substr)
                   (escape-text my-substr backend escapables))
                 (make-region :begin 0
                              :end (length convert-text))
                 region-to-escape)
                (escape-text convert-text backend escapables))
            convert-text))))

;; this barely handles simple patterns, it cannot be relied on, but it may make some things easier.
(defun convert-rule-with-shared-patterns (tree)
  (let ((mycar (car tree)))
    (if (equal mycar 'consec)
        (str:concat
         (loop for item in (cdr tree)
               collect (if (stringp item)
                           item
                           (if (getf item :pattern)
                               (convert-rule-with-shared-patterns
                                (getf item :pattern))
                               (convert-rule-with-shared-patterns
                                item)))))
        (loop for subrule in (cdr tree)
              for result = (convert-rule-with-shared-patterns subrule)
              when result
                return result))))

(defun convert-file (fmt1 fmt2 src-file dest-file)
  (let* ((text (uiop:read-file-string src-file))
         (result (convert-text fmt1 fmt2 text)))
    (with-open-file (f (uiop:parse-unix-namestring dest-file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-sequence result f))))

(defun convert-text (fmt1 fmt2 text)
  (let* ((text-tree (parse text
                           (text-format-text-object-types fmt1)
                           :doc-type (text-format-text-document-type fmt1)))
         (result (convert-tree text-tree
                               fmt2
                               (text-format-text-object-types fmt1))))
    result))