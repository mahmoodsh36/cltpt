(in-package :cltpt)

(defun filter-text-object-types-by-method (text-object-types method)
  (remove-if-not
   (lambda (type1)
     (let ((rule (text-object-rule-from-subclass type1)))
       (equal (getf rule :method) method)))
   text-object-types))

(defun text-object-types-data (text-object-types)
  (mapcar
   (lambda (type1)
     (getf (text-object-rule-from-subclass type1) :data))
   text-object-types))

;; todo: optimize
(defun parse (str1
              text-object-types
              &key
                (as-doc t)
                (relative-positions t)
                (doc-type 'document))
  "parse a string, returns an object tree."
  (let* ((text-objects)
         (pair-method-data
           (remove-if-not
            'identity
            (loop for type1 in text-object-types
                  collect (let ((rule (text-object-rule-from-subclass type1)))
                            (when (equal (getf rule :method) 'pair)
                              (append (getf rule :data) (list :id type1)))))))
         (line-pair-method-types
           (filter-text-object-types-by-method text-object-types 'line-pair))
         (line-pair-method-data
           (mapcar
            (lambda (type1)
              (let ((rule (text-object-rule-from-subclass type1)))
                (append (getf rule :data) (list :id type1))))
            line-pair-method-types))
         (regex-method-types
           (filter-text-object-types-by-method text-object-types 'regex))
         (regex-method-data
           (mapcar
            (lambda (type1)
              (getf (text-object-rule-from-subclass type1) :data))
            regex-method-types))
         (line-regex-method-types
           (filter-text-object-types-by-method text-object-types 'line-regex))
         (line-regex-method-data (text-object-types-data line-regex-method-types))
         ;; we apply multiple passes for different methods. this is perhaps not the best
         ;; way to do it, but for now it is simpler.
         (pair-matches (find-multiple-pairs str1 pair-method-data))
         ;; for regexes we apply even more passes, one pass per regex..
         (regex-matches (find-regex-multiple str1 regex-method-data regex-method-types))
         (line-regex-matches (find-lines-matching-regex str1
                                                        line-regex-method-data
                                                        line-regex-method-types))
         (line-pair-matches (find-line-pairs str1 line-pair-method-data))
         (all-pair-matches-sorted
           (sort (concatenate 'list pair-matches line-pair-matches)
                 #'< :key #'first))
         (all-regex-matches
           (sort (concatenate 'list regex-matches line-regex-matches)
                 #'< :key #'first)) )
    (loop for match1 in all-pair-matches-sorted
          do (let* ((match-opening-string (caddr match1))
                    (match-closing-string (cadddr match1))
                    (match-opening-begin (car match1))
                    (match-opening-end (+ match-opening-begin (length match-opening-string)))
                    (match-closing-end (cadr match1))
                    (match-closing-begin (- match-closing-end (length match-closing-string)))
                    (type1 (last-atom match1))
                    (is-macro (equal type1 'text-macro))
                    (match-text (subseq str1 match-opening-begin match-closing-end))
                    (macro-eval-result)
                    (new-text-object))
               (if is-macro
                   (progn
                     (handler-case
                         (eval (read-from-string
                                (subseq match-text (length *text-macro-seq*))))
                       (error (c)
                         (format t "error while evaluating macro ~A: ~A.~%" match-text c)
                         (setf macro-eval-result 'broken))
                       (:no-error (result1)
                         ;; (format t "evaluated macro ~A: ~A~%" match-text result1)
                         (setf macro-eval-result result1)
                         (if (typep result1 'text-object)
                             (setf new-text-object result1)
                             (setf new-text-object (make-instance 'text-object)))))
                     (when (equal macro-eval-result 'broken)
                       (setf new-text-object (make-instance 'text-object)))
                     (setf (text-object-property new-text-object :open-macro) t))
                   (progn
                     (setf new-text-object (make-instance type1))))
               (let ((opening-text-region
                       (make-region :begin match-opening-begin :end match-opening-end))
                     (closing-text-region
                       (make-region :begin match-closing-begin :end match-closing-end)))
                 (when is-macro
                   (setf (text-object-property new-text-object :eval-result) macro-eval-result))
                 (text-object-init new-text-object str1 opening-text-region closing-text-region)
                 ;; handle lexical scope of pair (if we found one)
                 (if is-macro
                     (let ((done))
                       (loop for prev-obj in (reverse text-objects)
                             for prev-obj-idx from 0
                             while (not done)
                             ;; check if this macro is the closing of a previous macro, if so,
                             ;; we have a "lexical" scope and we should drop everything in between
                             do (when (and prev-obj
                                           (text-object-property prev-obj :open-macro)
                                           (text-object-ends-by
                                            prev-obj
                                            (text-object-property new-text-object :eval-result)))
                                  ;; this macro is the ending of the previous one, we're done with them
                                  (text-object-init
                                   prev-obj
                                   str1
                                   (make-region :begin (region-begin
                                                        (text-object-opening-region prev-obj))
                                                :end (region-end
                                                      (text-object-closing-region prev-obj)))
                                   (make-region :begin match-opening-begin
                                                :end match-closing-end))
                                  (setf done t)
                                  (setf (text-object-property prev-obj :open-macro) nil)))
                       (unless done
                         (push new-text-object text-objects)))
                     (push new-text-object text-objects)))))
    (loop for match1 in all-regex-matches
          do (let* ((match-begin (car match1))
                    (match-end (cadr match1))
                    (match-type (cadddr match1))
                    (new-text-object (make-instance match-type)))
               (text-object-init new-text-object
                                 str1
                                 (make-region :begin match-begin :end match-end)
                                 nil)
               (push new-text-object text-objects)))
    ;; here we build the text object forest (collection of trees) properly
    (let ((forest (build-tree (loop for o in text-objects
                                    collect (list (text-object-begin o)
                                                  (text-object-end o)
                                                  o)))))
      ;; discard region indicies
      (setf forest (mapcar-forest forest 'caddr))
      ;; set child and parents properly
      (map-forest
       forest
       (lambda (tree)
         (unless (atom tree)
           (dolist (child (cdr tree))
             (text-object-set-parent (car child) (car tree))
             (text-object-adjust-to-parent (car child) (car tree))))))
      ;; (print-forest forest)
      (let ((top-level (mapcar 'car forest)))
        ;; (mapc (lambda (item)
        ;;         (format t "~A,  ~A~%" item (text-object-opening-region item)))
        ;;       top-level)
        (if as-doc
            (let ((doc (make-instance doc-type :text str1)))
              (setf (text-object-opening-region doc) (make-region :begin 0 :end (length str1)))
              (mapc
               (lambda (entry)
                 (text-object-set-parent entry doc)
                 (text-object-adjust-to-parent entry doc))
               top-level)
              (setf (text-object-children doc) top-level)
              ;; finalize objects
              (mapcar-forest
               forest
               (lambda (obj)
                 (when obj
                   (text-object-finalize obj))))
              doc)
            top-level)))))

(defun export-tree (text-obj backend text-object-types &key reparse)
  (let* ((result (text-object-export text-obj backend))
         (result-is-string (typep result 'string))
         (export-text
           (if result-is-string
               result
               (getf result :text)))
         (to-reparse (or reparse (unless result-is-string (getf result :reparse))))
         (to-recurse (or result-is-string (getf result :recurse) to-reparse))
         (region-to-reparse (when to-reparse (getf result :reparse-region)))
         (text-to-reparse (when to-reparse
                            (if region-to-reparse
                                (region-text region-to-reparse export-text)
                                export-text))))
    (if to-recurse
        (let ((final-result "")
              (idx 0)
              (children (sort-text-objects
                         (if to-reparse
                             (parse text-to-reparse
                                    text-object-types
                                    :as-doc nil
                                    :relative-positions t)
                             (text-object-children text-obj)))))
          (loop for child in children
                do (let ((child-result (export-tree child
                                                    backend
                                                    text-object-types
                                                    :reparse reparse))
                         (child-offset (if region-to-reparse
                                           (region-begin region-to-reparse)
                                           0)))
                     ;; if we reparsed only a specific region, we need to offset the regions of children
                     (setf final-result
                           (concatenate 'string
                                        final-result
                                        (subseq export-text
                                                (+ child-offset idx)
                                                (+ child-offset (text-object-begin child)))
                                        child-result))
                     (setf idx (text-object-end child))))
          (setf final-result (concatenate 'string final-result (subseq export-text idx)))
          final-result)
        export-text)))