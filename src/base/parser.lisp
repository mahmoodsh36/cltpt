(in-package :cltpt/base)

(defun parse (str1
              text-object-types
              &key
                (doc-type 'document))
  "parse a string, returns an object tree."
  (let* ((text-macro-classes '(text-macro))
         (post-lexer-macro-classes
           '(post-lexer-text-macro))
         (text-objects)
         (data
           (remove-if-not
            'identity
            (loop for type1 in text-object-types
                  collect (let ((rule (text-object-rule-from-subclass type1)))
                            rule))))
         (matches (cltpt/combinator::parse str1 data)))
    (loop for m in matches
          for main-match = (car m)
          for rest = (cdr m)
          do (when (member (getf main-match :id) text-object-types)
               (let* ((match-begin (getf main-match :begin))
                      (match-end (getf main-match :end))
                      (type1 (getf main-match :id))
                      (new-text-object (make-instance type1))
                      (is-lexer-macro (member type1 text-macro-classes)))
                 (if is-lexer-macro
                     (let ((match-text (subseq str1 match-begin match-end))
                           (macro-eval-result))
                       (handler-case
                           (eval (read-from-string
                                  ;; skip first char (`*lexer-text-macro-char*')
                                  (subseq match-text 1)))
                         (error (c)
                           (format t "error while evaluating macro ~A: ~A.~%"
                                   match-text c)
                           (setf macro-eval-result 'broken))
                         (:no-error (result1)
                           ;; (format t "evaluated macro ~A: ~A~%" match-text result1)
                           (setf macro-eval-result result1)
                           (if (typep result1 'text-object)
                               (setf new-text-object result1)
                               (setf new-text-object (make-instance 'text-object)))))
                       (when (equal macro-eval-result 'broken)
                         (setf new-text-object (make-instance 'text-object)))
                       (let ((opening-macro))
                         (loop for entry in text-objects
                               do (when (and
                                         (text-object-property entry :open-macro)
                                         (text-object-ends-by entry macro-eval-result))
                                    (setf opening-macro entry)
                                    (return)))
                         (if opening-macro
                             (progn
                               (setf
                                (text-object-property opening-macro :contents-region)
                                (make-region
                                 :begin (region-length
                                         (text-object-text-region opening-macro))
                                 :end (- match-begin
                                         (region-begin
                                          (text-object-text-region opening-macro)))))
                               (setf
                                (text-object-text-region opening-macro)
                                (make-region
                                 :begin (region-begin
                                         (text-object-text-region opening-macro))
                                 :end match-end))
                               ;; extend the text to contain the whole block
                               (setf
                                (text-object-text opening-macro)
                                (region-text (text-object-text-region opening-macro)
                                             str1))
                               (setf (text-object-property opening-macro :open-macro)
                                     nil))
                             (progn
                               (setf
                                (text-object-property new-text-object :open-macro)
                                t)
                               (setf
                                (text-object-property new-text-object :eval-result)
                                macro-eval-result)
                               (text-object-init new-text-object str1 m)
                               (push new-text-object text-objects)))))
                     (progn
                       (text-object-init new-text-object str1 m)
                       (push new-text-object text-objects))))))
    ;; here we build the text object forest (collection of trees) properly
    (let ((forest (build-forest (loop for o in text-objects
                                      collect (list (text-object-begin o)
                                                    (text-object-end o)
                                                    o)))))
      ;; discard region indicies
      (setf forest (mapcar-forest forest 'caddr))
      ;; set children and parents properly
      (build-text-object-forest forest)
      ;; (print-forest forest)
      (let ((top-level (mapcar 'car forest)))
        ;; (mapc (lambda (item)
        ;;         (format t "~A,  ~A~%" item (text-object-opening-region item)))
        ;;       top-level)
        (let ((doc (make-instance doc-type :text str1)))
          (setf (text-object-text-region doc)
                (make-region :begin 0 :end (length str1)))
          (mapc
           (lambda (entry)
             (text-object-set-parent entry doc)
             (text-object-adjust-to-parent entry doc))
           top-level)
          (setf (text-object-children doc) top-level)
          ;; we need to finalize only after the top-level doc object has been set as parent
          (finalize-text-object-forest forest)
          doc)))))

;; the parent-filtering with :disallow and :allow here may have become redundant
;; after i made changes to apply it at a lower level, i will come back to this
(defun build-text-object-forest (forest)
  ;; set child and parents properly, if their parents' rules allow it
  (map-forest
   forest
   (lambda (tree)
     (unless (atom tree)
       (let* ((parent-rule (text-object-rule-from-subclass (class-name (class-of (car tree)))))
              (disallowed (getf parent-rule :disallow))
              (allowed (getf parent-rule :allow)))
         ;; the rule could be nil in instances where a `text-object' was directly created
         ;; which currently (atleast) happens in the case of macros
         (when parent-rule
           (if (eq disallowed t) ;; do nothing if disallowed=t
               (setf (cdr tree) nil)
               (dolist (child (cdr tree))
                 (let ((allow-child t))
                   (if allowed
                       (unless (eq allowed t) ;; if allowed=t we keep allow-child=t
                         (setf allow-child
                               (loop for type1 in allowed thereis (typep (car child) type1))))
                       (if disallowed
                           (setf allow-child
                                 (loop for type1 in allowed never (typep child type1)))))
                   (when allow-child
                     (text-object-set-parent (car child) (car tree))
                     (text-object-adjust-to-parent (car child) (car tree))))))))))))

;; just a DRY shortcut
(defun finalize-text-object-forest (forest)
  (mapcar-forest
   forest
   (lambda (obj)
     (when obj
       (text-object-finalize obj)))))