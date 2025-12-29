(in-package :cltpt/base)

;; receives a match result, returns a text object
;; if MATCH has :rule that has :type, or if MATCH has :id, that refers to a `text-object', result will be a `text-object',
;; otherwise it will be a list that may contain many instances of `text-object'
(defun handle-match (input match existing-objects text-object-types)
  "recursively processes a match from the parser, creating text-objects.

the function passes the state between recursive calls by returning two values:
1. the new text-object or list of objects created at this level of the match tree.
   this value is used by the parent call to adopt new children. it is `nil' if the
   current match is purely structural and not a text-object itself.
2. the updated list of all text-objects found in the entire process so far.
   this value is the 'state' that is passed through every recursive call to ensure
   no created objects are lost."
  (let* ((main-match-rule (cltpt/combinator:match-rule match))
         (main-match-type (or (and (cltpt/base:plistp main-match-rule)
                                   (getf main-match-rule :type))
                              (cltpt/combinator:match-id match)))
         (is-new-object t)
         ;; this list will be built backwards for efficiency.
         (reversed-child-results)
         (current-objects existing-objects))
    ;; collect child objects in reverse order
    (loop for child in (cltpt/combinator:match-children match)
          do (multiple-value-bind (obj updated-objects)
                 (handle-match input
                               child
                               current-objects
                               text-object-types)
               (when obj
                 ;; push the objects returned by the child
                 (if (listp obj)
                     (loop for item in obj
                           do (push item reversed-child-results))
                     (push obj reversed-child-results)))
               (setf current-objects updated-objects)))
    ;; create the final, correctly-ordered list of children with one reversal.
    (let ((child-results (nreverse reversed-child-results)))
      (if (member main-match-type text-object-types)
          ;; this match corresponds to a text-object we need to create.
          (let* ((match-begin (cltpt/combinator:match-begin-absolute match))
                 (match-end (cltpt/combinator:match-end-absolute match))
                 (new-text-object (make-instance main-match-type))
                 (is-lexer-macro (eq main-match-type 'text-macro)))
            (if is-lexer-macro
                (let ((match-text (subseq input match-begin match-end))
                      (macro-eval-result))
                  ;; we always read macro strings in :cl-user package
                  ;; TODO: this takes it for granted that the sequence for text-macro is 1-char. perhaps it should be arbitrary.
                  ;; skip first char (`*text-macro-char*')
                  (handler-case
                      (eval
                       (let ((*package* (find-package :cl-user)))
                         (read-from-string (subseq match-text 1))))
                    (error (c)
                      (when (getf cltpt:*debug* :parse)
                        (format t
                                "error while evaluating macro ~A: ~A.~%"
                                match-text
                                c))
                      (setf macro-eval-result 'broken))
                    (:no-error (result1)
                      (when (getf cltpt:*debug* :parse)
                        (format t "evaluated macro ~A: ~A~%" match-text result1))
                      (setf macro-eval-result result1)
                      (if (typep result1 'text-object)
                          (setf new-text-object result1)
                          (setf new-text-object (make-instance 'text-object)))))
                  (when (equal macro-eval-result 'broken)
                    (setf new-text-object (make-instance 'text-object)))
                  (let ((opening-macro)
                        (intermediate-objects))
                    (loop for entry in current-objects
                          do (if (and (text-object-property entry :open-macro)
                                      (text-object-ends-by entry macro-eval-result))
                                 (progn
                                   (setf opening-macro entry)
                                   (return))
                                 (push entry intermediate-objects)))
                    (if opening-macro
                        (progn
                          (setf (text-object-property opening-macro :contents-region)
                                (cltpt/buffer:make-region
                                 :begin (cltpt/buffer:region-length
                                         (text-object-text-region opening-macro))
                                 :end (- match-begin
                                         (cltpt/buffer:region-begin
                                          (text-object-text-region
                                           opening-macro)))))
                          (setf (text-object-text-region opening-macro)
                                (cltpt/buffer:make-region
                                 :begin (cltpt/buffer:region-begin
                                         (text-object-text-region opening-macro))
                                 :end match-end))
                          (setf (slot-value opening-macro 'text)
                                (cltpt/buffer:region-text (text-object-text-region opening-macro)
                                             input))
                          (setf (text-object-property opening-macro :open-macro)
                                nil)
                          (loop for item in intermediate-objects
                                do (unless (text-object-parent item)
                                     (text-object-set-parent item opening-macro)
                                     (text-object-adjust-to-parent
                                      item
                                      opening-macro)))
                          (setf (text-object-children opening-macro)
                                (nreverse (text-object-children opening-macro)))
                          (setf is-new-object nil))
                        (progn
                          (setf (text-object-property new-text-object :open-macro)
                                t)
                          (setf (text-object-property new-text-object :eval-result)
                                macro-eval-result)
                          (text-object-init new-text-object input match)))))
                (progn
                  (text-object-init new-text-object input match)
                  new-text-object))
            ;; assign the correctly-ordered children to the parent.
            (setf (text-object-children new-text-object) child-results)
            (loop for item in child-results
                  do (setf (text-object-parent item) new-text-object)
                     (text-object-adjust-to-parent item new-text-object))
            (if is-new-object
                (values new-text-object (cons new-text-object current-objects))
                (values nil current-objects)))
          ;; this is a non-text-object match. return the list of children for
          ;; the ancestor to process.
          (values child-results current-objects)))))

;; TODO: this currently doesnt work incrementally with streams. it only works with fixed strings.
;; make it work with streams like the combinator. doing this might be tricky because it would make
;; backtracking costy as we would have to modify the document as we progress.
(defmethod parse ((format text-format)
                  input
                  &key
                    (text-object-types (text-format-text-object-types format))
                    doc)
  "parse a string, returning a document object tree."
  (let* ((all-objects)
         (data
           (remove-if-not
            'identity
            (loop for type1 in text-object-types
                  collect (text-object-rule-from-subclass type1)))))
    (multiple-value-bind (matches escaped) (cltpt/combinator:parse input data)
      (loop for m in matches
            do (multiple-value-bind (top-level-result updated-objects)
                   (handle-match input
                                 m
                                 all-objects
                                 text-object-types)
                 (setf all-objects updated-objects)))
      ;; here we build the text object forest (collection of trees) properly.
      ;; once we are done parsing with the combinator we can safely turn it into a string,
      ;; this ofcourse doesnt work incrementally.
      (let* ((str1 (coerce input 'string))
             (top-level
               (reverse
                (remove-if
                 (lambda (item) (text-object-parent item))
                 all-objects)))
             (doc (or doc
                      (make-instance (text-format-document-type format)
                                     :text str1))))
        (setf (text-object-text-region doc)
              (cltpt/buffer:make-region :begin 0 :end (length str1)))
        (setf (slot-value doc 'text) str1)
        (mapc
         (lambda (entry)
           (text-object-set-parent entry doc)
           (text-object-adjust-to-parent entry doc))
         top-level)
        (setf (text-object-children doc) top-level)
        ;; we need to finalize only after the top-level doc object has been set as parent
        (finalize-doc doc)
        (setf (slot-value doc 'escapes) escaped)
        doc))))

(defun finalize-doc (doc)
  (loop for child in (text-object-children doc)
        do (finalize-doc child))
  (text-object-finalize doc))

;; this is used for incremental parsing. it takes the region where the
;; modification happened, and modifies the object tree accordingly.
;; modifying the string is done in buffer.lisp and runs in linear time.
;; linear time is unavoiadble if we are detecting changes by position in the string.
;; and we arent doing anything smart about it. a gap buffer is an option, but im not sure
;; it would be the most fitting here.
;; in the future, perhaps we can use a more sophisticated data structure that can
;; keep track of changes in a way that doesnt require us to "split" the string and
;; "reconcatenate" which is the operation that requires linear time (in buffer.lisp).
;; TODO: this doesnt work correctly when there is a "complex" change where the part of
;; text before 'target-obj' would form an object with the part next to it that is in
;; 'target-obj'. im not yet sure how to overcome this issue.
(defun reparse-change-in-tree (root format buffer change result-text)
  (let* ((change-region (cltpt/buffer:change-region change))
         (change-start (cltpt/buffer:region-begin change-region))
         (new-text-len (length result-text))
         (post-change-end (+ change-start new-text-len))
         (rules
           (remove-if-not
            'identity
            (loop for type1 in (text-format-text-object-types format)
                  collect (text-object-rule-from-subclass type1))))
         ;; find the deepest child that encloses the changed region
         (target-obj (or (find-child-enclosing-region
                          root
                          (cltpt/buffer:make-region :begin change-start :end post-change-end))
                         root))
         (target-region-in-root (relative-child-region root target-obj))
         (root-text (text-object-text root))
         (target-obj-parent (text-object-parent target-obj))
         (target-obj-idx
           (when target-obj-parent
             (position
              target-obj
              (text-object-children target-obj-parent))))
         (prev-siblings (when target-obj-parent
                          (subseq (text-object-children target-obj-parent) 0 target-obj-idx)))
         (next-siblings (when target-obj-parent
                          (subseq (text-object-children target-obj-parent) (1+ target-obj-idx))))
         (reparse-all (typep target-obj 'document))
         (result
           (if reparse-all
               (parse format root-text :doc target-obj)
               (cltpt/combinator:scan-all-rules
                nil
                root-text
                rules
                (cltpt/buffer:region-begin target-region-in-root)
                (cltpt/buffer:region-end target-region-in-root))))
         (new-objects))
    (unless reparse-all
      (loop for m in result
            do (multiple-value-bind (new-obj updated-objects)
                   (handle-match
                    root-text
                    m
                    new-objects
                    (text-format-text-object-types format))
                 (setf new-objects updated-objects)))
      ;; filter to top-level text objects (no parent)
      (setf new-objects
            (nreverse
             (remove-if
              (lambda (item)
                (text-object-parent item))
              new-objects)))
      ;; here we integrate the "new" objects
      (if target-obj-parent
          (progn
            (setf (text-object-children target-obj-parent)
                  (concatenate 'list
                               prev-siblings
                               new-objects
                               next-siblings))
            (loop for new-child in new-objects
                  do (setf (text-object-parent new-child) target-obj-parent)))
          (setf (text-object-children target-obj) new-objects)))
    (finalize-doc root)))

;; creates a callback function suitable for use with apply-scheduled-changes :on-apply.
;; the callback will reparse the affected region after each change is applied,
;; but only if the change has :reparse t in its args.
(defun make-reparse-callback (root format)
  "create an on-apply callback for apply-scheduled-changes that performs reparsing.

ROOT is the root text-object (typically a document).
FORMAT is the text-format containing the rules for reparsing.

the callback only reparses changes that have :reparse t in their args.
returns a function that can be passed as :on-apply to apply-scheduled-changes."
  (lambda (buffer change result-text)
    (when (getf (cltpt/buffer:change-args change) :reparse)
      (reparse-change-in-tree root format buffer change result-text))))