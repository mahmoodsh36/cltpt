(in-package :cltpt/base)

;; do we even needs this as a function much less running `member'?
(defun is-text-macro (typ)
  (let ((text-macro-classes '(text-macro)))
    (member typ text-macro-classes)))

;; receives a match result, returns a text object
;; if MATCH has :id that refers to a `text-object', result will be a `text-object',
;; otherwise it will be a list that may contain many instances of `text-object'
(defun handle-match (str1 match text-objects text-object-types)
  (let ((main-match (car match))
        (rest (cdr match))
        (result)
        (is-new-object t)
        (child-results))
    (loop for child in rest
          do (let ((obj (handle-match str1
                                      child
                                      text-objects
                                      text-object-types)))
               (when obj
                 (if (listp obj)
                     (loop for item in obj
                           do (push item child-results))
                     (push obj child-results)))))
    (if (member (getf main-match :id) text-object-types)
        (let* ((match-begin (getf main-match :begin))
               (match-end (getf main-match :end))
               (type1 (getf main-match :id))
               (new-text-object (make-instance type1))
               (is-lexer-macro (is-text-macro type1)))
          (if is-lexer-macro
              (let ((match-text (subseq str1 match-begin match-end))
                    (macro-eval-result))
                (handler-case
                    (eval (read-from-string
                           ;; skip first char (`*text-macro-char*')
                           (subseq match-text 1)))
                  (error (c)
                    (when cltpt:*debug*
                      (format t "error while evaluating macro ~A: ~A.~%"
                              match-text c))
                    (setf macro-eval-result 'broken))
                  (:no-error (result1)
                    ;; (format t "evaluated macro ~A: ~A~%" match-text result1)
                    (setf macro-eval-result result1)
                    (if (typep result1 'text-object)
                        (setf new-text-object result1)
                        (setf new-text-object (make-instance 'text-object)))))
                (when (equal macro-eval-result 'broken)
                  (setf new-text-object (make-instance 'text-object)))
                (let ((opening-macro)
                      (intermediate-objects))
                  (loop for entry in (getf text-objects :objects)
                        do (if (and (text-object-property entry :open-macro)
                                    (text-object-ends-by entry macro-eval-result))
                               (progn (setf opening-macro entry)
                                      (return))
                               (push entry intermediate-objects)))
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
                              nil)
                        (loop for item in intermediate-objects
                              do (unless (text-object-parent item)
                                   (text-object-set-parent item opening-macro)
                                   (text-object-adjust-to-parent item opening-macro)))
                        (setf is-new-object nil))
                      (progn
                        (setf
                         (text-object-property new-text-object :open-macro)
                         t)
                        (setf
                         (text-object-property new-text-object :eval-result)
                         macro-eval-result)
                        (text-object-init new-text-object str1 match)))))
              (progn
                (text-object-init new-text-object str1 match)
                new-text-object))
          ;; loop in children, if any return text objects, set them as children of this
          ;; the children are already reversed, here we are inserting them also
          ;; in reverse order using *push* (by `text-object-set-parent'),
          ;; but this means they'll be ordered in the parent correctly
          (loop for item in child-results
                do (text-object-set-parent item new-text-object)
                   (text-object-adjust-to-parent item new-text-object))
          (when is-new-object
            (push new-text-object (getf text-objects :objects))
            new-text-object))
        (reverse child-results))))

(defun parse (str1
              text-object-types
              &key
                (doc-type 'document))
  "parse a string, returns an object tree."
  ;; we use a plist for text-objects because we need to modify it in a function
  ;; and it starts as nil
  (let* ((text-objects (list :objects nil))
         (data
           (remove-if-not
            'identity
            (loop for type1 in text-object-types
                  collect (text-object-rule-from-subclass type1))))
         (matches (cltpt/combinator:parse str1 data)))
    (loop for m in matches
          do (handle-match str1 m text-objects text-object-types))
    ;; here we build the text object forest (collection of trees) properly
    (let ((top-level
            (reverse
             (remove-if
              (lambda (item)
                (text-object-parent item))
              (second text-objects))))
          (doc (make-instance doc-type :text str1)))
      (setf (text-object-text-region doc)
            (make-region :begin 0 :end (length str1)))
      (mapc
       (lambda (entry)
         (text-object-set-parent entry doc)
         (text-object-adjust-to-parent entry doc))
       top-level)
      (setf (text-object-children doc) top-level)
      ;; we need to finalize only after the top-level doc object has been set as parent
      (finalize-doc doc)
      doc)))

(defun finalize-doc (doc)
  (loop for child in (text-object-children doc)
        do (finalize-doc child))
  (text-object-finalize doc))