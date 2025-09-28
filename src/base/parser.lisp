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
                    (eval
                     ;; we always read macro strings in :cl-user package
                     (let ((*package* (find-package :cl-user)))
                       (read-from-string
                        ;; TODO: this takes it for granted that the sequence for text-macro is 1-char. perhaps it should be arbitrary.
                        ;; skip first char (`*text-macro-char*')
                        (subseq match-text 1))))
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
                  (loop for entry in (getf text-objects :objects)
                        do (if (and (text-object-property entry :open-macro)
                                    (text-object-ends-by entry macro-eval-result))
                               (progn
                                 (setf opening-macro entry)
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
                         (slot-value opening-macro 'text)
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
      (setf (slot-value doc 'text)
            str1)
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

(defmethod find-child-enclosing-region ((obj text-object) (r region))
  (loop for child in (text-object-children obj)
        do (when (region-encloses (text-object-text-region child) r)
             (return-from find-child-enclosing-region
               (if (text-object-children child)
                   (or
                    (find-child-enclosing-region
                     child
                     (let ((new-region
                             (region-clone (text-object-text-region child))))
                       (region-decf new-region
                                    (region-begin (text-object-text-region child)))
                       new-region))
                    child)
                   child))))
  obj)

;; this is used for incremental parsing, it is a destructive operation.
;; currently the method is to reparse the parent that contains the region where
;; the change happened. it can probably be made more efficient with heuristics.
(defmethod handle-changed-regions ((obj text-object)
                                   text-object-types
                                   changes
                                   reparse)
  "given a text-object tree OBJ, handle a list of changes that happened in the given regions.

CHANGES is an alist of the form (region . new-str) where each pair describes the
region that changed and the new string that it now holds (or should hold)."
  ;; TODO: we are assuming the changes are sorted by region/location
  ;; the 'offset' variable is kept updated according to the changes in the string.
  (let ((offset 0))
    (loop for (new-str . region) in changes
          ;; we find the (possibly nested) child that strictly encloses the region of change.
          do (setf region (region-clone region))
             (region-incf region offset)
             (let* ((child (find-child-enclosing-region obj region))
                    (parent (text-object-parent child))
                    (child-text (text-object-text child))
                    (rel-child-region (relative-child-region obj child))
                    (child-region (region-clone (text-object-text-region child)))
                    (new-child-text
                      (concatenate 'string
                                   (subseq child-text
                                           0
                                           (- (region-begin region)
                                              (region-begin rel-child-region)))
                                   new-str
                                   (subseq child-text
                                           (- (region-end region)
                                              (region-begin rel-child-region)))))
                    (child-idx
                      (when parent
                        (position
                         child
                         (text-object-children parent))))
                    (prev-sibling-cons
                      (when (and parent (> child-idx 0))
                        (nthcdr
                         (1- child-idx)
                         (text-object-children (text-object-parent child)))))
                    (next-siblings (cddr prev-sibling-cons))
                    (change-in-child-end))
               ;; adjust the text of the object accordingly
               (setf (text-object-text child) new-child-text)
               (setf change-in-child-end
                     (- (text-object-end child)
                        (region-end child-region)))
               (incf offset change-in-child-end)
               (when reparse
                 (let* ((new-root-text (text-object-text (text-object-root child)))
                        (prev-result (cltpt/base:text-object-property
                                      child
                                      :combinator-match))
                        (new-result
                          (if (typep child 'document)
                              ;; TODO: this is bad because in this case we're just
                              ;; reparsing eveything..
                              (cltpt/combinator:parse
                               new-root-text
                               (remove-if-not
                                'identity
                                (loop for type1 in text-object-types
                                      collect (text-object-rule-from-subclass
                                               type1))))
                              (cltpt/combinator:scan-all-rules
                               (getf (car prev-result) :ctx)
                               new-root-text
                               (cltpt/combinator:context-rules
                                (getf (car prev-result) :ctx))
                               (text-object-begin-in-root child)
                               (text-object-end-in-root child))))
                        ;; the way we are using `new-objects' here as a plist
                        ;; and then turn it into a list isnt pretty. we should
                        ;; use another way of passing the list to the function
                        ;; and getting back the result that isnt hacky.
                        (new-objects (list :objects nil)))
                   (loop for m in new-result
                         do (handle-match new-root-text
                                          m
                                          new-objects
                                          text-object-types))
                   (setf new-objects (getf new-objects :objects))
                   ;; we need to replace the old child with the new parse results.
                   (cond
                     ((and parent prev-sibling-cons)
                      (progn
                         (setf (cdr prev-sibling-cons-cons) new-objects)
                         (setf (cdr (last new-objects)) next-siblings)))
                     (parent
                      (setf (text-object-children (text-object-parent child))
                            (concatenate 'list new-objects next-siblings)))
                     (t
                      (setf (text-object-children child)
                            new-objects)))
                   (when parent
                     (loop for new-child in new-objects
                           do (setf (text-object-parent new-child)
                                    parent)))))
               ;; we need to change the positions of the next siblings in the tree
               ;; according to the change in their sibling
               (when next-siblings
                 (loop for sibling in next-siblings
                       do (region-incf (text-object-text-region sibling)
                                       change-in-child-end)))))))

;; this is used for incremental parsing. it takes a position at which the
;; modification happened, and modifies the object tree accordingly.
;; currently, it runs in nlogn time in the length of the string, because we will
;; have to modify the strings stored in a specific path in the object tree. linear
;; time is unavoiadble if we are detecting changes by position in the string.
;; the logn might be easier to get rid of by not storing repeated portions of the
;; buffer in the object tree (why are we doing this anyway? i should have that resolved)
;; but atleast we arent reparsing which in all cases will be worse than linear
;; time.
;; in the future, perhaps we can use a more sophisticated data structure that can
;; keep track of changes in a way that doesnt require us to "split" the string and
;; "reconcatenate" which is the operation that requires linear time here.
(defmethod handle-change ((obj text-object) text-object-types change-pos new-str)
  (let* ((change-in-length (- (length new-str)
                              (length (text-object-text obj))))
         ;; this is a naive setting, we act as if a deletion or insertion
         ;; happened precisely at `change-pos'.
         ;; t means deletion, nil means insertion
         (is-deletion (> (length (text-object-text obj))
                         (length new-str)))
         (changed-region (if is-deletion
                             (make-region :begin change-pos
                                          :end (+ change-pos change-in-length))
                             (make-region :begin change-pos :end change-pos))))
    (if is-deletion
        ;; if its a deletion the change is replacing the region with an empty string.
        (handle-changed-regions obj (list (cons "" changed-region)) t)
        ;; if its an insertion we need to provide the inserted string, the region
        ;; of change is of size 0 so no text is replaced, only new text inserted.
        (handle-changed-regions
         obj
         text-object-types
         (list (cons (subseq new-str change-pos (+ change-pos change-in-length))
                     changed-region))
         t))))