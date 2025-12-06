(in-package :cltpt/base)

;; do we even needs this as a function much less running `member'?
(defun is-text-macro (typ)
  (let ((text-macro-classes '(text-macro)))
    (member typ text-macro-classes)))

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
         (main-match-type (or (getf main-match-rule :type)
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
          (let* ((match-begin (cltpt/combinator:match-begin match))
                 (match-end (cltpt/combinator:match-end match))
                 (new-text-object (make-instance main-match-type))
                 (is-lexer-macro (is-text-macro main-match-type)))
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
                              (make-region
                               :begin (region-length
                                       (text-object-text-region opening-macro))
                               :end (- match-begin
                                       (region-begin
                                        (text-object-text-region
                                         opening-macro)))))
                        (setf (text-object-text-region opening-macro)
                              (make-region
                               :begin (region-begin
                                       (text-object-text-region opening-macro))
                               :end match-end))
                        (setf (slot-value opening-macro 'text)
                              (region-text (text-object-text-region opening-macro)
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
            ;; since `text-object-set-parent` uses `push`, this will create a reversed list.
            (loop for item in child-results
                  do (text-object-set-parent item new-text-object)
                     (text-object-adjust-to-parent item new-text-object))
            ;; reverse the parent's children list to restore the correct order.
            (when (slot-exists-p new-text-object 'children)
               (setf (text-object-children new-text-object)
                     (nreverse (text-object-children new-text-object))))
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
              (make-region :begin 0 :end (length str1)))
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

;; this is used for incremental parsing, it is a destructive operation.
;; currently the method is to reparse the parent that contains the region where
;; the change happened. it can probably be made more efficient with heuristics.
(defmethod handle-changed-regions ((obj text-object)
                                   (format text-format)
                                   changes
                                   reparse
                                   &key
                                     (propagate t)
                                     (only-simple-changes))
  "given a text-object tree OBJ, handle a list of changes that happened in the given regions.

CHANGES is an alist of the form (region . new-str) where each pair describes the
region that changed and the new string that it now holds (or should hold).

returns the elements newly inserted into the tree."
  ;; TODO: we are assuming the changes are sorted by region/location
  ;; the 'offset' variable is kept updated according to the changes in the string.
  ;; TODO: if multiple regions are over the same object we should handle them
  ;; at once. i.e. we should group regions by containing element before handling.
  (let ((offset 0)
        (new-elements))
    (loop for (new-str . region) in changes
          ;; we find the (possibly nested) child that strictly encloses the region of change.
          do (setf region (region-incf (region-clone region) offset))
             (let* ((change-in-region-length
                      (- (length new-str)
                         (region-length region)))
                    (child (find-child-enclosing-region obj region))
                    (parent (text-object-parent child))
                    (child-text (text-object-text child))
                    (rel-child-region (relative-child-region obj child))
                    (child-region (region-decf (region-clone region)
                                               (region-begin rel-child-region)))
                    (region-in-root
                      (region-incf (region-clone region)
                                   (region-begin (relative-child-region (text-object-root obj)
                                                                        obj))))
                    (new-child-text (text-object-modify-region
                                     child
                                     new-str
                                     child-region
                                     :propagate propagate))
                    (child-idx
                      (when parent
                        (position
                         child
                         (text-object-children parent))))
                    (prev-sibling-cons
                      (when (and parent child-idx (> child-idx 0))
                        (nthcdr
                         (1- child-idx)
                         (text-object-children (text-object-parent child)))))
                    (next-siblings (cddr prev-sibling-cons)))
               (when (getf cltpt:*debug* :convert)
                 (format t
                         "DEBUG: replaced '~A' with '~A'~%"
                         (region-text child-region child-text)
                         new-str)
                 (format t
                         "DEBUG: text changed from '~A' to '~A'~%"
                         child-text
                         new-child-text))
               ;; TODO: optimize this, it iterates through the tree for each change which
               ;; is very very inefficient.
               ;; TODO: this is very hacky.. we shouldnt be modifying match trees like that..
               (when propagate
                 ;; TODO: we can reduce matches-to-update to only matches past the root of the
                 ;; "current" one.
                 ;; note that we cant just use the matches of the children of the root because
                 ;; of re-structuring that happens after parsing, such as extending header regions
                 ;; in org-document finalization.
                 ;; TODO: this is pretty slow, probably worse than O(n^2log(n)) because of
                 ;; member-checking. if we take into account the whole thing it could be closer
                 ;; to O(n^3log(n)).
                 (let ((matches-to-update
                         (let ((all))
                           (map-text-object
                            (text-object-root obj)
                            (lambda (other-object)
                              ;; we only push the match if its a root match (no parent).
                              (when (slot-boundp other-object 'match)
                                (let ((other-match (text-object-match other-object)))
                                  (when (not (member other-match
                                                     all
                                                     :test #'cltpt/tree:is-descendant))
                                    (push other-match all))))))
                           all)))
                   (loop for match-root in matches-to-update
                         do (when (getf cltpt:*debug* :convert)
                              (format t "match tree before:~%")
                              (cltpt/tree:tree-show match-root))
                         do (cltpt/tree:tree-map
                             match-root
                             (lambda (subtree)
                               ;; if the modified region is before the beginning of the match,
                               ;; we update both :begin and :end of the match, otherwise we
                               ;; only modify :end.
                               (if (< (region-begin region-in-root)
                                      (cltpt/combinator:match-begin subtree))
                                   (progn
                                     (incf (cltpt/combinator:match-begin subtree)
                                           change-in-region-length)
                                     (incf (cltpt/combinator:match-end subtree)
                                           change-in-region-length))
                                   (when (<= (region-end region-in-root)
                                             (cltpt/combinator:match-end subtree))
                                     (incf (cltpt/combinator:match-end subtree)
                                           change-in-region-length)))))
                         do (when (getf cltpt:*debug* :convert)
                              (format t "match tree after:~%")
                              (cltpt/tree:tree-show match-root))
                         )))
               ;; adjust offset accordingly for next children/regions
               (incf offset change-in-region-length)
               (if reparse
                   (let* ((reparse-all (typep child 'document))
                          (ancestor
                            (if only-simple-changes
                                child
                                (or (nearest-parent-with-text child)
                                    child)))
                          (region-relative-to-ancestor
                            (if only-simple-changes
                                (make-region :begin 0
                                             :end (length new-child-text))
                                (relative-child-region ancestor child)))
                          (new-ancestor-text
                            (region-replace
                             region-relative-to-ancestor
                             (text-object-text ancestor)
                             new-child-text))
                          (rules
                            (remove-if-not
                             'identity
                             (loop
                               for type1
                                 in (text-format-text-object-types format)
                               collect (text-object-rule-from-subclass type1))))
                          (new-result
                            (if reparse-all
                                ;; TODO: this is bad because in this case we're just
                                ;; reparsing eveything..
                                (parse format new-ancestor-text :doc child)
                                (cltpt/combinator:scan-all-rules
                                 ;; this isnt good, the "context" we used before
                                 ;; may not be good for reparsing.
                                 nil
                                 new-ancestor-text
                                 ;; we want to use the rules for the text format
                                 ;; this is intended for, not the ones previously
                                 ;; used (so not the ones from the prev "context").
                                 rules
                                 ;; (cltpt/combinator:context-rules
                                 ;;  (getf (car prev-result) :ctx))
                                 (region-begin region-relative-to-ancestor)
                                 (region-end region-relative-to-ancestor))))
                          ;; the way we are using `new-objects' here as a plist
                          ;; and then turn it into a list isnt pretty. we should
                          ;; use another way of passing the list to the function
                          ;; and getting back the result that isnt hacky.
                          (new-objects))
                     (unless reparse-all
                       (loop for m in new-result
                             do (multiple-value-bind (new-obj updated-objects)
                                    (handle-match
                                     new-ancestor-text
                                     m
                                     new-objects
                                     (text-format-text-object-types format))
                                  (setf new-objects updated-objects)))
                       (setf new-objects
                             (nreverse
                              (remove-if
                               (lambda (item)
                                 (text-object-parent item))
                               new-objects)))
                       (setf new-elements
                             (concatenate 'list new-elements (nreverse new-objects)))
                       ;; we need to replace the old child with the new parse results.
                       (cond
                         (only-simple-changes
                          (setf (text-object-children child) new-objects))
                         ((and parent prev-sibling-cons)
                          (if new-objects
                              (progn
                                (setf (cdr prev-sibling-cons) new-objects)
                                (setf (cdr (last new-objects)) next-siblings))
                              (setf (cdr prev-sibling-cons) next-siblings)))
                         (parent
                          (setf (text-object-children (text-object-parent child))
                                (concatenate 'list
                                             new-objects
                                             next-siblings)))
                         (t
                          (setf (text-object-children child) new-objects)))
                       (when (or parent only-simple-changes)
                         (loop for new-child in new-objects
                               do (setf (text-object-parent new-child)
                                        (if only-simple-changes
                                            child
                                            parent))))))
                   ;; if we arent reparsing we need to adjust the regions of the
                   ;; children of the object accordingly.
                   ;; if we are meant to propagate the 'text-object-modify-region' function
                   ;; will take care of this.
                   (unless propagate
                     (loop for node in (text-object-children child)
                           for node-begin = (region-begin
                                             (relative-child-region obj node))
                           do (when (>= node-begin (region-begin region))
                                (region-incf (text-object-text-region node)
                                             change-in-region-length)))))
               ;; we need to finalize after changes.
               ;; or maybe we shouldnt?
               ;; (finalize-doc child)
               ))
    (nreverse new-elements)))

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
(defmethod handle-change ((obj text-object) (format text-format) change-pos new-str)
  (let* ((change-in-length (- (length new-str)
                              (length (text-object-text obj))))
         ;; this is a naive setting, we act as if a deletion or insertion
         ;; happened precisely at `change-pos'.
         ;; t means deletion, nil means insertion
         (is-deletion (> (length (text-object-text obj))
                         (length new-str)))
         (changed-region (if is-deletion
                             (make-region :begin (+ change-pos change-in-length)
                                          :end change-pos)
                             (make-region :begin change-pos :end change-pos))))
    (if is-deletion
        ;; if its a deletion the change is replacing the region with an empty string.
        (handle-changed-regions obj format (list (cons "" changed-region)) t)
        ;; if its an insertion we need to provide the inserted string, the region
        ;; of change is of size 0 so no text is replaced, only new text inserted.
        (handle-changed-regions
         obj
         format
         (list (cons (subseq new-str change-pos (+ change-pos change-in-length))
                     changed-region))
         t))))