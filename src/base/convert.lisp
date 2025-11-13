(in-package :cltpt/base)

(defvar *convert-escape-newlines*
  t
  "whether to escape newlines on conversion. see `text-format-escape'.")

(defvar *convert-info*
  nil
  "conversion info that may be useful to pass to downstream functions during conversion.")

(defun replace-chars (s replace-table)
  "return a new string where every character in S that is a key in REPLACE-TABLE is
replaced by its associated string."
  (with-output-to-string (out)
    (loop for ch across s do
      (let ((replacement (cdr (assoc ch replace-table :test #'char=))))
        (if replacement
            (write-string replacement out)
            (write-char ch out))))))

(defun replace-chars-and-escapes (s replace-table &optional escapable-chars)
  "return a new string where chars in S are replaced via REPLACE-TABLE (alist).
handles escapes: '\\' followed by a char in ESCAPABLE-CHARS (list) drops
the '\\' and processes the char normally (replace or emit)."
  (with-output-to-string (out)
    (loop with len = (length s)
          for i from 0 below len
          do (let ((ch (aref s i)))
               (if (and escapable-chars
                        (char= ch #\\)
                        (< i (1- len))
                        (find (aref s (1+ i)) escapable-chars :test #'char=))
                   ;; handle escape
                   (let* ((next-char (aref s (1+ i)))
                          (replacement
                            (cdr (assoc next-char replace-table :test #'char=))))
                     (princ (or replacement next-char) out)
                     (incf i))
                   ;; handle normal character
                   (let ((replacement (cdr (assoc ch replace-table :test #'char=))))
                     (princ (or replacement ch) out)))))))

(defun escape-text (text backend escapable-chars to-escape-newlines)
  (text-format-escape backend text escapable-chars to-escape-newlines))

(defun collect-escapables (text-object-types)
  (remove-if-not
   'identity
   (loop for type1 in text-object-types
         collect (let ((rule (text-object-rule-from-subclass type1)))
                   (getf rule :escapable)))))

;; TODO: `to-recurse' isnt handled correctly. perhaps it should be used to
;; prevent iterating through children (but maybe not escaping? specified regions
;; should still be escaped).
(defmethod convert-tree ((text-obj text-object)
                         (fmt-src text-format)
                         (fmt-dest text-format)
                         &key
                           (reparse nil reparse-supplied)
                           (recurse nil recurse-supplied)
                           (escape nil escape-supplied)
                           (text-object-types
                            (text-format-text-object-types fmt-src)
                            text-object-types-supplied))
  (when (getf cltpt:*debug* :convert)
    (format t "DEBUG: converting object ~A~%" text-obj)
    (cltpt/tree:tree-show text-obj))
  (let* ((result (text-object-convert text-obj fmt-dest))
         (result-is-string (typep result 'string))
         (result-text
           (if result-is-string
               result
               (getf result :text)))
         (to-escape (if escape-supplied
                        escape
                        (or result-is-string
                            (getf result :escape))))
         (to-reparse (if reparse-supplied
                         reparse
                         (unless result-is-string
                           (getf result :reparse))))
         ;; look for :escape-regions, fall back to :escape-region, and store as a list.
         (regions-to-escape (when (and to-escape (not result-is-string))
                              (or (getf result :escape-regions)
                                  (let ((region (getf result :escape-region)))
                                    (when region (list region))))))
         (to-recurse (if recurse-supplied
                         recurse
                         (or (unless result-is-string (getf result :recurse))
                             to-reparse)))
         (escapables (collect-escapables text-object-types))
         (changes (or (unless result-is-string
                        (getf result :changes))
                      ;; if a reparse is requested but not changes were provided
                      ;; then we need to consider the whole text changed.
                      (and to-reparse
                           (list
                            (cons result-text
                                  (make-region
                                   :begin 0
                                   :end (region-length
                                         (text-object-text-region text-obj)))))))))
    (when (getf cltpt:*debug* :convert)
      (format t "DEBUG: before incremental changes:~%")
      (cltpt/tree:tree-show text-obj))
    ;; adjust 'regions-to-escape' to the changes provided in :changes
    (when regions-to-escape
      (setf
       regions-to-escape
       (if (and changes regions-to-escape)
           (let ((sorted-changes
                   (sort (copy-list changes)
                         #'<
                         :key (lambda (c)
                                (region-begin (cdr c))))))
             (loop for region-spec in regions-to-escape
                   collect
                   (let* ((region (if (consp region-spec) (car region-spec) region-spec))
                          (plist (if (consp region-spec) (cdr region-spec) nil))
                          (begin-offset 0)
                          (end-offset 0))
                     (loop for (str . change-region) in sorted-changes
                           do (when (< (region-begin change-region)
                                       (region-begin region))
                                (incf begin-offset
                                      (- (length str)
                                         (region-length change-region)))))
                     (loop for (str . change-region) in sorted-changes
                           do (when (< (region-begin change-region)
                                       (region-end region))
                                (incf end-offset
                                      (- (length str)
                                         (region-length change-region)))))
                     (let ((new-region (make-region
                                        :begin (+ (region-begin region) begin-offset)
                                        :end (+ (region-end region) end-offset))))
                       (if plist
                           (cons new-region plist)
                           new-region)))))
           regions-to-escape)))
    ;; this is tricky, we are modifying the text of these objects as we are
    ;; advancing, but we arent modifying the text at the root document.
    (if (or changes to-reparse)
        ;; we expect any changes to happen only to positions of children or changes
        ;; in their text and nothing else. because otherwise changes would be hard
        ;; or simply impossible to keep track of.
        (handle-changed-regions text-obj
                                fmt-src
                                changes
                                to-reparse
                                :propagate nil
                                :only-simple-changes t)
        (text-object-change-text text-obj
                                 result-text
                                 :propagate nil))
    (when (getf cltpt:*debug* :convert)
      (format t "DEBUG: after incremental changes:~%")
      (cltpt/tree:tree-show text-obj))
    ;; grab the new text after the changes were applied above.
    (setf result-text (text-object-text text-obj))
    (labels ((escape-text-in-regions (text containing-region escape-regions)
               ;; find intersections and sort them to process in order
               ;; TODO: dont sort here, make it a condition that
               ;; the regions are sorted before being passed.
               (let* ((effective-regions
                        (sort
                         (loop for region-spec in escape-regions
                               for r = (if (consp region-spec)
                                           (car region-spec)
                                           region-spec)
                               for plist = (when (consp region-spec)
                                             (cdr region-spec))
                               for intersection = (region-intersection
                                                   r
                                                   containing-region)
                               when intersection
                                 collect (cons intersection plist))
                         #'<
                         :key (lambda (item)
                                (region-begin (car item))))))
                 (if (not effective-regions)
                     ;; if no escape regions are in this section, just return the text.
                     (subseq text
                             (region-begin containing-region)
                             (region-end containing-region))
                     ;; otherwise, build the string with escaped parts.
                     (with-output-to-string (s)
                       (let ((last-end (region-begin containing-region)))
                         (dolist (region-spec effective-regions)
                           (let* ((r (car region-spec))
                                  (plist (cdr region-spec))
                                  (escape-newlines
                                    (getf plist
                                          :escape-newlines
                                          *convert-escape-newlines*)))
                             (write-string
                              (subseq text last-end (region-begin r))
                              s)
                             (when (getf cltpt:*debug* :convert)
                               (format t "DEBUG: escaping ~A~%"
                                       (subseq text
                                               (region-begin r)
                                               (region-end r))))
                             (write-string
                              (escape-text
                               (subseq text
                                       (region-begin r)
                                       (region-end r))
                               fmt-dest
                               escapables
                               escape-newlines)
                              s)
                             (setf last-end (region-end r))))
                         (write-string
                          (subseq text
                                  last-end
                                  (region-end containing-region))
                          s)))))))
      (if to-recurse
          ;; we store the results as fragments (`final-result-fragments') to avoid concatenating all the time
          (let* ((final-result-fragments)
                 (idx 0))
            ;; if we reparsed, we need to reset the parent of the new copies of "children"
            ;; (when to-reparse
            ;;   (setf (text-object-children text-obj) children)
            ;;   (dolist (child children)
            ;;     (setf (text-object-parent child) text-obj)
            ;;     ;; (setf (text-object-parent child) nil)
            ;;     ;; (text-object-set-parent child text-obj)
            ;;     ;; (text-object-adjust-to-parent child text-obj)
            ;;     ))
            (loop for child in (text-object-children text-obj)
                  do (when (getf cltpt:*debug* :convert)
                       (format t "DEBUG: converting child ~A~%" child))
                     (let* ((child-result (convert-tree child fmt-src fmt-dest))
                            (child-options
                              (text-object-convert-options child
                                                           fmt-dest))
                            (to-remove-newline-after
                              (getf child-options :remove-newline-after))
                            (text-in-between-begin idx)
                            (text-in-between-end (text-object-begin child))
                            ;; text up to next child.
                            ;; escape only the region that is requested for escaping in
                            ;; text-in-between.
                            (text-in-between
                              (if to-escape
                                  (if regions-to-escape
                                      (escape-text-in-regions
                                       result-text
                                       (make-region :begin text-in-between-begin
                                                    :end text-in-between-end)
                                       regions-to-escape)
                                      (escape-text
                                       (subseq result-text
                                               text-in-between-begin
                                               text-in-between-end)
                                       fmt-dest
                                       escapables
                                       *convert-escape-newlines*))
                                  (subseq result-text
                                          text-in-between-begin
                                          text-in-between-end))))
                       ;; if we reparsed only a specific region, we need to offset the regions of children
                       (push text-in-between final-result-fragments)
                       (push child-result final-result-fragments)
                       (setf idx (text-object-end child))
                       ;; if requested, ignore all newlines after the object (if any)
                       (when to-remove-newline-after
                         (when (and (< idx (length result-text))
                                    (equal (char result-text idx) #\newline))
                           (incf idx)))))
            ;; we need to handle regions-to-escape properly on the remaining text after
            ;; the region of the last child
            (let ((final-text-in-between
                    (if to-escape
                        (if regions-to-escape
                            (escape-text-in-regions
                             result-text
                             (make-region :begin idx
                                          :end (length result-text))
                             regions-to-escape)
                            (escape-text (subseq result-text idx)
                                         fmt-dest
                                         escapables
                                         *convert-escape-newlines*))
                        (subseq result-text idx))))
              (push final-text-in-between final-result-fragments)
              (apply 'concatenate 'string (nreverse final-result-fragments))))
          (if to-escape
              (if regions-to-escape
                  (escape-text-in-regions
                   result-text
                   (make-region :begin 0
                                :end (length result-text))
                   regions-to-escape)
                  (escape-text result-text
                               fmt-dest
                               escapables
                               *convert-escape-newlines*))
              result-text)))))

;; this barely handles simple patterns, it cannot be relied on, but it may make some things easier.
(defun convert-rule-with-shared-patterns (tree)
  (let ((mycar (car tree)))
    (if (equal mycar 'consec)
        (concatenate
         'string
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

(defmethod convert-document ((src-fmt text-format)
                             (dest-fmt text-format)
                             (doc document))
  "convert a document from one text format to another.

this grabs the template defined for the destination format using `text-format-conversion-template'
before calling `convert-tree' on the given DOC."
  (let ((*convert-info*
          (merge-plist *convert-info*
                       (list :text-obj doc
                             :dest-fmt dest-fmt
                             :src-fmt src-fmt)))
        (template (text-format-conversion-template dest-fmt)))
    ;; process escape sequences by applying them as incremental changes to the document
    (let* ((escapes (text-document-escapes doc)))
      (when escapes
        (let* ((changes
                 (loop for escape in escapes
                       collect (cons (getf (cltpt/combinator/match::match-props escape) :replace)
                                     (make-region :begin (cltpt/combinator:match-begin escape)
                                                  :end (cltpt/combinator:match-end escape))))))
          ;; sort changes by region begin position to ensure correct offset calculations in handle-changed-regions
          ;; (setf changes (sort changes #'< :key (lambda (x) (region-begin (cdr x)))))
          (handle-changed-regions doc
                                  src-fmt
                                  changes
                                  nil
                                  :only-simple-changes t
                                  :propagate t))))
    ;; here we've bound :text-obj in *convert-info* so that
    ;; we can access it from within the template.
    (if template
        (convert-tree
         (parse
          *simple-format*
          template
          :text-object-types (list 'cltpt/base:text-macro
                                   'cltpt/base:post-lexer-text-macro))
         src-fmt
         dest-fmt
         :escape nil
         :recurse t
         :reparse nil)
        (convert-tree new-doc src-fmt dest-fmt))))

(defun convert-simple-format (format-str)
  "this function can be used for \"formatting strings\" with lisp code."
  (let* ((result (convert-tree
                  (parse *simple-format* format-str)
                  *simple-format*
                  *simple-format*)))
    result))

(defun filepath-format (filepath format-str &optional additional-data)
  "convert FORMAT-STR for FILEPATH, add ADDITIONAL-DATA to `cl-user::*file-info*'."
  (let* ((cl-user::*file-info*
           (merge-plist
            (list
             :file filepath
             :file-no-ext (cltpt/file-utils:path-without-extension filepath)
             :filename-no-ext (cltpt/file-utils:base-name-no-ext filepath)
             :filename (cltpt/file-utils:file-basename filepath))
            additional-data))
         (result (convert-simple-format format-str)))
    result))