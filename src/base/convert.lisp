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
                            (getf result :escape t))))
         (to-reparse (if reparse-supplied
                         reparse
                         (unless result-is-string
                           (getf result :reparse))))
         ;; look for :escape-regions, fall back to :escape-region, and store as a list.
         (regions-to-escape (when (and to-escape (not result-is-string))
                              (or (getf result :escape-regions)
                                  (let ((region (getf result :escape-region)))
                                    (when region (list region)))
                                  ;; if :escape t but no regions specified, create a default region covering the whole text.
                                  (list (cltpt/buffer:make-region
                                         :begin 0
                                         :end (length (text-object-text text-obj)))))))
         (to-recurse (if recurse-supplied
                         recurse
                         (or (unless result-is-string
                               (getf result :recurse t))
                             to-reparse)))
         (escapables (collect-escapables text-object-types))
         (changes (unless result-is-string
                        (getf result :changes))))
    (when (getf cltpt:*debug* :convert)
      (format t "DEBUG: before incremental changes:~%")
      (cltpt/tree:tree-show text-obj))
    ;; set the child's own buffer where we will apply the changes. and then schedule the changes
    (if changes
      (cltpt/buffer:buffer-fetch-parent-text text-obj)
      (text-object-force-set-text text-obj result-text))
    (loop for change in changes
          do (cltpt/buffer:schedule-change* text-obj change :delegate nil))
    (let* ((parent-text (text-object-text text-obj))
           (child-regions (loop for child in (text-object-children text-obj)
                                collect (text-object-text-region child)))
           ;; compute regions for newlines that should be removed (not escaped) and schedule their removal
           (newline-removal-regions
             (loop for child in (text-object-children text-obj)
                   for child-options = (text-object-convert-options child fmt-dest)
                   for to-remove-newline = (getf child-options :remove-newline-after)
                   for child-end = (text-object-end child)
                   when (and to-remove-newline
                             (< child-end (length parent-text))
                             (char= (char parent-text child-end) #\newline))
                     collect (cltpt/buffer:make-region :begin child-end :end (1+ child-end))
                     and do (cltpt/buffer:schedule-change
                             text-obj child-end (1+ child-end) "" :delegate nil)))
           (raw-change-regions
             (loop for change in changes
                   for change-region = (cltpt/buffer:change-region change)
                   for change-args = (cltpt/buffer:change-args change)
                   ;; only exclude change regions that don't have :escape t in their args.
                   ;; changes with :escape t insert content that still needs escaping.
                   unless (getf change-args :escape)
                     collect change-region))
           ;; thinking of the requested escape regions as "holes" in the text-object, the final
           ;; escape regions would be holes "minus" inner-holes, where inner-holes are holes
           ;; within the holes that should not be escaped unlike their parent holes.
           (final-escape-regions
             (cltpt/buffer/region:region-complement-scoped
              (cltpt/buffer:make-region
               :begin 0
               :end (length parent-text))
              ;; holes
              regions-to-escape
              ;; inner-holes
              (concatenate 'list
                           raw-change-regions
                           child-regions
                           newline-removal-regions))))
      ;; schedule the final escape regions
      (when to-escape
        (cltpt/buffer:schedule-batch
         text-obj
         (mapcar
          (lambda (region)
            (labels ((my-text-escape (text)
                       (let ((escape-newlines (getf (cltpt/buffer/region:region-props region)
                                                    :escape-newlines
                                                    t)))
                         (escape-text
                          text
                          fmt-dest
                          escapables
                          escape-newlines))))
              (cltpt/buffer:make-change
               :region (cltpt/buffer:make-region
                        :begin (cltpt/buffer:region-begin region)
                        :end (cltpt/buffer:region-end region))
               :operator #'my-text-escape)))
          final-escape-regions)
         :new-level t
         :delegate nil)))
    (when (getf cltpt:*debug* :convert)
      (format t "DEBUG: after incremental changes:~%")
      (cltpt/tree:tree-show text-obj))
    ;; process children: convert each child and schedule its replacement.
    (when to-recurse
      (loop for child in (text-object-children text-obj)
            do (when (getf cltpt:*debug* :convert)
                 (format t "DEBUG: converting child ~A~%" child))
               (let ((child-result (convert-tree child fmt-src fmt-dest)))
                 (cltpt/buffer:schedule-change
                  text-obj
                  (text-object-begin child)
                  (text-object-end child)
                  child-result
                  :delegate nil))))
    ;; finally we apply all scheduled changes and return the result.
    ;; escape sequences may have already been registered by the root at document-convert, so
    ;; we dont want to apply changes if both to-escape and to-recurse are nil because that
    ;; might cause issues/conflicts.
    (when (or to-escape to-recurse)
      (cltpt/buffer:apply-scheduled-changes text-obj))
    (text-object-text text-obj)))

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
                       collect (cltpt/buffer:make-change
                                :operator (getf (cltpt/combinator/match::match-props escape) :replace)
                                :region (cltpt/buffer:make-region
                                         :begin (cltpt/combinator:match-begin-absolute escape)
                                         :end (cltpt/combinator:match-end-absolute escape))))))
          (loop for change in changes
                for str = (cltpt/buffer:change-operator change)
                for change-region = (cltpt/buffer:change-region change)
                for args = (cltpt/buffer:change-args change)
                do (apply 'cltpt/buffer:schedule-change
                          doc
                          (cltpt/buffer:region-begin change-region)
                          (cltpt/buffer:region-end change-region)
                          str
                          (cltpt/base:merge-plist args
                                                  (list :new-level nil
                                                        :delegate t)))))))
    ;; here we've bound :text-obj in *convert-info* so that
    ;; we can access it from within the template.
    (if template
        (convert-tree
         (parse
          *simple-format*
          template
          :text-object-types (list 'text-macro
                                   'post-lexer-text-macro))
         src-fmt
         dest-fmt
         :escape nil
         :recurse t
         :reparse nil)
        (convert-tree doc src-fmt dest-fmt))))

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