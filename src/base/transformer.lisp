(defpackage :cltpt/transformer
  (:use :cl)
  (:export #:transform-string-via-rule
           #:reconstruct-string-from-rule))

(in-package :cltpt/transformer)

(defun find-node-by-id (parse-node target-id)
  (when parse-node
    (let ((parent-info (car parse-node))
          (children (cdr parse-node)))
      (if (string= (getf parent-info :id) target-id)
          parse-node
          (loop for child in children thereis (find-node-by-id child target-id))))))

(defun generate-new-text-segment (context-node rule-for-new-text)
  (let ((rule-type (first rule-for-new-text)))
    (cond
      ((eq rule-type 'cltpt/combinator::literal)
       (second rule-for-new-text))
      ((eq rule-type 'cltpt/combinator::pair)
       (concatenate 'string
                    (generate-new-text-segment context-node
                                               (second rule-for-new-text))
                    (generate-new-text-segment context-node
                                               (third rule-for-new-text))
                    (generate-new-text-segment context-node
                                               (fourth rule-for-new-text))))
      ((eq rule-type 'cltpt/combinator::consec)
       (apply #'concatenate 'string
              (loop for sub-rule in (rest rule-for-new-text)
                    collect (generate-new-text-segment context-node sub-rule))))
      (t (error "unsupported rule type for text transformation: ~S" rule-type)))))

(defun generate-text-from-patterns (patterns original-parse-result)
  (apply #'concatenate 'string
         (loop for pattern-rule in patterns
               collect (let* ((target-sub-rule (second pattern-rule))
                              (id-value (fourth pattern-rule))
                              (context-node (find-node-by-id original-parse-result
                                                             id-value)))
                         (generate-new-text-segment context-node target-sub-rule)))))

(defun find-anchor-for-patterns (patterns original-parse-result)
  (loop for pattern-rule in patterns
        for id-value = (fourth pattern-rule)
        for anchor-node = (find-node-by-id original-parse-result id-value)
        when anchor-node do (return anchor-node)))

(defun collect-replacements (rule original-parse-result)
  (when (and rule (listp rule))
    (let ((op (first rule)))
      (cond
        ((and (member op '(cltpt/combinator::pair cltpt/combinator::consec))
              (not
               (every
                (lambda (child)
                  (and (listp child)
                       (eq (first child) :pattern)))
                (rest rule))))
         (loop for child-rule in (rest rule)
               appending (collect-replacements child-rule original-parse-result)))
        ((and (member op '(cltpt/combinator::pair cltpt/combinator::consec))
              (every
               (lambda (child)
                 (and (listp child)
                      (eq (first child) :pattern)))
               (rest rule)))
         (let* ((patterns (rest rule))
                (anchor-node (find-anchor-for-patterns patterns
                                                       original-parse-result)))
           (when anchor-node
             (list (list (getf (car anchor-node) :begin)
                         (getf (car anchor-node) :end)
                         (generate-text-from-patterns patterns
                                                      original-parse-result))))))
        ((eq op :pattern)
         (let* ((target-sub-rule (second rule))
                (id-value (fourth rule))
                (context-node (find-node-by-id original-parse-result id-value)))
           (when context-node
             (list (list (getf (car context-node) :begin)
                         (getf (car context-node) :end)
                         (generate-new-text-segment context-node
                                                    target-sub-rule))))))
        (t (warn "ignoring rule with unknown operator: ~S" rule) nil)))))

(defun apply-replacements (original-string replacements)
  (let ((sorted-replacements (sort (copy-list replacements) #'< :key #'first))
        (result-parts)
        (current-pos 0)
        (original-string-length (length original-string)))
    (dolist (rep sorted-replacements)
      (let ((begin (first rep))
            (end (second rep))
            (new-text (third rep)))
        (when (> begin current-pos)
          (push (subseq original-string current-pos begin) result-parts))
        (push new-text result-parts)
        (setf current-pos end)))
    (when (< current-pos original-string-length)
      (push (subseq original-string current-pos) result-parts))
    (apply #'concatenate 'string (nreverse result-parts))))

(defun transform-string-via-rule (original-string original-parse-result transformation-rule)
  (let ((replacements (collect-replacements transformation-rule
                                            original-parse-result)))
    (apply-replacements original-string replacements)))

(defun reconstruct-string-from-rule (rule parsed-data-source)
  "constructs a new string from a rule, using a parse tree as a data source."
  (cond
    ((stringp rule)
     rule)
    ;; rule is a :pattern
    ((and (listp rule) (getf rule :pattern))
     (let* ((inner-rule (getf rule :pattern))
            (id (getf rule :id)))
       (if (and (listp inner-rule)
                (member (first inner-rule)
                        '(cltpt/combinator:consec
                          cltpt/combinator:pair
                          cltpt/combinator:literal)))
           ;; if its a compound rule we need to recurse
           (reconstruct-string-from-rule inner-rule parsed-data-source)
           ;; else, inner-rule is a simple type descriptor (e.g. symbol-matcher)
           ;; and id is the ID to find in parsed-data-source.
           (let ((found-node (find-node-by-id parsed-data-source id)))
             (if found-node
                 (cltpt/combinator:match-text (car found-node))
                 (progn
                   (warn "id '~S' not found in parse data for reconstruction."
                         id)
                   ""))))))
    ;; rule is a literal combinator (e.g. '(cltpt/combinator:literal "text"))
    ((and (listp rule)
          (member (first rule)
                  '(cltpt/combinator:literal)))
     (second rule))
    ;; recursive step for consec/pair
    ((and (listp rule)
          (member (first rule)
                  '(cltpt/combinator:consec
                    cltpt/combinator:pair)))
     (apply #'concatenate 'string
            (loop for sub-rule in (rest rule)
                  collect (reconstruct-string-from-rule sub-rule
                                                        parsed-data-source))))
    (t
     (warn "ignoring unknown rule component in reconstruction: ~S" rule)
     "")))