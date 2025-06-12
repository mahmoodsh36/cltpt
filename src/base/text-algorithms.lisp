(in-package :cltpt/base)

(defun modify-substring (str1 func main-region &optional small-region)
  "modify the string in SMALL-REGION of MAIN-REGION of string STR1. SMALL-REGION is relative
to MAIN-REGION."
  (let ((small-begin (if small-region
                         (+ (region-begin main-region) (region-begin small-region))
                         (region-begin main-region)))
        (small-end (if small-region
                       (+ (region-begin main-region) (region-end small-region))
                       (region-end main-region))))
    (concatenate 'string
                 (subseq str1 (region-begin main-region) small-begin)
                 (funcall func (subseq str1 small-begin small-end))
                 (subseq str1 small-end (region-end main-region)))))

(defun extract-modified-substring (str1 func main-region modification-region)
  "extract and modify the substring in MODIFICATION-REGION contained in MAIN-REGION of
string STR1. notice that MODIFICATION-REGION can be wider and even disjoint from MAIN-REGION."
  (let ((modification-begin (max (region-begin main-region)
                                 (region-begin modification-region)))
        (modification-end (min (region-end main-region)
                               (region-end modification-region))))
    (if (and (<= (region-begin modification-region)
                 (region-end main-region))
             (>= (region-end modification-region)
                 (region-begin main-region)))
        (concatenate 'string
                     (subseq str1 (region-begin main-region) modification-begin)
                     (funcall func (subseq str1 modification-begin modification-end))
                     (subseq str1 modification-end (region-end main-region)))
        str1)))

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
                          (replacement (cdr (assoc next-char replace-table :test #'char=))))
                     (princ (or replacement next-char) out)
                     (incf i))
                   ;; handle normal character
                   (let ((replacement (cdr (assoc ch replace-table :test #'char=))))
                     (princ (or replacement ch) out)))))))