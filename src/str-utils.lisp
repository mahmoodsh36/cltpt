(defpackage :cltpt/str-utils
  (:use :cl)
  (:export
   :md5-str :str-join :str-prune :str-split :str-dupe
   :replace-all
   :unindent :ensure-min-indent))

(in-package :cltpt/str-utils)

(defun md5-str (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (sb-ext:string-to-octets s :external-format :utf-8))))

(defun str-join (string-list separator)
  "joins a list of strings with a SEPARATOR in between each element."
  (with-output-to-string (stream)
    (loop for (str . rest) on string-list
          do (write-string str stream)
          when rest do (write-string separator stream))))

(defun str-prune (string max-length &optional (omission "..."))
  "truncates a string to a maximum length, appending an omission string.

args:
  string: the input string to prune.
  max-length: the maximum desired length of the resulting string.
  omission: the string to append if truncation occurs (defaults to \"...\").

returns the pruned string, or the original string if it's short enough."
  (let ((string-length (length string))
        (omission-length (length omission)))
    ;; if the original string is already short enough, return it as is.
    (if (<= string-length max-length)
        string
        ;; check if the max-length is even long enough for the omission.
        ;; if not, truncate the omission string itself to fit.
        (if (< max-length omission-length)
            (subseq omission 0 max-length)
            ;; otherwise, concatenate the start of the string with the omission.
            (concatenate 'string
                         (subseq string 0 (- max-length omission-length))
                         omission)))))

(defun str-split (str sep)
  "split STR by the seperator SEP. simply calls `uiop:split-string'."
  (uiop:split-string str :separator sep))

(defun str-dupe (str count)
  "return STR concatenated COUNT times."
  (with-output-to-string (out)
    (loop for i from 0 upto count
          do (write-sequence str out))))

(defun replace-all (string part replacement &key (test #'string=))
  "returns a new string in which all occurrences of PART are replaced with REPLACEMENT."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun ensure-min-indent (input-string &optional (min-indent 4))
  "ensures every line has at least MIN-INDENT spaces at the start."
  (with-output-to-string (out)
    (with-input-from-string (in input-string)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((existing-spaces
                        ;; count leading spaces
                        (or (position-if-not (lambda (c) (char= c #\space)) line)
                            (length line)))
                      (needed-spaces
                        (max 0 (- min-indent existing-spaces))))
                 ;; add padding if existing < min-indent
                 (dotimes (i needed-spaces)
                   (write-char #\space out))
                 ;; write the line (includes its original spaces)
                 (write-string line out)
                 (terpri out))))))

(defun unindent (input-string)
  "calculates indentation of the first line and removes that amount from all lines."
  (with-output-to-string (out)
    (with-input-from-string (in input-string)
      (let ((first-line (read-line in nil nil)))
        (when first-line
          (let ((indent-amount
                  ;; calculate the indentation level of the first line
                  (or (position-if-not (lambda (c) (char= c #\space)) first-line)
                      (length first-line))))
            (flet ((print-trimmed (line)
                     (let* ((len (length line))
                            ;; count actual spaces in this specific line
                            (current-indent (or (position-if-not (lambda (c) (char= c #\space)) line)
                                                len))
                            ;; determine how many chars to actually cut.
                            ;; we cut the requested amount, unless the line has fewer spaces
                            ;; than that.
                            (cut-point (min indent-amount current-indent)))
                       (write-string (subseq line cut-point) out)
                       (terpri out))))
              (print-trimmed first-line)
              (loop for line = (read-line in nil nil)
                    while line
                    do (print-trimmed line)))))))))