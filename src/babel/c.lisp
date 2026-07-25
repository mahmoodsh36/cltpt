(in-package :cltpt/babel)

(defvar *c-compiler*
  "cc"
  "the C compiler to invoke")

(defvar *c-default-includes*
  '("stdio.h" "stdlib.h" "string.h")
  "headers included by default when wrapping C code in a main function.")

(defun c-run-source (source)
  "compile SOURCE with `*c-compiler*' and run the resulting executable, returning (stdout stderr)
as readers. if compilation fails, stdout is empty and stderr carries the compiler diagnostics."
  (uiop:with-temporary-file (:pathname src-path :type "c" :keep t)
    (let* ((src-file (uiop:native-namestring src-path))
           (exe-file (concatenate 'string src-file ".out")))
      (with-open-file (f src-file :direction :output :if-exists :supersede)
        (write-string source f))
      (multiple-value-bind (compile-out compile-err compile-code)
          (uiop:run-program (list *c-compiler* src-file "-o" exe-file)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (if (zerop compile-code)
            ;; run the compiled executable
            (let* ((process (uiop:launch-program (list exe-file)
                                                 :output :stream
                                                 :error-output :stream))
                   (stdout (cltpt/reader:make-reader (uiop:process-info-output process)))
                   (stderr (cltpt/reader:make-reader (uiop:process-info-error-output process))))
              (bt:make-thread
               (lambda ()
                 ;; wait for the process to finish and clean up the temp files. as in python.lisp,
                 ;; spawning a thread just for this is not ideal.
                 (uiop:wait-process process)
                 (uiop:delete-file-if-exists src-file)
                 (uiop:delete-file-if-exists exe-file))
               :name "c-cleanup-worker")
              (values stdout stderr))
            ;; compilation failed: surface the compiler's error output
            (progn
              (uiop:delete-file-if-exists src-file)
              (uiop:delete-file-if-exists exe-file)
              (values (cltpt/reader:reader-from-string "")
                      (cltpt/reader:reader-from-string compile-err))))))))

(defun c-wrap-source (code)
  "wrap CODE in a `main' function preceded by the default includes."
  (format nil
          "~{#include <~A>~%~}~%int main(void) {~%~A~%    return 0;~%}~%"
          *c-default-includes*
          (cltpt/str-utils:ensure-min-indent code 4)))

(defun c-type-for-value (value)
  "the C type used to declare a scalar variable holding VALUE, for `:var' bindings."
  (cond ((integerp value) "int")
        ((numberp value) "float")
        ((stringp value) "const char*")))

(defun c-array-element-type (list)
  "the C element type used for an array declaration holding LIST."
  (cond ((some #'stringp list) "const char*")
        ((every #'integerp list) "int")
        ((every #'numberp list) "float")))

(defun c-declare-var (name value)
  "the C declaration binding NAME to VALUE.
- a scalar becomes a single typed variable
- a list becomes a local array plus a `<name>_len' companion (`sizeof' also works, `<name>_len' is
  provided for convenience). only flat lists of numbers/strings are supported."
  (cond
    ((and (listp value) value)
     (let* ((element-type (c-array-element-type value))
            ;; a list containing a string is declared `const char*', and the numeric elements are
            ;; coerced to strings.
            (value (if (and (equal element-type "const char*")
                            (notevery #'stringp value))
                       (mapcar (lambda (elem)
                                 (if (stringp elem)
                                     elem
                                     (princ-to-string elem)))
                               value)
                       value)))
       (format nil
               "~A ~A[] = ~A;~%const int ~A_len = ~A;"
               element-type
               name
               (babel-encode 'c value)
               name
               (length value))))
    ((null value)
     (format nil "int *~A = NULL;~%const int ~A_len = 0;" name name))
    (t (format nil "~A ~A = ~A;" (c-type-for-value value) name (babel-encode 'c value)))))

(defmethod babel-lexical-wrap ((lang (eql 'c)) code assignments)
  (reduce (lambda (assignment acc-code)
            (destructuring-bind (name . value) assignment
              (format nil "~A~%~A" (c-declare-var name value) acc-code)))
          assignments
          :from-end t
          :initial-value code))

(defmethod babel-eval ((lang (eql 'c)) code)
  (c-run-source (c-wrap-source code)))

(defmethod babel-eval* ((lang (eql 'c)) code assignments result-type &key (main t))
  (c-run-source
   (if main
       (c-wrap-source (babel-lexical-wrap lang code assignments))
       (babel-lexical-wrap lang code assignments))))

(defvar *c-string-rule*
  '(:pattern (cltpt/combinator:consec "\"" (cltpt/combinator:all-but "\"") "\"")
    :get list))

(defvar *c-list-rule*
  '(cltpt/combinator:any
    (:pattern "{}" :value nil)
    (:pattern
     (cltpt/combinator:consec
      "{"
      (cltpt/combinator:separated-atleast-one
       (cltpt/combinator:any ", " ",")
       *c-value-rule*)
      "}")
     :get list)))

(defvar *c-value-rule*
  '(cltpt/combinator:any
    *c-list-rule*
    *c-string-rule*
    (cltpt/combinator:number-matcher)))

(defmethod babel-value-rule ((lang (eql 'c)))
  '*c-value-rule*)