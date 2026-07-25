(in-package :cltpt/babel)

(defvar *python-interpreter*
  "python"
  "the name of the python interpreter to use")

(defvar *python-main-func-name*
  "cltpt_babel"
  "the name of the python interpreter to use")

(defun python-run-source (source)
  (uiop:with-temporary-file (:pathname p :keep t)
    (let ((temp-file (uiop:native-namestring p)))
      ;; write code file
      (with-open-file (f temp-file :direction :output :if-exists :supersede)
        (write-string source f))
      ;; run the interpreter on the file we wrote
      (let* ((process (uiop:launch-program (list *python-interpreter* temp-file)
                                           :output :stream
                                           :error-output :stream))
             (stdout (cltpt/reader:make-reader (uiop:process-info-output process)))
             (stderr (cltpt/reader:make-reader (uiop:process-info-error-output process))))
        (bt:make-thread
         (lambda ()
           ;; wait for process to finish and clean up temp file. its not ideal to run a thread
           ;; just for this
           (uiop:wait-process process)
           (uiop:delete-file-if-exists temp-file))
         :name "python-cleanup-worker")
        (values stdout stderr)))))

(defun python-wrap-source (code result-type)
  (format nil
          "def ~A():~%~A~%~%~A"
          *python-main-func-name*
          (cltpt/str-utils:ensure-min-indent code 4)
          (if (eq result-type :value)
              (format nil "print(repr(~A()), end=\"\")" *python-main-func-name*)
              (format nil "~A()" *python-main-func-name*))))

(defmethod babel-lexical-wrap ((lang (eql 'python)) code assignments)
  (reduce (lambda (assignment acc-code)
            (destructuring-bind (name . value) assignment
              (format nil "~A = ~A~%~A"
                      name
                      (babel-encode lang value)
                      acc-code)))
          assignments
          :from-end t
          :initial-value code))

(defmethod babel-eval ((lang (eql 'python)) code)
  (python-run-source (python-wrap-source code :output)))

(defmethod babel-eval* ((lang (eql 'python)) code assignments result-type &key &allow-other-keys)
  (python-run-source
   (python-wrap-source (babel-lexical-wrap lang code assignments) result-type)))