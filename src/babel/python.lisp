(in-package :cltpt/babel)

(defvar *python-interpreter*
  "python"
  "the name of the python interpreter to use")

(defvar *chunk-size*
  1024
  "size of chunks to read from command outputs.")

(defvar *python-main-func-name*
  "cltpt_babel"
  "the name of the python interpreter to use")

(defmethod babel-eval ((lang (eql 'python)) code)
  (uiop:with-temporary-file (:pathname p :keep t)
    (let ((temp-file (uiop:native-namestring p)))
      ;; write code file
      (with-open-file (f temp-file :direction :output :if-exists :supersede)
        (format f "def ~A():~%~A~%~%~A()"
                *python-main-func-name*
                (cltpt/str-utils:ensure-min-indent code 4)
                *python-main-func-name*))
      ;; run the interpreter on the file we wrote
      (let* ((process (uiop:launch-program (list *python-interpreter* temp-file)
                                           :output :stream
                                           :error-output :stream))
             (stdout (cltpt/reader:make-reader (uiop:process-info-output process)))
             (stderr (cltpt/reader:make-reader (uiop:process-info-error-output process))))
        (bt:make-thread
         (lambda ()
           ;; wait for process to finish
           (uiop:wait-process process)
           ;; cleanup temp file
           (uiop:delete-file-if-exists temp-file))
         :name "python-cleanup-worker")
        (values stdout stderr)))))