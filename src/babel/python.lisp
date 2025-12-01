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
  (let ((output-stream *standard-output*))
    (bt:make-thread
     (lambda ()
       (uiop:with-temporary-file (:pathname p)
         (let ((temp-file (uiop:native-namestring p)))
           ;; write code file
           (with-open-file (f temp-file :direction :output :if-exists :supersede)
             (format f "def ~A():~%~A~%~%~A()"
                     *python-main-func-name*
                     code
                     (ensure-min-indent code 4)
                     *python-main-func-name*))
           ;; run the interpreter on the file we wrote
           (let* ((process (uiop:launch-program (list *python-interpreter* temp-file)
                                                :output :stream
                                                :error-output :stream))
                  (stdout (uiop:process-info-output process))
                  (stderr (uiop:process-info-error-output process))
                  (out-buf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
                  (err-buf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
             (unwind-protect
                  (flet ((process-stream (stream buffer prefix)
                           ;; read all currently available chars
                           (loop for char = (read-char-no-hang stream nil :eof)
                                 until (or (null char) (eq char :eof))
                                 do (if (char= char #\newline)
                                        (progn
                                          ;; flush buffer as a line
                                          (format output-stream "~A: ~A~%" prefix buffer)
                                          (force-output output-stream)
                                          (setf (fill-pointer buffer) 0))
                                        ;; else add to buffer
                                        (vector-push-extend char buffer)))
                           ;; return T if stream is closed (:eof)
                           (eq (peek-char nil stream nil :eof) :eof)))
                    ;; this is the polling loop, we keep "polling" for new characters and sleep
                    ;; if we havent received any.
                    (loop
                      (process-stream stdout out-buf "STDOUT")
                      (process-stream stderr err-buf "STDERR")
                      ;; if process is dead, we break the loop
                      when (not (uiop:process-alive-p process)) return nil
                      ;; sleep briefly to prevent excessive cpu usage
                      do (sleep 0.05))
                    ;; if the process died, we process the remainders
                    (process-stream stdout out-buf "STDOUT")
                    (process-stream stderr err-buf "STDERR"))
               ;; cleanup
               (close stdout)
               (close stderr)
               (uiop:terminate-process process))))))
     :name "python-worker")))