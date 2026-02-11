#!/usr/bin/env -S sbcl --script
(require 'asdf)
(require 'uiop)
(require 'sb-sprof)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t)

;; trying to run the benchmarks on a small number of files will give deceiving results. since
;; there is currently a significant overhead before parsing the first file. i think it comes from
;; text-object rule evaluations. i should probably take care of it but its good to note for now.

(defparameter *benchmark-iterations* 100)
(defparameter *benchmark-files* nil)
(defparameter *enable-profiling* nil)

(defun get-org-files ()
  (or *benchmark-files*
      (directory (merge-pathnames "tests/*.org" (asdf:system-source-directory :cltpt)))))

(defun org-rules ()
  (remove-if-not
   'identity
   (loop for type1 in (cltpt/base:text-format-text-object-types cltpt/org-mode:*org-mode*)
         collect (cltpt/base:text-object-rule-from-subclass type1))))

(defun run-benchmark (name iterations org-files parse-fn total-bytes-per-iteration)
  (flet ((do-benchmark ()
           (let ((start-time (get-internal-real-time)))
             (dotimes (i iterations)
               (when (zerop (mod (1+ i) 10))
                 (format t "  iteration ~D/~D~%" (1+ i) iterations))
               (dolist (file org-files)
                 (funcall parse-fn file)))
             (let* ((end-time (get-internal-real-time))
                    (elapsed-seconds (/ (- end-time start-time)
                                        internal-time-units-per-second))
                    (total-parses (* iterations (length org-files)))
                    (total-bytes (* iterations total-bytes-per-iteration))
                    (mb-per-second (/ total-bytes (* elapsed-seconds 1024 1024)))
                    (parses-per-second (/ total-parses elapsed-seconds)))
               (format t "~%=== ~A results ===~%" name)
               (format t "total time: ~,2F seconds~%" elapsed-seconds)
               (format t "total parses: ~D~%" total-parses)
               (format t "total data: ~,2F MB~%" (/ total-bytes 1024.0 1024.0))
               (format t "parses/second: ~,2F~%" parses-per-second)
               (format t "MB/second: ~,2F~%" mb-per-second)
               (format t "time per parse: ~,4F ms~%~%"
                       (* 1000 (/ elapsed-seconds total-parses)))))))
    (if *enable-profiling*
        (sb-sprof:with-profiling (:report :graph :loop nil)
          (do-benchmark))
        (do-benchmark))))

(let* ((iterations *benchmark-iterations*)
       (org-files (get-org-files))
       (num-files (length org-files))
       (rules (org-rules))
       (total-bytes-per-iteration
         (loop for file in org-files
               sum (with-open-file (s file) (file-length s))))
       ;; pre-read all files into memory once
       (org-file-contents
         (mapcar #'uiop:read-file-string org-files)))

  (format t "~%=== parsing benchmarks ===~%")
  (format t "files: ~D~%" num-files)
  (format t "total size: ~,2F KB~%" (/ total-bytes-per-iteration 1024.0))
  (format t "iterations: ~D~%" iterations)
  (format t "profiling: ~A~%~%" (if *enable-profiling* "enabled" "disabled"))

  ;; low-level combinator parsing (match tree)
  (format t "=== benchmark 1: cltpt/combinator:parse (low-level) ===~%")
  (run-benchmark "combinator"
                 iterations
                 org-file-contents
                 (lambda (content)
                   (cltpt/combinator:parse content rules))
                 total-bytes-per-iteration)

  ;; high-level base parsing (document tree)
  (format t "=== benchmark 2: cltpt/base:parse (high-level) ===~%")
  (run-benchmark "base"
                 iterations
                 org-file-contents
                 (lambda (content)
                   (cltpt/base:parse cltpt/org-mode:*org-mode* content))
                 total-bytes-per-iteration))