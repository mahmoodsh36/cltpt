(defpackage :cltpt/tests/region
  (:use :cl :it.bese.fiveam :cltpt/buffer :cltpt/buffer/region)
  (:export :run-region-tests))

(in-package :cltpt/tests/region)

(def-suite region-suite
  :description "tests for cltpt/buffer region functions."
  :in cltpt/tests::cltpt-suite)

(in-suite region-suite)

(defun make-regs (list)
  (mapcar (lambda (item)
            (make-region :begin (first item)
                         :end (second item)
                         :props (third item)))
          list))

(defun regs-to-list (regs)
  (mapcar (lambda (r)
            (list (region-begin r) (region-end r) (region-props r)))
          regs))

(test test-region-complement-basic
  (let* ((master (make-region :begin 0 :end 100))
         (n (make-regs '((0 100 :main))))
         (m (make-regs '((40 60))))
         (result (region-complement-scoped master n m))
         (result-list (regs-to-list result)))
    (is (equal result-list '((0 40 :main) (60 100 :main))))))

(test test-region-complement-property-seg
  (let* ((master (make-region :begin 0 :end 100))
         (n (make-regs '((0 50 :red) (50 100 :blue))))
         (m (make-regs '((40 60))))
         (result (region-complement-scoped master n m))
         (result-list (regs-to-list result)))
    (is (equal result-list '((0 40 :red) (60 100 :blue))))))

(test test-region-merge-adjacent-same-prop
  (let* ((master (make-region :begin 0 :end 100))
         (n (make-regs '((0 20 :bold) (20 40 :bold))))
         (m (make-regs nil))
         (result (region-complement-scoped master n m))
         (result-list (regs-to-list result)))
    (is (equal result-list '((0 40 :bold))))))

(test test-region-keep-adjacent-diff-prop
  (let* ((master (make-region :begin 0 :end 100))
         (n (make-regs '((0 20 :bold) (20 40 :italic))))
         (m (make-regs nil))
         (result (region-complement-scoped master n m))
         (result-list (regs-to-list result)))
    (is (equal result-list '((0 20 :bold) (20 40 :italic))))))

(test test-region-master-clip-bounds
  (let* ((master (make-region :begin 10 :end 90))
         (n (make-regs '((0 100 :meta))))
         (m (make-regs '((0 20))))
         (result (region-complement-scoped master n m))
         (result-list (regs-to-list result)))
    (is (equal result-list '((20 90 :meta))))))

(defun run-region-tests ()
  (run! 'region-suite))