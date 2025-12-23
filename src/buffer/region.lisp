(defpackage :cltpt/buffer/region
  (:use :cl)
  (:export
   :region
   :make-region
   :region-begin
   :region-end
   :region-props
   :region-text
   :region-length
   :region-decf
   :region-incf
   :region-contains
   :region-encloses
   :region-encloses-strict
   :region-clone
   :region-compress
   :region-compress-by
   :region-replace
   :region-intersection
   :merge-regions
   :region-complement-list
   :region-complement-scoped
   :region-props))

(in-package :cltpt/buffer/region)

(defstruct region
  (begin 0)
  (end -1)
  props)

(defmethod region-decf ((r region) num)
  (decf (region-begin r) num)
  (decf (region-end r) num)
  r)

(defmethod region-incf ((r region) num)
  (incf (region-begin r) num)
  (incf (region-end r) num)
  r)

(defmethod region-text ((r region) str1)
  (subseq str1 (region-begin r) (region-end r)))

(defmethod region-contains ((r region) pos)
  (and (< pos (region-end r))
       (>= pos (region-begin r))))

(defmethod region-encloses ((r region) (r2 region))
  "returns whether region R encloses region R2 (non-strict)."
  (and (>= (region-begin r2) (region-begin r))
       (<= (region-end r2) (region-end r))))

(defmethod region-encloses-strict ((r region) (r2 region))
  "returns whether region R strictly encloses region R2."
  (and (> (region-begin r2) (region-begin r))
       (< (region-end r2) (region-end r))))

(defmethod region-length ((r region))
  (with-slots (begin end) r
    (- end begin)))

(defmethod region-clone ((r region))
  "creates a copy of the given region."
  (make-region :begin (region-begin r)
               :end (region-end r)
               :props (region-props r)))

(defmethod region-compress ((r region) begin end)
  "adjusts the region by the given offsets."
  (incf (region-begin r) begin)
  (decf (region-end r) end)
  r)

(defmethod region-compress-by ((r region) (offsets region))
  "invoke `region-compress' using begin/end from the region OFFSETS."
  (region-compress r
                   (region-begin offsets)
                   (region-end offsets)))

(defmethod region-replace ((r region) main-str new-str)
  "replace the substring bounded by region R in MAIN-STR with NEW-STR."
  (cltpt/str-utils:replace-substr main-str new-str (region-begin r) (region-end r)))

(defmethod region-intersection ((r1 region) (r2 region))
  "calculates the intersection of two regions."
  (let ((inter-begin (max (region-begin r1) (region-begin r2)))
        (inter-end (min (region-end r1) (region-end r2))))
    (when (< inter-begin inter-end)
      (make-region :begin inter-begin
                   :end inter-end
                   :props (region-props r2)))))

(defun merge-regions (regions)
  "returns a new list of regions where overlapping or adjacent regions with identical properties are merged into single continuous regions."
  (let ((sorted (sort (mapcar #'region-clone regions) '< :key 'region-begin)))
    (when sorted
      (let ((merged (list (car sorted))))
        (dolist (next (cdr sorted))
          (let ((current (car merged)))
            (if (and (<= (region-begin next) (region-end current))
                     (equal (region-props next) (region-props current)))
                (setf (region-end current) (max (region-end current) (region-end next)))
                (push next merged))))
        (nreverse merged)))))

(defun subtract-one-region (source hole)
  "subtracts a single 'hole' region from a 'source' region.
returns a list of 0, 1, or 2 resulting fragments which inherit SOURCE's props."
  (let ((s-beg (region-begin source))
        (s-end (region-end source))
        (h-beg (region-begin hole))
        (h-end (region-end hole))
        (p (region-props source)))
    (cond
      ;; no overlap at all
      ((or (>= h-beg s-end) (<= h-end s-beg))
       (list (region-clone source)))
      ;; hole completely covers source
      ((and (<= h-beg s-beg) (>= h-end s-end))
       nil)
      ;; partial overlap / splitting
      (t
       (let ((results))
         (when (> h-beg s-beg)
           (push (make-region :begin s-beg :end h-beg :props p) results))
         (when (< h-end s-end)
           (push (make-region :begin h-end :end s-end :props p) results))
         (nreverse results))))))

(defun region-complement-list (n-regions m-regions)
  "returns fragments of N-REGIONS that are not covered by any regions in M-REGIONS.
fragments inherit properties from their originating N regions."
  (let ((source-list (merge-regions n-regions))
        (hole-list (merge-regions m-regions)))
    (dolist (hole hole-list)
      (setf source-list
            (loop for source in source-list
                  append (subtract-one-region source hole))))
    source-list))

(defun region-complement-scoped (master n-regions m-regions)
  "calculates the complement of M within N, restricted by the MASTER region.

- clips N regions to MASTER (inheriting N's props).
- merges overlapping N regions with identical props.
- subtracts M regions (clipped to MASTER) from the N list."
  (let* ((clipped-n (loop for n in n-regions
                          for inter = (region-intersection master n)
                          when inter collect inter))
         (source-list (merge-regions clipped-n))
         (clipped-m (loop for m in m-regions
                          for inter = (region-intersection master m)
                          when inter collect inter))
         (hole-list (merge-regions clipped-m)))
    (dolist (hole hole-list)
      (setf source-list
            (loop for source in source-list
                  append (subtract-one-region source hole))))
    source-list))