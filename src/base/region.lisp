(in-package :cltpt/base)

;; a `region' is used to pinpoint a substring without storing it, it does so
;; by storing the index at which the substring begin, and the number at
;; which it ends in the "parent" string.

(defstruct region
  (begin 0)
  (end -1))

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
  (make-region :begin (region-begin r)
               :end (region-end r)))

(defmethod region-compress ((r region) begin end)
  "place offsets BEGIN and END at the beginning/ending of the string respectively."
  (incf (region-begin r) begin)
  (decf (region-end r) end)
  r)

(defmethod region-compress-by ((r region) (offsets region))
  "invoke `region-compress' using begin/end from the region OFFSETS."
  (region-compress r
                   (region-begin offsets)
                   (region-end offsets)))

(defmethod region-replace ((r region) main-str new-str)
  "replace the substring bounded by the region R in MAIN-STR with NEW-STR."
  (cltpt/base:replace-substr main-str new-str (region-begin r) (region-end r)))

(defmethod region-intersection ((r1 region) (r2 region))
  "calculates the intersection of two regions.
returns a new region if they overlap, otherwise returns NIL."
  (let ((inter-begin (max (region-begin r1) (region-begin r2)))
        (inter-end (min (region-end r1) (region-end r2))))
    (when (< inter-begin inter-end)
      (make-region :begin inter-begin :end inter-end))))