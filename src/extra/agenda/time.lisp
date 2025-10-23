(defpackage :cltpt/agenda/time
  (:use :cl)
  (:export
   :time-range :make-time-range :time-range-begin :time-range-end))

(in-package :cltpt/agenda/time)

;; <2025-02-02 Sun 10:00:00 +1w>--<2025-02-02 Sun 14:00:00 +1w>
(defstruct time-range
  begin
  end)

(defgeneric is-between (time begin-time end-time)
  (:documentation "given 3 timestamps, check whether the first is between the other 2."))

(defmethod is-between ((time local-time:timestamp)
                       (begin local-time:timestamp)
                       (end local-time:timestamp))
  (and (local-time:timestamp>= time begin)
       (local-time:timestamp< time end)))

(defmethod is-between ((time time-range)
                       (begin local-time:timestamp)
                       (end local-time:timestamp))
  ;; (and (is-between (time-range-begin time) begin end)
  ;;      (is-between (time-range-end time) begin end))
  (is-between (time-range-begin time) begin end))

;; (defmethod is-between ((time repeated-timestamp)
;;                        (begin local-time:timestamp)
;;                        (end local-time:timestamp))
;;   (and (local-time:timestamp>= (repeated-timestamp-time time) begin)
;;        (local-time:timestamp< (repeated-timestamp-time time) end)))