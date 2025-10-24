(defpackage :cltpt/agenda/state
  (:use :cl)
  (:export
   :state-name :state-is-terminal :make-state :state-by-name
   :state-desc-name :state-desc-is-terminal
   :make-state-desc :make-state-sequence-desc
   :state-sequence-desc :state-sequence-desc-state-descs :cycle))

(in-package :cltpt/agenda/state)

;; perhaps this is too fancy an abstraction over how org-mode actually handles
;; things. but perhaps we wont be restricting ourselves to org-mode functionality
;; so it would be a good thing.

(defclass state-sequence-desc* ()
  ((name
    :initarg :name
    :accessor state-sequence-desc-name
    :initform nil
    :documentation "the name of the sequence.")
   (state-descs
    :initarg :state-descs
    :accessor state-sequence-desc-state-descs
    :initform nil
    :documentation "a list of `state-desc*' instances describing the states of the sequence.")))

(defun make-state-sequence-desc (&key name state-descs)
  (make-instance 'state-sequence-desc*
                 :name name
                 :state-descs state-descs))

(defclass state-desc* ()
  ((name
    :initarg :name
    :accessor state-desc-name
    :initform nil
    :documentation "the name of the state.")
   (is-terminal
    :initarg :is-terminal
    :accessor state-desc-is-terminal
    :initform nil
    :documentation "whether the state is a terminal state (e.g. DONE).")))

(defun make-state-desc (&key name is-terminal)
  (make-instance 'state-desc*
                 :name name
                 :is-terminal is-terminal))

(defstruct state
  desc
  timestamp
  sequence-desc)

(defmethod state-is-terminal ((s state))
  (state-desc-is-terminal (state-desc s)))

(defmethod state-name ((s state))
  (state-desc-name (state-desc s)))

(defgeneric cycle (state)
  (:documentation "cycle to the next state."))

(defgeneric cycle-backward (state)
  (:documentation "cycle to the previous state."))

(defmethod cycle ((s state))
  (let* ((seq-desc (state-sequence-desc s))
         (state-cdr (cltpt/base:find-cons-if
                     (lambda (x)
                       (equal x s))
                     (state-sequence-desc-state-descs seq-desc)))
         (next-state-desc (cadr state-cdr))
         (next-state (make-state :desc next-state-desc
                                 :timestamp (local-time:now)
                                 :sequence-desc seq)))
    next-state))