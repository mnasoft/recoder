;;;; d-signal-defmethods.lisp

(in-package #:recoder)

(defmethod print-object ((x <d-signal>) stream)
  (format stream "~S ~S ~S" (d-signal-num x) (d-signal-id x) (d-signal-description x)))

