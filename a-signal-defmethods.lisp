;;;; a-signal-defmethods.lisp

(in-package #:recoder)

(defmethod print-object ((x a-signal) stream)
  (format stream "~S ~S [~A ~A] ~S ~S" (a-signal-num x) (a-signal-id x) (a-signal-min x) (a-signal-max x) (a-signal-units x) (a-signal-description x)))

(defmethod a-signal-value ((x a-signal) ushort-int)
  (+ (a-signal-min x)
     (* (- (a-signal-max x)
	   (a-signal-min x))
	(/ ushort-int *ushort-max*))))


(defmethod print-object ((x d-signal) stream)
  (format stream "~S ~S ~S" (d-signal-num x) (d-signal-id x) (d-signal-description x)))
