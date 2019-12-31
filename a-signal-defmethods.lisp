;;;; a-signal-defmethods.lisp

(in-package #:recoder)

(annot:enable-annot-syntax)

(defmethod print-object ((x <a-signal>) stream)
  (format stream "~S ~S [~A ~A] ~S ~S" (a-signal-num x) (a-signal-id x) (a-signal-min x) (a-signal-max x) (a-signal-units x) (a-signal-description x)))

@export
(defmethod a-signal-value ((x <a-signal>) ushort-int)
  (+ (a-signal-min x)
     (* (- (a-signal-max x)
	   (a-signal-min x))
	(/ ushort-int *ushort-max*))))



