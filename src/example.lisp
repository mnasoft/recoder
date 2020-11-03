;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter
    *trd-fname* (concatenate
		 'string
		 (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
    "Для примеров.")
(defparameter *trd* (make-instance 'recoder:<trd> :trd-file-name *trd-fname*))

(recoder:trd-open *trd*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <trd-001> (<trd> sequence)
  ((signal-strings :accessor signal-strings :initform nil :initarg :signal-strings)
   (a-sig :accessor <trd-001>-a-sig :initform nil)
   (d-sig :accessor <trd-001>-d-sig :initform nil)))

(defmethod sequence:length ((trd <trd-001>))
  (trd-total-records trd))

(defmethod sequence:elt ((trd <trd-001>) index)
  (let ((a-sig (<trd-001>-a-sig trd))
        (d-sig (<trd-001>-d-sig trd)))
    (append (when a-sig (trd-analog-by-rec-number  trd index a-sig))
            (when d-sig (trd-discret-by-rec-number trd index d-sig)))))

(defmethod trd-open :after ((trd <trd-001>)) 
  (let ((sig (trd-separate-signals trd (signal-strings trd))))
    (setf (<trd-001>-a-sig trd) (first  sig))
    (setf (<trd-001>-d-sig trd) (second sig))
    trd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *t-001* (make-instance '<trd-001> :trd-file-name *trd-fname*
                                                :signal-strings '("V2" "ET300")))

(trd-open *t-001*)
(<trd-001>-a-sig *t-001*)
(<trd-001>-d-sig *t-001*)

(length *t-001*)  ; => 15706 (14 bits, #x3D5A)

(length *t-001*)

(find-if
 #'(lambda (el) (> el 800.0))
 *t-001* :key #'second)

(position-if
 #'(lambda (el) (> el 700.0))
 *t-001* :key #'second :from-end t :start 5000)

 ; => 5097 (13 bits, #x13E9)

 
(elt *t-001* 15705)

