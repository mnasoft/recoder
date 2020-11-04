;;;; test.lisp

(in-package #:recoder)

(defparameter *trd-fname* (concatenate 'string
				       (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
  "Для примеров.")

(defparameter *trd* (make-instance 'recoder:<trd> :trd-file-name *trd-fname*))

(recoder:trd-open *trd*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *t-seq* (make-instance '<trd-seq> :trd-file-name *trd-fname*
                                                :signal-strings '("V2" "ET300"))
  "Для примеров.")

(trd-open *t-seq*)

(<trd-seq>-a-sig *t-seq*)
(<trd-seq>-d-sig *t-seq*)

(length *t-seq*)  ; => 15706 (14 bits, #x3D5A)

(find-if #'(lambda (el) (> el 800.0)) *t-seq* :key #'second)
(elt *t-seq* 15705)

(position-if #'(lambda (el) (> el 700.0)) *t-seq* :key #'second :from-end t :start 5000)
