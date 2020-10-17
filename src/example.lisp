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
