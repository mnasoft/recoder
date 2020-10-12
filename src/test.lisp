;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd*
  (make-instance '<trd> :trd-file-name "~/quicklisp/local-projects/clisp/recoder/trd/2018-11-06_092329.trd"))

(trd-open *trd*)

(decode-universal-time (trd-utime-start *trd*))
(trd-utime-end *trd*)
