(defpackage :recoder/trd/test
  (:use #:cl)
  (:nicknames "R/TRD/TEST")
  (:export *trd*
           *trd-fname*))

(in-package :recoder/trd)

(defparameter *trd-fname*
  (concatenate 'string
	       (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
  "Для примеров.")

(defparameter *trd* (make-instance 'r/trd:<trd> :file-name *trd-fname*))

(r/trd:trd-open *trd*)




#+nil
(progn
  (let ((trd *trd*))
    (with-open-file (out "/home/mna/123321.bin" 
                         :element-type 'unsigned-byte
                         :direction :output
                         :if-exists :supersede)
      (r/g:write-obj trd  out)))

  )
(defparameter *trd1* (make-instance '<trd>))
(let ((trd *trd1*))
  (with-open-file (in *fnm* 
                      :element-type 'unsigned-byte
                      )
    (r/g:read-obj trd in)))

