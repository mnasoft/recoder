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

(defparameter *trd1* (make-instance '<trd>))
(let ((trd *trd1*))
  (with-open-file (in *trd-fname*
                      :element-type 'unsigned-byte)
    (r/g:read-obj trd in)))
(let ((trd *trd1*))
  (with-open-file (out "/home/mna/123321.bin" 
                       :element-type 'unsigned-byte
                       :direction :output
                       :if-exists :supersede)
    (r/g:write-obj trd  out)))

(defparameter *signals*  (r/slist:a-signals  *trd1* '("Gv" "V2" "P02" "P03")))


(defparameter *signals*  (r/slist:d-signals  *trd1* '("NJ010" "NJ020" "GAS" "OIL")))



(r/get:signal-value *trd1* 2400
                    (append
                     (r/slist:a-signals  *trd1* '("Gv" "V2" "P02" "P03"))
                     (r/slist:d-signals  *trd1* '("NJ010" "NJ020" "GAS" "OIL"))
                     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fn-xls*
   (probe-file
    (mnas-path:asdf-path :recoder "trd/1_Custom_sec_14April2025_10226_PM.xls")))

(defparameter *fn-txt*
  (probe-file
   (fname-xls->txt *fn-xls*)))

(defparameter *trd* (make-instance '<trd> :file-name *fn-txt*))

(r/g:read-obj *trd* *fn-txt*)

(utime-stream *fn-txt* *trd*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(file-path pathname)
