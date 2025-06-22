(defpackage :recoder/trd/test
  (:use #:cl)
  (:nicknames "R/TRD/TEST")
  (:export *trd*
           *trd-fname*))

(in-package :recoder/trd)

(defparameter *trd-fname*
  (probe-file (mnas-path:asdf-path :recoder "data/trd/2018-11-06_092329.trd"))
   "Для примеров.")

(setf (trd-print-format) :long)
(setf (r/a-sig:a-signal-print-format) :long)
(defparameter *trd* (make-instance 'r/c:<trd> :file-name *trd-fname*))

(r/trd:trd-open *trd*)

(defparameter *trd1* (make-instance 'r/c:<trd>))

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



(r/g:signal-value *trd1* 2400
                    (append
                     (r/slist:a-signals  *trd1* '("Gv" "V2" "P02" "P03"))
                     (r/slist:d-signals  *trd1* '("NJ010" "NJ020" "GAS" "OIL"))
                     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *fn-xls*
    (probe-file
     (mnas-path:asdf-path :recoder "trd/1_Custom_sec_14April2025_10226_PM.xls")))

  (defparameter *fn-txt*
    (probe-file
     (fname-xls->txt *fn-xls*)))

  (defparameter *fn-trd*
    (fname-xls->trd *fn-xls*))

  (defparameter *trd* (make-instance '<trd> :file-name *fn-txt*))

  (r/g:read-obj *trd* *fn-txt*))


(length (r/g:a-signal-list *trd*))
(length (r/g:d-signal-list *trd*))

(r/bin:with-open-file-b-out (out *fn-trd*)
  (r/g:write-obj *trd* out))

(defparameter *trd1* (make-instance '<trd> :file-name *fn-trd*))

(r/bin:with-open-file-b-in (in *fn-trd*)
  (r/g:read-obj *trd1* in))

(defparameter *signals*
  (r/slist:a-signals *trd1* '("EB010" "ET020" "ET030")))

(r/get:signal-value *trd1* 2000 *signals*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(recode *fn-xls*)

*load-truename*

(uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))

(r:recode *fn-xls*)
