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

(defparameter *fn*
  (probe-file
   "D:/home/_namatv/PRG/msys64/home/namatv/quicklisp/local-projects/clisp/recoder/trd/1_Custom_sec_14April2025_10226_PM.txt"))

(with-open-file (in *fn* :external-format :utf-16le )
  (let* ((names-256 (ppcre:split #\Tab (read-line in)))
        (cols (position-if
               #'(lambda (el)
                   (or (string= el "") (string= el "")))
               names-256)))
    (loop :for name :in (cdddr names-256)
          :for n :from 0 :below (- cols 3)
          :collect
          (let ((a-signal (make-instance 'r/a-sig:<a-signal>))
                (name-3 (foo name) )
                )
            (setf  (r/a-sig:<a-signal>-num a-signal) n)
            (setf  (r/a-sig:<a-signal>-id  a-signal) (first name-3))
            (setf  (r/a-sig:<a-signal>-description  a-signal) (second name-3))
            (setf  (r/a-sig:<a-signal>-units  a-signal) (third name-3))
            a-signal
            )
          
          )))


(defun foo (string)
  (multiple-value-bind (whole m1)
      (ppcre:scan-to-strings
       "^([^:]*):?([^,]*),?(.*)$" string)
    (loop :for i :across m1
          :collect (string-trim " " i))))

(foo "EB040: Вибрация корпуса ГТД (КС), vv2")


  (make-instance 'r/a-sig:<a-signal>)



