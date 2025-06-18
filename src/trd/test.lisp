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



(defun detect-utime (line)
  (let* ((lst (ppcre:split #\tab line))
         (dd-mm-yyyy (ppcre:split #\. (first lst)))
         (hh-mm-ss (ppcre:split #\: (second lst)))
         (ss-mm-hh-dd-mm-yyyy
           (append (mapcar #'parse-integer (reverse hh-mm-ss))
                   (mapcar #'parse-integer dd-mm-yyyy))))
    (apply #'encode-universal-time ss-mm-hh-dd-mm-yyyy)))


(defun utime-stream (file-name trd)
  (with-open-file (in file-name :external-format :utf-16le)
    (read-line in) ;; Пропускаем первую строку заголовков
    (let ((ut-start nil)
          (ut-end   nil)
          (i -1))
      (loop :for line = (read-line in nil nil)
            :while (and line
                        (< 0 (length
                              (string-trim '(#\Space #\Tab #\Return) line))))
            :do
               (incf i)
               (if (= i 0)
                 (setf ut-start (detect-utime line))
                 (setf ut-end   (detect-utime line))))
      (setf (<trd>-utime-start trd) ut-start)
      (setf (<trd>-increment trd)
            (coerce (/ (- ut-end ut-start) i) 'double-float))
      trd)))

(utime-stream *fn-txt*)

(defparameter *fn-txt*
  (probe-file
   (fname-xls->txt *fn-xls*)))

(defparameter *trd* (make-instance '<trd> :file-name *fn-txt*))

(r/g:read-obj *trd* *fn-txt*)

(utime-stream *fn-txt* *trd*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

