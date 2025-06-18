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
   (mnas-path:asdf-path :recoder "trd/1_Custom_sec_14April2025_10226_PM.txt")))

(defparameter *trd* (make-instance '<trd> :file-name *fn*))

(r/g:read-obj *trd* *fn*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(uiop:run-program '("powershell.exe" "-Command" "Get-Date") :output :string :external-format :cp866)

(uiop:run-program '("powershell.exe" "-Command" "Get-Date") :output :string :external-format :cp866)

$Script = "D:\\home\\_namatv\\PRG\\msys64\\home\\namatv\\quicklisp\\local-projects\\clisp\\recoder\\ps1\\ConvertExcelToTxt.ps1"
$InputFile = 
$OutputFile = 
$SkipRows = 15

(uiop:run-program
 '("powershell.exe"
   "-ExecutionPolicy" "Bypass"
   "-File" "D:\\home\\_namatv\\PRG\\msys64\\home\\namatv\\quicklisp\\local-projects\\clisp\\recoder\\ps1\\ConvertExcelToTxt.ps1"
   "-InputFile" "D:\\home\\_namatv\\PRG\\msys64\\home\\namatv\\quicklisp\\local-projects\\clisp\\recoder\\trd\\1_Custom_sec_14April2025_10226_PM.xls"
   "-OutputFile" "D:\\home\\_namatv\\PRG\\msys64\\home\\namatv\\quicklisp\\local-projects\\clisp\\recoder\\trd\\1_Custom_sec_14April2025_10226_PM.xls.txt"
   "-SkipRows" "15"))
