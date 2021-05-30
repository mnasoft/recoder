;;;; package.lisp

(defpackage #:recoder/binary
  (:use #:cl #:mnas-string)
  (:export *cp1251*
	   *cp866*
           *ascii-sym*
	   )
  (:export list-to-int
           open-trd-file-read
           open-trd-file-write
           )
  (:export read-trd-file-long
	   read-trd-file-long-long
	   read-trd-file-float
	   read-trd-file-int trd-file-descr
	   read-trd-file-double
	   read-trd-file
	   read-trd-file-short
	   )
  (:export write-trd-file-int
	   write-trd-file-long
	   write-trd-file-float
	   write-trd-file
	   write-trd-file-long-long
	   write-trd-file-double 
	   write-trd-file-short
	   ))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
