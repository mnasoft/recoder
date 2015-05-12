;;;; package.lisp

(defpackage #:recoder
  (:use #:cl)
  (:export *ascii-sym*
	   *cp866*
	   *cp1251*)
  (:export recode-string)
  (:export open-trd-file-read
	   open-trd-file-write)
  (:export read-trd-file
	   write-trd-file)
  (:export read-trd-file-short
	   read-trd-file-int
	   read-trd-file-long
	   read-trd-file-long-long
	   read-trd-file-float
	   read-trd-file-double
	   read-trd-file-quad
	   read-trend-hdr)
  (:export write-trd-file-short
	   write-trd-file-int
	   write-trd-file-long
	   write-trd-file-long-long
	   write-trd-file-float
	   write-trd-file-double
	   write-trd-file-quad)
  (:export test_01))


