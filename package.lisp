;;;; package.lisp

(defpackage #:recoder
  (:use #:cl)
  (:export *ascii-sym*
	   *cp866*
	   *cp1251*
	   recode-string)
  (:export open-trd-file-read
	   open-trd-file-write)
  (:export read-trd-file
	   write-trd-file)
  (:export   read-trd-file-short
	     read-trd-file-int
	     read-trd-file-long
	     read-trd-file-long-long
	     read-trd-file-float
	     read-trd-file-double
	     read-trd-file-quad
	     read-trend-hdr)
  (:export test_01))

