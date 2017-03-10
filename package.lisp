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
  (:export transpose
	   time-universal-encode
	   recode-string)
  (:export a-signal
	   a-signal-value)
  (:export d-signal)
  (:export trd
	   print-object
	   trd-open
	   trd-start-offset
	   trd-record-length
	   trd-date-time-end
	   trd-analog-signal-list
	   trd-values-by-rec-number
	   trd-record-number-by-udate
	   trd-values-by-universal-date
	   trd-mid-values-by-udate
   	   trd-mid-values-by-snames
	   trd-stddev-values-by-udate
	   trd-stddev-values-by-snames
	   make-html-trd
	   make-transpose-html-trd)
  (:export test_01))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))



