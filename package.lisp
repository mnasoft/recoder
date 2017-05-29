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
  (:export a-signal-num  a-signal-id  a-signal-description  a-signal-units  a-signal-min  a-signal-max)
  (:export a-signal a-signal-value)

  (:export d-signal-num d-signal-id d-signal-description)
  (:export d-signal)
  
  (:export trd-file-name trd-file-descr trd-id-string trd-version trd-date-time trd-reserv trd-total-records trd-delta-time
	   trd-analog-number trd-discret-number trd-analog-ht trd-discret-ht )
  (:export trd
	   print-object
	   trd-open
	   trd-close
	   trd-start-offset
	   trd-record-length
	   trd-date-time-end
	   trd-analog-signal-list
	   trd-record-number-by-udate	   
	   trd-values-by-rec-number
	   trd-values-by-universal-date
	   trd-mid-values-by-udate
   	   trd-mid-values-by-snames
	   trd-stddev-values-by-udate
	   trd-stddev-values-by-snames
	   make-html-trd
	   make-transpose-html-trd)


  (:export test_01)
  (:export get-trd-by-utime-dirname)
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

