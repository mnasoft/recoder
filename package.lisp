;;;; package.lisp

(defpackage #:recoder
  (:use #:cl)
  (:export *ascii-sym*
	   *cp866*
	   *cp1251*)
  (:export list-to-int)
  
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
	   trd-start-offset trd-record-length trd-discret-length-byte trd-analog-length-byte trd-discret-offset

	   trd-date-time-end
	   trd-analog-signal-list trd-discret-signal-list
	   
	   trd-record-number-by-udate	   
	   trd-analog-by-rec-number trd-analog-by-universal-date
	   trd-analog-mid-by-udate
   	   trd-analog-mid-by-snames
	   trd-analog-stddev-by-udate
	   trd-analog-stddev-by-snames
	   
	   trd-discret-by-rec-number trd-discret-by-rec-number-t-nil
	   trd-flag-on-intervals trd-flag-on-intervals-time	   

	   make-html-trd
	   make-transpose-html-trd)
  
  (:export test_01)
  (:export get-trd-by-utime-dirname)
  (:export trd-export-csv trd-split-signal trd-export-csv-singal-string
	   trd-split-by-conndition-intervals
	   )
  (:export apply-and  apply-or)

  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
