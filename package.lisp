;;;; package.lisp

(defpackage #:recoder
  (:use #:cl #:mnas-string)
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
  
  (:export trd
	   print-object)
  
;;;; Class trd members  
  (:export trd-file-name
	   trd-file-descr
	   trd-id-string
	   trd-version
	   trd-utime-start
	   trd-reserv
	   trd-total-records
	   trd-delta-time
	   trd-analog-number
	   trd-discret-number
	   trd-analog-ht
	   trd-discret-ht )
  
;;;; Open and Close trd  
  (:export trd-open
	   trd-close)
  (:export trd-start-offset
	   trd-record-length
	   trd-discret-length-byte
	   trd-analog-length-byte
	   trd-discret-offset
	   trd-utime-end
	   trd-analog-signal-list
	   trd-discret-signal-list
	   trd-record-number-by-udate)
;;;; Analog signal extraction  
  (:export trd-analog-by-rec-number
	   trd-analog-by-utime
	   trd-analog-mid-by-utime
	   trd-analog-mid-by-snames
	   trd-analog-stddev-by-utime
	   trd-analog-stddev-by-snames)
;;;; Discret signal extraction    
  (:export trd-discret-by-rec-number
	   trd-discret-by-rec-number-t-nil)

  (:export 
   make-html-trd
   make-html-trd-foo  
   )
  
  (:export test_01)
  (:export get-trd-by-utime-dirname)

;;;; Signal separation
  (:export trd-separate-signals
	   trd-separate-a-signals
	   trd-separate-d-signals
	   trd-separate-not-signals)
;;;; CSV 
  (:export trd-export-csv
	   trd-export-csv-singal-string)
;;;; Slitting 
  (:export trd-split-on-intervals-when-flag-is-on
	   trd-split-on-intervals-of-time-when-flag-is-on
	   trd-split-on-intervals-by-condition )
;;;; Interval to time conversion
  (:export trd-interval-to-secods trd-interval-to-minutes trd-interval-to-hours)
	   
  (:export apply-and  apply-or)

  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
