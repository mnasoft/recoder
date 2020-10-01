;;;; package.lisp

(defpackage #:recoder
  (:use #:cl #:mnas-string)
  (:export a-signal-units
	   trd-analog-ht->org
	   *cp1251*
	   read-trd-file-long
	   trd-analog-mid-by-snames
	   trd-a-units
	   trd-discret-by-utime
	   trd-delta-time
	   a-signal-num
	   trd-export-csv
	   write-trd-file-int
	   trd-export-csv-singal-string
	   apply-or
	   <trd>
	   trd-separate-a-signals
	   trd-header->org
	   trd-interval-to-minutes
	   read-trd-file-long-long
	   trd-discret-length-byte
	   write-trd-file-long
	   trd-id-string
	   trd-analog-by-utime
	   read-trd-file-float
	   make-html-trd-foo
	   write-trd-file-float
	   trd-analog-number
	   a-signal-min
	   a-signal-id
	   a-signal-value
	   find-trd-by-utime-dirname
	   d-signal-description
	   write-trd-file
	   get-open-ternds
	   change-directory-default
	   read-trd-file-short
	   write-trd-file-short
	   trd-record-length
	   trd-analog-signal-list
	   make-html-trd
	   write-trd-file-long-long
	   d-signal-id
	   trd-record-number-to-udate
	   read-trd-file-double
	   time-universal-encode
	   get-open-ternd
	   trd-discret-number
	   trd-discret-by-utime-t-nil
	   trd-analog-stddev-by-snames
	   trd-utime-end
	   trd-open
	   apply-and a-signal-description
	   trd-discret-ht->org read-trd-file
	   trd-split-on-intervals-when-flag-is-on
	   open-trd-file-write list-to-int
	   trd-a-ids trd-discret-offset
	   trd-analog-mid-by-utime
	   trd-discret-by-rec-number-t-nil
	   *cp866* trd-record-number-by-utime
	   write-trd-file-double trd-discret-ht
	   trd-version trd-separate-not-signals
	   <d-signal> trd-file-name
	   trd-analog-by-rec-number a-signal-max
	   trd-separate-signals
	   trd-analog-length-byte *ascii-sym*
	   trd-analog-stddev-by-utime
	   trd-interval-to-hours
	   trd-record-number-by-udate
	   trd-discret-signal-list
	   trd-start-offset open-trd-file-read
	   trd-reserv recode-string
	   trd-split-on-intervals-of-time-when-flag-is-on
	   trd-analog-ht trd-total-records
	   trd-split-on-intervals-by-condition
	   *mid-value-number-offset*
	   trd-interval-to-secods <a-signal>
	   trd-close trd-discret-by-rec-number
	   read-trd-file-int trd-file-descr
	   d-signal-num trd-utime-start
	   trd-separate-d-signals)

  
;;;; Open and Close trd  
;;;; Analog signal extraction  
;;;; Discret signal extraction    
;;;; Signal separation
;;;; CSV 
;;;; Splitting 
;;;; Interval to time conversion
;;;; Open directory  
;;;; Expotr to org  
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
