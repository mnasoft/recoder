;;;; package.lisp


(defpackage #:recoder)

(defpackage #:recoder
  (:use #:cl #:mnas-string)
  (:export recoder::*ascii-sym*
	   recoder::*cp866*
	   recoder::*cp1251*
	   )
  (:export recoder::list-to-int
	   )
  (:export recoder::recode-string
	   )
  (:export recoder::open-trd-file-read
	   recoder::open-trd-file-write
	   )
  (:export recoder::read-trd-file
	   recoder::write-trd-file
	   )
  (:export recoder::read-trd-file-short
	   recoder::read-trd-file-int
	   recoder::read-trd-file-long
	   recoder::read-trd-file-long-long
	   recoder::read-trd-file-float
	   recoder::read-trd-file-double
;;;;	   recoder::read-trd-file-quad
	   recoder::read-trend-hdr
	   )
  (:export recoder::write-trd-file-short
	   recoder::write-trd-file-int
	   recoder::write-trd-file-long
	   recoder::write-trd-file-long-long
	   recoder::write-trd-file-float
	   recoder::write-trd-file-double
;;;;	   recoder::write-trd-file-quad
	   )
  
  (:export recoder::transpose
	   recoder::time-universal-encode
	   recoder::recode-string
	   )
  (:export recoder::a-signal-num
	   recoder::a-signal-id
	   recoder::a-signal-description
	   recoder::a-signal-units
	   recoder::a-signal-min
	   recoder::a-signal-max)
  
  (:export recoder::a-signal
	   recoder::a-signal-value)

  (:export recoder::d-signal-num
	   recoder::d-signal-id
	   recoder::d-signal-description
	   )
  (:export recoder::d-signal
	   )
  (:export recoder::trd
;;;;	   recoder::print-object
	   )
  
;;;; Class trd members  
  (:export recoder::trd-file-name
	   recoder::trd-file-descr
	   recoder::trd-id-string
	   recoder::trd-version
	   recoder::trd-utime-start
	   recoder::trd-reserv
	   recoder::trd-total-records
	   recoder::trd-delta-time
	   recoder::trd-analog-number
	   recoder::trd-discret-number
	   recoder::trd-analog-ht
	   recoder::trd-discret-ht
	   )
;;;; Open and Close trd  
  (:export recoder::trd-open
	   recoder::trd-close
	   )
  (:export recoder::trd-start-offset
	   recoder::trd-record-length
	   recoder::trd-discret-length-byte
	   recoder::trd-analog-length-byte
	   recoder::trd-discret-offset
	   recoder::trd-utime-end
	   recoder::trd-analog-signal-list
	   recoder::trd-discret-signal-list
	   )
;;;; Analog signal extraction  
  (:export recoder::trd-analog-by-rec-number
	   recoder::trd-analog-by-utime
	   recoder::trd-analog-mid-by-utime
	   recoder::trd-analog-mid-by-snames
	   recoder::trd-analog-stddev-by-utime
	   recoder::trd-analog-stddev-by-snames
	   )
;;;; Discret signal extraction    
  (:export recoder::trd-discret-by-rec-number
	   recoder::trd-discret-by-rec-number-t-nil
	   )
  (:export recoder::make-html-trd
	   recoder::make-html-trd-foo  
	   )
  (:export recoder::test_01
	   )
  (:export recoder::get-trd-by-utime-dirname
	   )
;;;; Signal separation
  (:export recoder::trd-separate-signals
	   recoder::trd-separate-a-signals
	   recoder::trd-separate-d-signals
	   recoder::trd-separate-not-signals
	   )
;;;; CSV 
  (:export recoder::trd-export-csv
	   recoder::trd-export-csv-singal-string
	   )
;;;; Slitting 
  (:export recoder::trd-split-on-intervals-when-flag-is-on
	   recoder::trd-split-on-intervals-of-time-when-flag-is-on
	   recoder::trd-split-on-intervals-by-condition
	   )
;;;; Interval to time conversion
  (:export recoder::trd-interval-to-secods
	   recoder::trd-interval-to-minutes
	   recoder::trd-interval-to-hours
	   )
  (:export recoder::apply-and
	   recoder::apply-or
	   )
;;;; Open directory  
  (:export recoder::get-open-ternd
	   recoder::get-open-ternds
	   recoder::change-directory-default
	   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
