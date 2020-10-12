;;;; package.lisp

(defpackage #:recoder
  (:use #:cl #:mnas-string #:recoder/binary #:recoder/d-signal #:recoder/a-signal)

  (:export trd-analog-ht->org
           trd-analog-mid-by-snames
           trd-analog-by-utime
           trd-analog-signal-list
           trd-analog-stddev-by-snames
           trd-analog-mid-by-utime
           trd-analog-by-rec-number
           trd-analog-length-byte 
           trd-analog-stddev-by-utime
           trd-analog-ht 
           trd-analog-number
           trd-file-descr
           )
  (:export trd-discret-by-utime
           trd-discret-length-byte
           trd-discret-number
           trd-discret-by-utime-t-nil
           trd-discret-offset
           trd-discret-by-rec-number-t-nil
           trd-discret-signal-list
           trd-discret-by-rec-number
           trd-discret-ht->org
           trd-discret-ht
           )
  (:export trd-separate-a-signals
           trd-separate-not-signals
           trd-separate-signals
           trd-separate-d-signals
           )
  (:export trd-a-units
           trd-a-ids
           )
  (:export <trd>
	   trd-total-records
	   trd-delta-time
	   trd-export-csv
	   trd-header->org
	   trd-interval-to-minutes
	   trd-record-number-to-udate
	   trd-id-string
	   
	   trd-record-length
	   
	   trd-utime-end
	   trd-open
	   
	   trd-version
	   trd-file-name
	   
	   trd-interval-to-hours
	   trd-record-number-by-udate
	   trd-start-offset 
	   trd-reserv recode-string
	   trd-split-on-intervals-of-time-when-flag-is-on
	   
	   trd-split-on-intervals-by-condition
	   trd-interval-to-secods
	   trd-close 
	   trd-utime-start
	   trd-record-number-by-utime
	   
	   trd-export-csv-singal-string
	   trd-split-on-intervals-when-flag-is-on
	   )
  (:export *mid-value-number-offset*
	   )
  (:export apply-and
	   apply-or
	   )
  (:export make-html-trd-foo
	   make-html-trd
	   )
  (:export
	   find-trd-by-utime-dirname
	   get-open-ternds
	   get-open-ternd
	   change-directory-default
	   time-universal-encode
	   )
  (:export open-trd-file-write
	   open-trd-file-read
	   ))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
