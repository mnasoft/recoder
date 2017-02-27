;;;; DM80L№1_100_10_CPIPES_trd.lisp


(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn (defparameter *t* (make-instance 'trd :trd-file-name "~/develop/TRD/DM80L№1-100-10/CPIPES/20170123_085545.trd")) (trd-open *t*))

(progn 
  (defparameter *a-s-lst* 
    (trd-analog-signal-list *t* '("GQ010" "EN1" "EN2" 
				  "T04" "dT04plus" "dT04minus"
				  "T01"
				  "P02"
				  "FP210" "FP220" "FP230"
				  "PT240" "PT230" "PT250" 
				  "FQ110" "FA020"
				  "FQ010" "FA010" 
				  "EB100" "EB110" "EB120"
				  ))) *a-s-lst*)
(progn 
  (defparameter *dt*
    (mapcar #'(lambda (el) (trd-value-mid-list-by-udate *t* el  *a-s-lst*))
	    (list(encode-universal-time 00 21 9  23 1 2017) ;;;; 1.0 МВт
		 (encode-universal-time 07 31 9  23 1 2017) ;;;; 4.0 МВт
		 (encode-universal-time 50 42 9  23 1 2017) ;;;; 8.0 МВт
		 (encode-universal-time 19 52 9  23 1 2017) ;;;; 11.0 МВт
		 (encode-universal-time 02 12 10 23 1 2017) ;;;; 14.0 МВт
		 (encode-universal-time 18 33 10 23 1 2017) ;;;; 16.0 МВт
		 (encode-universal-time 53 53 10 23 1 2017) ;;;; 18.0 МВт
		 (encode-universal-time 53 12 11 23 1 2017) ;;;; 20.8 МВт
		 (encode-universal-time 16 33 11 23 1 2017) ;;;; 23.16 МВт
		 (encode-universal-time 52 10 16 23 1 2017) ;;;; 22.53 МВт
		 )))
  (push (mapcar #'(lambda (el) (a-signal-units el) ) *a-s-lst* ) *dt*)
  (push (mapcar #'(lambda (el) (a-signal-id el) ) *a-s-lst* ) *dt*))

(require :html-table)

(html-table:list-list-html-table *dt* "~/rez.html")

(close (trd-file-descr *t*))
