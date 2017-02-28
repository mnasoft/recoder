;;;; DM80L№1_100_10_CPIPES_trd.lisp


(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *t-cpipes* (make-instance 'trd :trd-file-name "~/develop/TRD/DM80L№1-100-10/CPIPES/20170123_085545.trd")) (trd-open *t-cpipes*)
  (defparameter *a-s-lst* 
    (trd-analog-signal-list *t-cpipes* '("GQ010" "EN1" "EN2" 
					 "T04" "dT04plus" "dT04minus"
					 "T01"
					 "P02" 
					 "FT020" "FP210" "FP220" "FA020" "FQ110"

					 "FT010" "FP230" "FA010" "FQ010"
					 "PT240" "PT230" "PT250" 
				  
				   
					 "EB100" "EB110" "EB120"
					 )))
  *a-s-lst*)

"FP470" 
"FP450" 
"FP460" 

(progn 
  (defparameter *dt-cpipes*
    (mapcar #'(lambda (el) (trd-mid-values-by-udate *t-cpipes* el  *a-s-lst*))
	    (list(encode-universal-time 00 21 9  23 1 2017) ;;;; 1.0 МВт
		 (encode-universal-time 07 31 9  23 1 2017) ;;;; 4.0 МВт
		 (encode-universal-time 50 42 9  23 1 2017) ;;;; 8.0 МВт
		 (encode-universal-time 19 52 9  23 1 2017) ;;;; 11.0 МВт
		 (encode-universal-time 02 12 10 23 1 2017) ;;;; 14.0 МВт
		 (encode-universal-time 18 33 10 23 1 2017) ;;;; 16.0 МВт
		 (encode-universal-time 53 53 10 23 1 2017) ;;;; 18.0 МВт
		 (encode-universal-time 53 12 11 23 1 2017) ;;;; 20.8 МВт
		 (encode-universal-time 52 10 16 23 1 2017) ;;;; 22.53 МВт
		 (encode-universal-time 16 33 11 23 1 2017) ;;;; 23.16 МВт
		 )))
  (push (mapcar #'(lambda (el) (a-signal-units el)) *a-s-lst* ) *dt-cpipes*)
  (push (mapcar #'(lambda (el) (a-signal-id el) )   *a-s-lst* ) *dt-cpipes*)
  (push (mapcar #'(lambda (el) (gethash (a-signal-id el) *ht-s-o*) ) *a-s-lst* ) *dt-cpipes*)
  
  (html-table:list-list-html-table *dt-cpipes* "~/rez-cpipes.html"))

;;;; (close (trd-file-descr *t-cpipes*))


