;; ./src/dir/test.lisp

;;;; (in-package :recoder/dir)

(in-package :recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               +------FH27--> в свечу    ;;;;
;;;;               |                         ;;;;
;;;;      +--FH25--+------FH26--> в КГТ3     ;;;;
;;;; З1---+                                  ;;;;
;;;;      +--FH28--+--+---FH29--> в КДТ1     ;;;;
;;;;               |   \                     ;;;;
;;;;               |    --FH31--> в КДТ2     ;;;;
;;;;               |                         ;;;;
;;;;               +------FH30--> в дренаж   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aver-max-min (seq)
  (let ((mid-v (math/stat:average-value seq))
	(max-v (math/stat:max-value seq))
	(min-v (math/stat:min-value seq)))
    (list mid-v max-v min-v)))

(defun aver-dmax-dmin (seq)
  (let ((mid-v (math/stat:average-value seq))
	(max-v (math/stat:max-value seq))
	(min-v (math/stat:min-value seq)))
  (list mid-v (- max-v mid-v) (- min-v mid-v))))

(defun foo-Oil2Gas (trd interval)
  (let ((rez nil)
	(utime (trd-utime-by-record-number trd (first interval))))
    (setf rez (cons (first interval) rez))
    (setf rez (cons (position-if
		     #'(lambda (el)
			 (and (> (sig "FA016" el trd) 0.5))) trd :start (first rez))
		    rez))
    (setf rez (cons (position-if
		     #'(lambda (el)
			 (and (< (sig "FA026" el trd) 0.5))) trd :start (first rez))
		    rez))
    (setf rez (cons (position-if
		     #'(lambda (el)
			 (and (sig-on "FK301" el trd))) trd :start (first rez))
		    rez))
    (setf rez (cons (position-if
		     #'(lambda (el)
			 (and (sig-on "FK301" el trd)
			      (sig-on "FK280" el trd)
			      (sig-on "FK290" el trd)
			      (sig-on "FK310" el trd))) trd :start (first rez)) rez))
    ;; (setf rez (cons (+ (first rez) (* 5 30)) rez))
    (setf rez (nreverse rez))
;;;(break "rez=~S:" rez )
    (append (mnas-org-mode:utime->date-time utime) 
	  (apply #'append
		 (mapcar #'(lambda (start end)
			     (apply #'append (cons (list (* (- end start) 0.2))
						   (mapcar #'aver-dmax-dmin
							   (analogs-in-records 
							    trd  start end
							    (trd-analog-signal-list trd '("GQ010" "EN2")))))))
;;; "EN2" "T04" "T04max" "T04min"
			 rez
			 (cdr rez))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *sig* '("GQ010"
			"FA016" "FA010" "G1" "FQ010"
			"FA026" "FA020" "G2" "FQ110"
   			"EN1" "EN2" "EN3"
			"T04" "T04max" "T04min"
			"Oil2Gas" "Gas2Oil"
			"FK250" "FK251" "FK260" "FK261" "FK270" "FK271"
			"FK280" "FK281" "FK290" "FK291" "FK300" "FK301" "FK310" "FK311"))
  (defparameter *trd* (make-instance
		       'recoder:<trd-seq>
		       :trd-file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
		       :s-sig *sig*)))

(trd-open *trd*)

(split-on-intervals-when-flag-is-on *trd* "Oil2Gas")

(defparameter *per-foo-Oil2Gas*
  (apply #'append
	 (mapcar
	  #'(lambda (t-i)
	      (let ((trd-seq (make-instance 'recoder:<trd-seq> :trd-file-name (first t-i) :s-sig *sig*)))
		(trd-open trd-seq)
;;; (break "0000:")
		(mapcar
		 #'(lambda (int)
;;;(break "0000:~S" int)
		     (foo-Oil2Gas trd-seq int))
		 (second t-i))))
	  (cdr *i-t*))))

(defparameter *trd*
  (make-instance 'recoder:<trd-seq>
		 :trd-file-name "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
		 :s-sig *sig*))

(trd-open *trd*)

(foo-Oil2Gas *trd* '(13355 14081))

(first *i-t*) 

(defparameter *i-t* 
  (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas"))

(split-on-utimes-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (require :recoder/dir)

(defparameter *trd-CPIPES-dir*
  (make-instance 'recoder/dir:<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))

(recoder/dir: *trd-CPIPES-dir* ) 
 
(defparameter *i-t* 
  (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas"))

