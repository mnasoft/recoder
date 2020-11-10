;; ./src/dir/test.lisp

;;;; (in-package :recoder/dir)

(in-package :recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun utime-dtime-signals (trd records signals)
  (append
   (cons (* (trd-delta-time trd) (- (first (last records)) (first records)))
	    (mnas-org-mode:utime->date-time (trd-utime-by-record-number trd (first records))))
   (apply #'append
	  (mapcar #'(lambda (start end)
		      (apply #'append (cons (list (* (- end start) 0.2))
					    (mapcar #'math/stat:aver-dmax-dmin
						    (analogs-in-records 
						     trd  start end
						     (trd-analog-signal-list trd signals)))))) ;;; "T04" "T04max" "T04min"
		  records (cdr records)))))


(setf next-find (position-if #'(lambda (el) (and (sig-on "FK251" el trd)
						     (sig-on "FK261" el trd))) trd :start (first rez)))
(if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (sig-on \"FK251\" el trd) (sig-on \"FK261\" el trd))"))

(defun foo-Oil2Gas (trd interval)
  (let ((rez nil)
        (next-find nil))
    (push (first interval) rez)
;;;;    
    (setf next-find (position-if #'(lambda (el) (> (sig "FA016" el trd) 0.5)) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(> (sig \"FA016\" el trd) 0.5)"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (< (sig "FA026" el trd) 0.5)) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(< (sig \"FA026\" el trd) 0.5)"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (sig-on "FK301" el trd))      trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(sig-on \"FK301\" el trd)"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (sig-on "FK301" el trd) (sig-on "FK280" el trd) (sig-on "FK290" el trd) (sig-on "FK310" el trd))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (sig-on \"FK301\" el trd) (sig-on \"FK280\" el trd) (sig-on \"FK290\" el trd) (sig-on \"FK310\" el trd))"))
;;;;
    (setf rez (nreverse rez))
    (utime-dtime-signals trd rez '("GQ010" "EN2"))))

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

(defun foo-Gas2Oil (trd interval)
  (let ((rez nil)
	(next-find nil))
    (push (first interval) rez)
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (sig-on "FK281" el trd)
						     (sig-on "FK291" el trd)
						     (sig-on "FK311" el trd))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (sig-on \"FK251\" el trd) (sig-on \"FK261\" el trd))"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (> (sig "FA026" el trd) 0.5))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (> (sig \"FA026\" el trd) 0.5))"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (< (sig "FA016" el trd) 0.5))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (< (sig \"FA016\" el trd) 0.5))"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (sig-on "FK271" el trd))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (sig-on \"FK271\" el trd))"))
;;;;    
    (setf next-find (position-if
		     #'(lambda (el)
			 (and (sig-on "FK271" el trd)
			      (sig-on "FK250" el trd)
			      (sig-on "FK260" el trd))) trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(and (sig-on \"FK271\" el trd) (sig-on \"FK250\" el trd) (sig-on \"FK260\" el trd))"))
    (setf rez (nreverse rez))
    (utime-dtime-signals trd rez '("GQ010" "EN2"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sig* '("GQ010"
			"FA016" "FA010" "G1" "FQ010"
			"FA026" "FA020" "G2" "FQ110"
   			"EN1" "EN2" "EN3"
			"T04" "T04max" "T04min"
			"Oil2Gas" "Gas2Oil"
			"FK250" "FK251" "FK260" "FK261" "FK270" "FK271"
			"FK280" "FK281" "FK290" "FK291" "FK300" "FK301" "FK310" "FK311"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (require :recoder/dir)

(defparameter *trd-CPIPES-dir*
  (make-instance 'recoder/dir:<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))

(defparameter *i-t-Oil2Gas* 
  (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas"))

(defparameter *per-foo-Oil2Gas*
  (apply #'append
	 (mapcar
	  #'(lambda (t-i)
	      (let ((trd-seq (make-instance 'recoder:<trd-seq> :trd-file-name (first t-i) :s-sig *sig*)))
		(trd-open trd-seq)
		(mapcar #'(lambda (int) (foo-Oil2Gas trd-seq int)) (second t-i))))
	  *i-t-Oil2Gas*)))

(require :mnas-format)
(mnas-format:round-2d-list *per-foo-Oil2Gas*)

'(("d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_100354.trd" (13355 14081) "(sig-on \"FK301\" el trd)")
  ("d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_100354.trd" (13355 14081) "(and (sig-on \"FK301\" el trd) (sig-on \"FK280\" el trd) (sig-on \"FK290\" el trd) (sig-on \"FK310\" el trd))"))

*PER-FOO-OIL2GAS*

(defparameter *i-t-Gas2Oil* 
  (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Gas2Oil"))

(defparameter *per-foo-Gas2Oil*
  (apply #'append
	 (mapcar
	  #'(lambda (t-i)
	      (let ((trd-seq (make-instance 'recoder:<trd-seq> :trd-file-name (first t-i) :s-sig *sig*)))
		(trd-open trd-seq)
		(mapcar #'(lambda (int) (foo-Gas2Oil trd-seq int)) (second t-i))))
	  *i-t-Gas2Oil*)))

(mnas-format:round-2d-list *per-foo-Gas2Oil*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
