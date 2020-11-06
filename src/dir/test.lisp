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
  (list (math/stat:average-value seq)
        (math/stat:max-value seq)
        (math/stat:min-value seq)))
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
  ;; (defparameter *ht* (make-ht-by-sig-names *sig*))
  (defparameter *trd* (make-instance
		       'recoder:<trd-seq>
		       :trd-file-name "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
		       :signal-strings *sig*)))


(sig "GQ010" (elt *trd* (+ 10931 (* 5 10))) *trd*)

(elt *trd* 11082)

(split-on-intervals-when-flag-is-on *trd* "Oil2Gas")

(defun foo-Oil2Gas (trd interval)
  (let ((rez nil))
    (setf rez (cons (first interval) rez))
    (setf rez (cons (position-if #'(lambda (el) (and (> (sig "FA016" el *ht*) 0.5))) trd :start (first rez)) rez))
    ;;  (setf rez (cons (position-if #'(lambda (el) (and (< (sig "FA026" el *ht*) 0.5))) trd :start (first rez)) rez))
    (setf rez (cons (position-if #'(lambda (el) (and (sig-on "FK301" el *ht*))) trd :start (first rez)) rez))
    (setf rez (cons (position-if #'(lambda (el) (and (sig-on "FK301" el *ht*)
						     (sig-on "FK280" el *ht*)
						     (sig-on "FK290" el *ht*)
						     (sig-on "FK310" el *ht*))) trd :start (first rez)) rez))
    (setf rez (cons (+ (first rez) (* 5 30)) rez))
    (setf rez (nreverse rez))
    (mapcar #'(lambda (start end)
		(cons (* (- end start) 0.2)
			(mapcar #'aver-max-min
				(analogs-in-records 
				 trd  start end
				 (trd-analog-signal-list trd '("GQ010")))))) ;;;; "EN2" "T04" "T04max" "T04min"
	    rez
	    (cdr rez))))
    
(foo-Oil2Gas *trd* '(10931 11252))

((10931 11252) (15413 15677) (19598 19858) (24705 24971) (29177 29440)
	       (33479 33755) (37542 37813) (42104 42367))

(trd-separate-signals *trd* '("GQ010" "EN1" "EN2" "EN3" "T04" "Oil2Gas"))

(mapcar #'math/stat:average-value (analogs-in-utimes *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04"))))

(trd-analog-mid-by-utime *trd*  (+ 3806391948 30) (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))

(math/stat:max-value (nth 0 (analogs-in-records *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))))
(math/stat:min-value (nth 2 (analogs-in-utimes *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))))



(recoder:trd-utime-by-record-number *trd* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (require :recoder/dir)

(defparameter *trd-CPIPES-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))

(split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas")

(split-on-utimes-when-flag-is-on    *trd-CPIPES-dir* "Oil2Gas")



(analogs-in-utimes 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((ut-s `(,(encode-universal-time 07 12 14 29 07 2020))))
  (analog-table ut-s
		`(,*trd-CPIPES-dir* ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-C100-dir*   ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-NIO-dir*    ,(append (loop :for i :from 1 :to 76 :collect (format nil "T15_~2,'0D" i)) '("CF104" "CF140")))
		))

(recoder:trd-split-on-intervals-of-time-when-flag-is-on trd d-signal-str) 
(recoder:trd-split-on-intervals-by-condition trd start-signal-str-lst end-signal-str-lst)
(recoder:split-on-intervals-when-flag-is-on)
