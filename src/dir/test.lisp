;; ./src/dir/test.lisp

;;;; (in-package :recoder/dir)

(in-package :recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun utime-dtime-signals-v0 (trd records signals)
  (append
   (cons (* (trd-delta-time trd) (- (first (last records)) (first records)))
	 (mnas-org-mode:utime->date-time (trd-utime-by-record-number trd (first records))))
   (apply #'append
	  (mapcar #'(lambda (start end)
		      (apply #'append (cons (list (* (- end start) 0.2))
					    (mapcar #'math/stat:aver-dmax-dmin
						    (analogs-in-records 
						     trd  start end
						     (trd-analog-signal-list trd signals)))))) 
		  records (cdr records)))))

(defun find-divisor (val)
  "@b(Описание:) функция @b(find-divisor)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-divisor 10.964739714723287d0)
@end(code)
"
  (do ((divisor 1))
      ((<= 1 (abs (* val divisor)) 10) divisor)
    (cond
      ((< (abs (* val divisor)) 1) (setf divisor (* divisor 10)))
      ((< 1 (abs (* val divisor))) (setf divisor (/ divisor 10))))))

(defun my-round (val &optional (sb-kernel::divisor 1))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (my-round 10.964739714723287d0 1/100) 
 => 10.96
@end(code)
"
  (* (round val sb-kernel::divisor)
     (coerce sb-kernel::divisor 'single-float)))

(defun my-round-n (val &optional (numbers 4) (base-val val))
  (my-round val
            (/ (expt 10 (* -1 (- numbers 1)))
               (find-divisor base-val))))

(my-round-n 0.1465 4 10.964739714723287d0)

(my-round-n 0.0010964739714723287d0)

  
(my-round 10.964739714723287d0 1/100)





(defun utime-dtime-signals (trd records signals)
  "@b(Описание:) функция @b(utime-dtime-signals)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (foo-Oil2Gas *trd* '(33479 33755))
@end(code)
"
  (let ((dt (coerce (trd-delta-time trd) 'single-float))
        (first-rec (first records)))
    (mapcar
     #'(lambda (start end)
	 (apply #'append (cons
                          (append (mnas-org-mode:utime->date-time (trd-utime-by-record-number trd first-rec))
                                  (list (* (- end first-rec) dt) (* (- end start) dt)))
			       (mapcar #'math/stat:aver-dmax-dmin
				       (analogs-in-records 
					trd  start end
					(trd-analog-signal-list trd signals)))))) 
     records (cdr records))
    ))

(apply #'append
	    )

(append
     (cons (* (trd-delta-time trd) (- (first (last records)) (first records)))
	   (mnas-org-mode:utime->date-time (trd-utime-by-record-number trd (first records))))
)

(defun foo-Oil2Gas (trd interval)
  "Переход с ДТ на ГТ

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elt-named *trd* (+ 33479 (* 5 21)))
@end(code)

"
  (let ((rez nil)
        (next-find nil))
    (push (first interval) rez)
;;;;    
    (setf next-find (position-if #'(lambda (el) (and
                                                 (> (sig "FA010" el trd) 0.5)
                                                 (> (sig "FA016" el trd) 0.5)
                                                 (sig-on "FK010" el trd)
                                                 (sig-on "FK020" el trd)
                                                 (> (sig "G1" el trd) 150.0)))
                                 trd :start (first rez)))
    (if next-find
	(push next-find rez)
	(format t "~S ~S ~S~%" (trd-file-name trd) interval "(> (sig \"FA016\" el trd) 0.5)"))
;;;;    
    (setf next-find (position-if #'(lambda (el) (and (< (sig "FA026" el trd) 0.5)
                                                     (< (sig "FA020" el trd) 0.5)
                                                     (sig-off "FK200" el trd)
                                                     (sig-on  "FK201" el trd)))
                                 trd :start (first rez)))
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

(defparameter *sig* '("GQ010" ;; Мощность активная
		      "FA016" ;; ГТ задание   РК
                      "FA010" ;; ГТ положение РК
                      "G1"    ;; ГТ расход по РК
                      "FQ010" ;; ГТ расход по Расходомеру
		      "FA026" ;; ДТ задание   РК
                      "FA020" ;; ДТ положение РК
                      "G2"    ;; ДТ расход по Расходомеру
                      "FQ110" ;; ДТ расход по Расходомеру
   		      "EN1"   ;;  
                      "EN2"   ;;
                      "EN3"   ;;
		      "T04"
                      "T04max"
                      "T04min"
                      "Oil2Gas" 
                      "Gas2Oil"
                      "FK010" ;; ГТ СК открыт
                      "FK020" ;; ГТ СК закрыт
                      "FK200" ;; ДТ СК открыт                      
                      "FK201" ;; ДТ СК закрыт
                      "FK400" ;; ДТ СК FH40 открыт
                      "FK401" ;; ДТ СК FH40 закрыт
		      "FK250" "FK251" "FK260" "FK261" "FK270" "FK271"
		      "FK280" "FK281" "FK290" "FK291" "FK300" "FK301" "FK310" "FK311"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (require :recoder/dir)

(defparameter *trd-CPIPES-dir*
  (make-instance 'recoder/dir:<trd-dir> :directory "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))
;;;; d:/PRG/msys32/home/namatv

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

;;;;;;;;;;;

(defparameter *trd*
  (make-instance '<trd-seq>
                 :trd-file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
                 :s-sig *sig*))

(elt-named *trd* (+ 33479 (* 5 21)))

(defmethod elt-named ((trd-seq <trd-seq>) index)
  (format t "~{~{~A~10T~6,1F~}~%~}"
  (mapcar #'list
          (recoder:<trd-seq>-s-sig trd-seq) (coerce (elt trd-seq index) 'list))))
                 

;;;;;;;;;;;


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
