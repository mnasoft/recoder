;; ./src/dir/test.lisp

(in-package :recoder)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun utime-dtime-signals (trd records signals comments)
  "@b(Описание:) функция @b(utime-dtime-signals)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (foo-Oil2Gas *trd* '(33479 33755))
@end(code)
"
  (let ((dt (coerce (trd-delta-time trd) 'single-float))
        (first-rec (first records)))
    (mapcar #'(lambda (start end comment)
	        (apply #'append (cons
                                 (append (mnas-org-mode:utime->date-time (trd-utime-by-record-number trd first-rec))
                                         (list (* (- end first-rec) dt) comment (* (- end start) dt)))
			         (mapcar #'math/stat:aver-dmax-dmin
				         (analogs-in-records 
				          trd  start end
				          (trd-analog-signal-list trd signals)))))) 
            records (cdr records) comments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo-Gas2Oil (trd interval)
  (let ((rez nil)
        (comments nil)
	(next-find nil))
    (push (first interval) rez)
    (block перекладка-клапанов
      (setf next-find
            (position-if
             #'(lambda (el) (and (> (sig  "FA010" el trd) 0.5)
                                 (> (sig  "FA016" el trd) 0.5)
                                 (sig-on  "FK200" el trd)
                                 (sig-off "FK201" el trd)
                                 (> (sig  "G2" el trd) 150.0)))
             trd :start (first rez)))
      (when next-find (push next-find rez) (push "Перекладка клапанов" comments)))
    (block замещение-топлива
      (setf next-find
            (position-if
             #'(lambda (el)
                 (and
                  (< (sig "FA016" el trd) 0.5)
                  (< (sig "FA010" el trd) 0.5)
                  (sig-off "FK010" el trd)
                  (sig-on  "FK020" el trd)))
             trd :start (first rez)))
      (when next-find (push next-find rez) (push "Замещение топлива" comments)))
    (block подготовка-к-продувке
      (setf next-find (position-if #'(lambda (el) (and (sig-on "FK271" el trd))) trd :start (first rez)))
      (when next-find (push next-find rez) (push "Подготовка к продувке" comments)))
    (block продувка
      (setf next-find (position-if
		       #'(lambda (el)
			   (and (sig-on "FK271" el trd)
			        (sig-on "FK250" el trd)
			        (sig-on "FK260" el trd)))
                       trd :start (first rez)))
      (when next-find (push next-find rez) (push "Продувка" comments)))

    (setf rez (nreverse rez) comments (nreverse comments))
    (utime-dtime-signals trd rez '("GQ010" "EN2") comments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun foo-Oil2Gas (trd interval)
  "Переход с ДТ на ГТ

 @b(Пример использования:)
@begin[lang=lisp](code)
 (elt-named *trd* (+ 33479 (* 5 21)))
@end(code)"
  (let ((rez nil)
        (comments nil)
        (next-find nil))
    (push (first interval) rez)
    (block перекладка-клапанов
      (setf next-find (position-if #'(lambda (el)
                                       (and
                                        (> (sig  "FA010" el trd) 0.5)
                                        (> (sig  "FA016" el trd) 0.5)
                                        (sig-on  "FK010" el trd)
                                        (sig-off "FK020" el trd)
                                        (> (sig  "G1"    el trd) 150.0)))
                                   trd :start (first rez)))
      (when next-find (push next-find rez) (push "Перекладка клапанов" comments)))
    (block замещение-топлива
      (setf next-find (position-if #'(lambda (el)
                                       (and (< (sig "FA026" el trd) 0.5)
                                            (< (sig "FA020" el trd) 0.5)
                                            (sig-off "FK200" el trd)
                                            (sig-on  "FK201" el trd)))
                                   trd :start (first rez)))
      (when next-find (push next-find rez) (push "Замещение топлива" comments)))
    (block подготовка-к-продувке
      (setf next-find (position-if #'(lambda (el)
                                       (sig-on "FK301" el trd))
                                   trd :start (first rez)))
      (when next-find (push next-find rez) (push "Подготовка к продувке" comments)))
    (block продувка
      (setf next-find (position-if #'(lambda (el)
                                       (and
                                        (sig-on "FK301" el trd)
                                        (sig-on "FK280" el trd)
                                        (sig-on "FK290" el trd)
                                        (sig-on "FK310" el trd)))
                                   trd :start (first rez)))
      (when next-find (push next-find rez) (push "Продувка" comments)))
    (setf rez (nreverse rez) comments (nreverse comments))
    (utime-dtime-signals trd rez '("GQ010" "EN2") comments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	 (apply #'append
		(mapcar
		 #'(lambda (t-i)
		     (let ((trd-seq (make-instance 'recoder:<trd-seq> :trd-file-name (first t-i) :s-sig *sig*)))
		       (trd-open trd-seq)
		       (mapcar #'(lambda (int) (foo-Oil2Gas trd-seq int)) (second t-i))))
		 *i-t-Oil2Gas*))))

*per-foo-Oil2Gas*

;;;;;;;;;;;

(defparameter *trd-sig* (make-instance 'recoder:<trd-seq>
	       :trd-file-name "D:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_151019.trd"
	       :s-sig *sig*))
(trd-open *trd-sig*)
(foo-Gas2Oil *trd-sig* '(57176 57699))
;;;;;;;;;;;

(defparameter *i-t-Gas2Oil* (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Gas2Oil"))

(defparameter *per-foo-Gas2Oil* (apply #'append
				       (apply #'append
					      (mapcar
					       #'(lambda (t-i)
						   (let ((trd-seq (make-instance 'recoder:<trd-seq> :trd-file-name (first t-i) :s-sig *sig*)))
						     (trd-open trd-seq)
						     (mapcar #'(lambda (int) (foo-Gas2Oil trd-seq int)) (second t-i))))
					       *i-t-Gas2Oil*))))

*per-foo-Gas2Oil*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod elt-named ((trd-seq <trd-seq>) index)
  (format t "~{~{~A~10T~6,1F~}~%~}"
  (mapcar #'list
          (recoder:<trd-seq>-s-sig trd-seq) (coerce (elt trd-seq index) 'list))))

(defparameter *trd*
  (make-instance '<trd-seq>
                 :trd-file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
                 :s-sig *sig*))

(elt-named *trd* (+ 33479 (* 5 21)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-signals (fname signals)
  (let ((trd-seq (make-instance '<trd-seq> :trd-file-name fname :s-sig signals)))
    (trd-open trd-seq)
    (with-open-file (os (concatenate 'string (trd-file-name trd-seq) ".csv")
			:direction :output :if-exists :supersede
			:external-format :cp1251)
      (format os "Time;NUM;~{~,4F~^;~}~%" (<trd-seq>-s-sig trd-seq))
      (format os "~{~,S~^;~}~%" (append '("hh:mm:ss" "NUM") (<trd-seq>-units trd-seq)))
      (loop :for i :from 0 :below (trd-total-records trd-seq) :by 5
	    :do (format os "~S;~A;~{~,4F~^;~}~%"
			(mnas-org-mode:utime->time (trd-utime-by-record-number trd-seq i))
			i
			(coerce (elt trd-seq i) 'list))))))


(defparameter *pulsation-template* '("EN1" "EN2" "EN3"  "EB060" "EB120" "EB130" "EB090" "T04" "Na"))
  
(extract-signals "~/org/troynich/20200907_090415.trd" *pulsation-template*)

(extract-signals "~/org/troynich/20200907_133300.trd" *pulsation-template*)

		 
