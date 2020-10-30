;; /src/dir/test.lisp

(in-package :recoder/dir)

(defgeneric split-on-intervals-when-flag-is-on (trd d-signal-str)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(split-on-intervals-when-flag-is-on)"))

(defmethod split-on-intervals-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str )
  (let ((trd (make-instance 'recoder:<trd>)))
    (apply #'append
	   (mapcar
	    #'(lambda (el)
		(let ((rez nil))
		  (recoder:trd-close trd)
		  (setf (recoder:trd-file-name trd) el)
		  (recoder:trd-open trd)
		  (setf rez
			(recoder:split-on-intervals-when-flag-is-on trd d-signal-str))
		  rez))
	    (mnas-path:find-filename (<dir>-directory trd-dir) "trd")))))

(defmethod split-on-utime-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str )
  (let ((trd (make-instance 'recoder:<trd>)))
    (apply #'append
	   (mapcar
	    #'(lambda (el)
		(let ((rez nil))
		  (recoder:trd-close trd)
		  (setf (recoder:trd-file-name trd) el)
		  (recoder:trd-open trd)
		  (setf rez
			(recoder:split-on-utime-when-flag-is-on trd d-signal-str))
		  rez))
	    (mnas-path:find-filename (<dir>-directory trd-dir) "trd")))))

;;;; 

(split-on-intervals-of-time-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas")

(split-on-utime-when-flag-is-on  *trd-CPIPES-dir* "Oil2Gas")

(trd-delta-time trd)

(mnas-path:find-filename (<dir>-directory *trd-CPIPES-dir*) "trd")

(defparameter *trd* (make-instance
		     'recoder:<trd>
		     :trd-file-name
		     "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"))

(recoder:trd-open *trd*)

(split-on-intervals-of-time-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas")

(split-on-utime-when-flag-is-on *trd* "Oil2Gas")

(trd-utime-by-record-number *trd* 0)

(decode-universal-time (recoder:trd-utime-by-record-number *trd* 0))



"Oil2Gas" 447 "Oil2Gas" "Переход на газ. топливо"
"Gas2Oil" 448 "Gas2Oil" "Переход на жидкое топливо"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-C100-dir*
  (make-instance '<trd-tc-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-C100"))

(defparameter *trd-CPIPES-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))


(defparameter *trd-NIO-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-NIO"))

(analog-signals *trd-NIO-dir* `(,(encode-universal-time 07 12 14 29 07 2020)) nil )

(let ((ut-s `(,(encode-universal-time 07 12 14 29 07 2020))))
  (analog-table ut-s
		`(,*trd-CPIPES-dir* ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-C100-dir*   ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-NIO-dir*    ,(append (loop :for i :from 1 :to 76 :collect (format nil "T15_~2,'0D" i)) '("CF104" "CF140")))
		))

(recoder:trd-split-on-intervals-of-time-when-flag-is-on trd d-signal-str) 
(recoder:trd-split-on-intervals-by-condition trd start-signal-str-lst end-signal-str-lst)
(recoder:split-on-intervals-when-flag-is-on)


   



