;;;; read-trend-hdr.lisp

(in-package #:recoder)

(defun read-trend-hdr (in)
  (let ((bufer nil)		     ;; Буфер для чтения данных
	(analog nil)		     ;; Количество аналоговых сигналов
	(discret nil)		     ;; Количество дискретных сигналов
	(head-wid 30)		     ;;
	(head-id-wid 5)		     ;; Строка идентификации
	(head-version-wid 1)	     ;; Версия данных тренда
	(head-date-wid 3)	     ;; День Месяц Год-2000
	(head-time-wid 3)	     ;; Час Минута Секунда
	(id nil)		     ;; Строка идентификации
	(version nil)		     ;; Версия данных трендера
	(date-day nil)		     ;; День
	(date-month nil)	     ;; Месяц 
	(date-year nil)		     ;; Год-2000;
	(time-hour nil)		     ;; Час
	(time-minute nil)	     ;; Минута
	(time-second nil)	     ;; Секунда
	(reserv nil)		     ;; Резерв
	(total-records nil)	     ;; Общее число записей
	(delta-time nil)	     ;; Интервал времени
	(analog-number nil)	     ;; Количество аналоговых сигналов
        (discret-number nil)	     ;; Количество дискретных сигналов
	(analog-id-wid 10)	     ;; char[10]
	(analog-description-wid 40)  ;; char[40]
	(analog-units-wid  8)	     ;; char[8]
	(analog-LowLimit-wid 8)	     ;; double
	(analog-HighLimit-wid 8)     ;; double
  	(discret-id-wid 10)	     ;; char[10]
	(discret-description-wid 40) ;; char[40]
	(analog-id nil)		     ;;
	(analog-description nil)     ;;
	(analog-units  nil)	     ;;
	(analog-LowLimit nil)	     ;;
	(analog-HighLimit nil)	     ;;
  	(discret-id nil)	     ;;
	(discret-description nil)    ;;
	(analog-descriptor-list nil) ;;
	(discret-descriptor-list nil) ;;
	(record nil)		      ;;
	(record-list nil)	      ;;
	)
    (setf
     id (recode-string (read-trd-file in head-id-wid))
     version  (car(read-trd-file in head-version-wid))
     bufer (read-trd-file in head-date-wid)
     date-day (first bufer)
     date-month (second bufer)
     date-year (+ 2000 (third bufer))
     bufer (read-trd-file in head-time-wid)
     time-hour (first bufer)
     time-minute (second bufer)
     time-second (third bufer)
     reserv (read-trd-file-short in)
     total-records (read-trd-file-long in)
     delta-time (read-trd-file-double in)
     analog-number (read-trd-file-short in)
     discret-number (read-trd-file-short in)
     )
    (dotimes (i analog-number (setf analog-descriptor-list (nreverse analog-descriptor-list)))
      (setf analog-id (recode-string (read-trd-file in analog-id-wid))
	    analog-description (recode-string (read-trd-file in analog-description-wid))
	    analog-units  (recode-string (read-trd-file in analog-units-wid))
	    analog-LowLimit (read-trd-file-double in)
	    analog-HighLimit (read-trd-file-double in))
      (push (list analog-id
		  analog-description
		  analog-units
		  analog-LowLimit
		  analog-HighLimit) analog-descriptor-list))
    (dotimes (i discret-number (setf discret-descriptor-list (nreverse discret-descriptor-list)))
      (setf discret-id (recode-string (read-trd-file in discret-id-wid))
	    discret-description (recode-string (read-trd-file in discret-description-wid)))
      (push (list discret-id
		  discret-description
		  ) discret-descriptor-list))
    (format t "~S ~A ~S-~S-~S_~S:~S:~S "
	    id version date-year date-month date-day time-hour time-minute time-second)
    (format t "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    reserv total-records delta-time analog-number discret-number)
    (list
     analog-descriptor-list discret-descriptor-list)))


(defun read-trend-header (in &optional (byte-number 30))
  (let ((bufer nil)	     ;; Буфер для чтения данных
	(analog nil)	     ;; Количество аналоговых сигналов
	(discret nil)	     ;; Количество дискретных сигналов
	(head-wid 30)	     ;;
	(head-id-wid 5)	     ;; Строка идентификации
	(head-version-wid 1) ;; Версия данных тренда
	(head-date-wid 3)    ;; День Месяц Год-2000
	(head-time-wid 3)    ;; Час Минута Секунда
	(id nil)	     ;; Строка идентификации
	(version nil)	     ;; Версия данных трендера
	(date-day nil)	     ;; День
	(date-month nil)     ;; Месяц 
	(date-year nil)	     ;; Год-2000;
	(time-hour nil)	     ;; Час
	(time-minute nil)    ;; Минута
	(time-second nil)    ;; Секунда
	(reserv nil)	     ;; Резерв
	(total-records nil)  ;; Общее число записей
	(delta-time nil)     ;; Интервал времени
	(analog-number nil)  ;; Количество аналоговых сигналов
        (discret-number nil) ;; Количество дискретных сигналов
	)
    (setf
     id (recode-string (read-trd-file in head-id-wid))
     version  (car(read-trd-file in head-version-wid))
     bufer (read-trd-file in head-date-wid)
     date-day (first bufer)
     date-month (second bufer)
     date-year (+ 2000 (third bufer))
     bufer (read-trd-file in head-time-wid)
     time-hour (first bufer)
     time-minute (second bufer)
     time-second (third bufer)
     reserv (read-trd-file-short in)
     total-records (read-trd-file-long in)
     delta-time (read-trd-file-double in)
     analog-number (read-trd-file-short in)
     discret-number (read-trd-file-short in)
     )
    (format t "id=~S~%version=~A~%date=~S-~S-~S~%time=~S:~S:~S"
	    id version date-year date-month date-day time-hour time-minute time-second)
    (format t "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    reserv total-records delta-time analog-number discret-number)
    (list id version
	  date-day date-month date-year 
	  time-hour time-minute time-second
	  reserv total-records
	  delta-time analog-number discret-number)))

(defun read-trend-analog-descriptor-list (in analog-number)
  (let (
	(analog-id-wid 10)	     ;; char[10]
	(analog-description-wid 40)  ;; char[40]
	(analog-units-wid  8)	     ;; char[8]
	(analog-LowLimit-wid 8)	     ;; double
	(analog-HighLimit-wid 8)     ;; double
	(analog-id nil)		     ;;
	(analog-description nil)     ;;
	(analog-units  nil)	     ;;
	(analog-LowLimit nil)	     ;;
	(analog-HighLimit nil)	     ;;
	(analog-descriptor-list nil)
	)
    (dotimes (i analog-number (nreverse analog-descriptor-list))
      (setf analog-id (recode-string (read-trd-file in analog-id-wid))
	    analog-description (recode-string (read-trd-file in analog-description-wid))
	    analog-units  (recode-string (read-trd-file in analog-units-wid))
	    analog-LowLimit (read-trd-file-double in)
	    analog-HighLimit (read-trd-file-double in))
      (push (list analog-id
		  analog-description
		  analog-units
		  analog-LowLimit
		  analog-HighLimit)
	    analog-descriptor-list))))

(defun read-trend-discret-descriptor-list (in discret-number)
  (let (
  	(discret-id-wid 10)	     ;; char[10]
	(discret-description-wid 40) ;; char[40]
  	(discret-id nil)	     ;;
	(discret-description nil)    ;;
	(discret-descriptor-list nil) ;;
	)
    (dotimes (i discret-number (nreverse discret-descriptor-list))
      (setf discret-id (recode-string (read-trd-file in discret-id-wid))
	    discret-description (recode-string (read-trd-file in discret-description-wid)))
      (push (list discret-id
		  discret-description
		  )
	    discret-descriptor-list))))

(defun read-trend-analog-record (in analog-number analog-descriptor-list)
  (let ((analog-tmp nil)
	(analog-record nil)
	(lst analog-descriptor-list))
    (dotimes (i analog-number (values (nreverse analog-record) analog-number t))
      (let* ((a-descr (car lst))
	     (low-b (fourth a-descr))
	     (up-b  (fifth  a-descr)))
	(multiple-value-bind (rez n file-status)
	    (read-trd-file-short in)
	  (if file-status
	      (progn
		(setf analog-tmp (+ low-b (* (- up-b low-b) (/ rez 65536)))
		      lst (cdr lst))
		(push analog-tmp analog-record))
	      (return (values analog-record i nil))))))))

(defun int-bit-array (int n-signal)
  (let ((b (make-array (* 8 (ceiling (/ n-signal 8))) :element-type 'bit)))
  (dotimes (i n-signal b)
    (setf (bit b i) (ldb (byte 1 (- n-signal 1 i)) int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-trend-discret-record (in discret-number discret-descriptor-list)
  (let ((discret-byte (ceiling (/ discret-number 8)))
	(*b* (make-array (* 8 (ceiling (/ discret-number 8))) :element-type 'bit))
	(discret-tmp nil)
	(discret-record nil)
	(lst discret-descriptor-list))
    (multiple-value-bind (rez n file-status) (read-trd-file in discret-byte)
      (if file-status
	  (values
	   (int-bit-array (list-to-int rez) discret-number)
	   discret-number
	   t)
	  ;;(values (digits(list-to-int rez) 2) discret-number t)
	  (values 0 n nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-trend-analog-record-list (in analog-number analog-descriptor-list)
  (do ((analog-lst nil)
       (exit nil))
      (exit (nreverse analog-lst))
    (multiple-value-bind (rez n file-status) (read-trend-analog-record in analog-number analog-descriptor-list)
      (if file-status (push rez analog-lst) (setf exit t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(format nil "~F" 1.d0)

;(progn
;  (defparameter fname "/home/namatv/My/git/Trends/тренды для 11 отдела/20150409_144519.trd")
;  ;;"/home/namatv/My/git/Trends/ДМ80№1/090415_150604.trd"
;  ;;"/home/namatv/My/git/Trends/ДМ80№1/230415_191202.trd"
;  (defparameter in (open-trd-file-read fname))
;  (defparameter header (read-trend-header in))
;  (defparameter analog-number (nth 11 header))
;  (defparameter discret-number (nth 12 header))
;  (defparameter analog-descriptor-list (read-trend-analog-descriptor-list in analog-number))
;  (defparameter discret-descriptor-list (read-trend-discret-descriptor-list in discret-number)))

;(progn 
;  (defparameter analog-rec (read-trend-analog-record in analog-number analog-descriptor-list))
;  (defparameter discret-rec (read-trend-discret-record in discret-number discret-descriptor-list))
;  discret-rec)

;;;;(length analog-number-list)

;(close in)
