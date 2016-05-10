;;;; classes.lisp

(in-package #:recoder)

(defclass trend ()
  ((file-name
    :initarg :file-name
    :initform ""
    :accessor file-name
    :documentation "Trend file name")
   (file-descriptor
    :initform nil 
    :accessor file-descriptor
    :documentation "Descriptor of opened trend file")
   (is-file-opened
    :initform nil
    :accessor is-file-opened
    :documentation "nil - if trend file is not opened
T - if trend file is opened")
   (id
    :initform nil
    :accessor id
    :documentation "Строка идентификации")
   (version 
    :initform nil
    :accessor version
    :documentation "Версия данных трендера")
   (date-day 
    :initform nil
    :accessor date-day
    :documentation "День")
   (date-month 
    :initform nil
    :accessor date-month
    :documentation "Месяц")
   (date-year 
    :initform nil
    :accessor date-year
    :documentation "Год-2000")
   (time-hour 
    :initform nil
    :accessor time-hour
    :documentation "Час")
   (time-minute 
    :initform nil
    :accessor time-minute
    :documentation "Минута")
   (time-second 
    :initform nil
    :accessor time-second
    :documentation "Секунда")
   (reserv 
    :initform nil
    :accessor reserv
    :documentation "Резерв")
   (total-records 
    :initform nil
    :accessor total-records
    :documentation "Общее число записей")
   (delta-time
    :initform 1
    :accessor delta-time
    :documentation "Интервал времени")
   (analog-number
    :initform 0
    :accessor analog-number
    :documentation "Количество аналоговых сигналов")
   (discret-number
    :initform 0
    :accessor discret-number
    :documentation "Количество дискретных сигналов")
   (analog-descriptors
    :initform nil
    :accessor analog-descriptors
    :documentation "List of descriptors of analog signals wich is into trend")
   (discret-descriptors
    :initform nil
    :accessor discret-descriptors
    :documentation "List of descriptors of discret signals wich is into trend")))

(defmethod print-object :before ((x trend) s) (format s "#trend("))

(defmethod print-object  ((x trend) s) 
  (format s "id=~S~%version=~A~%date=~S-~S-~S~%time=~S:~S:~S"
	  (id x) (version x) (date-year x) (date-month x) (date-day x) (time-hour x) (time-minute x) (time-second x))
  (format s "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	  (reserv x) (total-records x) (delta-time x) (analog-number x) (discret-number x))
  (format s "~%f-n=~S is-file-opened=~S" (file-name x) (is-file-opened x)))

(defmethod print-object :after ((x trend) s) (format s ")"))

(defmethod open-trd-file ((tr trend))
  "Выполняет открытие файла-тренда для чтения"
  (if (not (is-file-opened tr))
      (progn
	(setf (file-descriptor tr) (open-trd-file-read (file-name tr))
	      (is-file-opened tr) t)
	(format nil "~A" (file-descriptor tr)))
      (format nil "Trend file is alredy opened")))

(defmethod close-trd-file ((tr trend))
  "Выполняет закрытие файла-тренда"
  (if (is-file-opened tr)
      (progn
	(close (file-descriptor tr))
	(setf (file-descriptor tr) nil
	       (is-file-opened tr) nil)
	(format nil "Trend file ~A is closed" (file-name tr)))
      (format nil "Trend file ~A is alredy closed" (file-name tr))))
    
(defmethod read-header ((tr trend))
  (let ((bufer nil)	     ;; Буфер для чтения данных
	(head-wid 30)	     ;; Полная  длина заголовка файла-тренда
	(head-id-wid 5)	     ;; Длина строка-идентификатора Trend
	(head-version-wid 1) ;; Версия данных тренда
	(head-date-wid 3)    ;; День Месяц Год-2000
	(head-time-wid 3)    ;; Час Минута Секунда
	)
    (setf
     (id tr) (recode-string (read-trd-file  (file-descriptor tr) head-id-wid))
     (version tr)  (car(read-trd-file  (file-descriptor tr) head-version-wid))
     bufer (read-trd-file  (file-descriptor tr) head-date-wid)
     (date-day tr) (first bufer)
     (date-month tr) (second bufer)
     (date-year tr) (+ 2000 (third bufer))
     bufer (read-trd-file  (file-descriptor tr) head-time-wid)
     (time-hour tr) (first bufer)
     (time-minute tr) (second bufer)
     (time-second tr) (third bufer)
     (reserv tr) (read-trd-file-short (file-descriptor tr))
     (total-records tr) (read-trd-file-long (file-descriptor tr))
     (delta-time tr) (read-trd-file-double (file-descriptor tr))
     (analog-number tr) (read-trd-file-short (file-descriptor tr))
     (discret-number tr) (read-trd-file-short (file-descriptor tr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass analog-hdr ()
  ((analog-id-wid          :allocation :class  :initform 10  :reader analog-id-wid)
   (analog-description-wid :allocation :class  :initform 40  :reader analog-description-wid)
   (analog-units-wid       :allocation :class  :initform  8  :reader analog-units-wid)
   (analog-LowLimit-wid    :allocation :class  :initform  8  :reader analog-LowLimit-wid)
   (analog-HighLimit-wid   :allocation :class  :initform  8  :reader analog-HighLimit-wid)
   (analog-id              :initform nil  :accessor analog-id
			   :documentation "Обозначение аналогового сигнала; char[10]")
   (analog-description     :initform nil  :accessor analog-description
			   :documentation "Описание аналогового сигнала; char[40]")
   (analog-units           :initform nil  :accessor analog-units
			   :documentation "Размернсть аналогового сигнала; char[8]")
   (analog-LowLimit        :initform nil  :accessor analog-LowLimit
			   :documentation "Нижняя граница аналогового сигнала; double")
   (analog-HighLimit       :initform nil  :accessor analog-HighLimit
			   :documentation "Верхняя граница аналогового сигнала; double")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod read-analog-descriptor-list ((tr trend))
  (let (
	(analog-descriptor-list nil)
	(a-hdr nil)
	)
    (dotimes (i (analog-number tr) (nreverse analog-descriptor-list))
      (setf a-hdr (make-instance 'analog-hdr)
	    (analog-id a-hdr) (recode-string
			       (read-trd-file
				(file-descriptor tr)
				(analog-id-wid a-hdr)))
	    (analog-description a-hdr) (recode-string
					(read-trd-file
					 (file-descriptor tr)
					 (analog-description-wid a-hdr)))
	    (analog-units a-hdr)  (recode-string
				   (read-trd-file
				    (file-descriptor tr)
				    (analog-units-wid a-hdr)))
	    (analog-LowLimit a-hdr) (read-trd-file-double (file-descriptor tr))
	    (analog-HighLimit a-hdr) (read-trd-file-double (file-descriptor tr)))
      (push a-hdr analog-descriptor-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass discret-hdr ()
  ((discret-id-wid          :allocation :class  :initform 10  :reader discret-id-wid)
   (discret-description-wid :allocation :class  :initform 40  :reader discret-description-wid)
   (discret-id              :initform nil  :accessor discret-id
			   :documentation "Обозначение аналогового сигнала; char[10]")
   (discret-description     :initform nil  :accessor discret-description
			   :documentation "Описание аналогового сигнала; char[40]")))

(defun read-trend-discret-descriptor-list (in discret-number)
  (let ((discret-id-wid 10)	     ;; char[10]
	(discret-description-wid 40) ;; char[40]
  	(discret-id nil)	     ;;
	(discret-description nil)    ;;
	(discret-descriptor-list nil))
    (dotimes (i discret-number (nreverse discret-descriptor-list))
      (setf discret-id (recode-string (read-trd-file (file-descriptor tr) discret-id-wid))
	    discret-description (recode-string (read-trd-file in discret-description-wid)))
      (push (list discret-id
		  discret-description
		  )
	    discret-descriptor-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;

(defparameter *tr* (make-instance 'trend :file-name "/home/namatv/1.txt"))

;;;;(open-trd-file *tr*)

;;;;(is-file-opened *tr*)

;;;;(close-trd-file *tr*)

;;;;(file-descriptor *tr*)

(defparameter *2* (open "/home/namatv/2.txt" :direction :output :if-exists :supersede) )

(do
 ((i 0 (1+ i)))
 ((>= i 1000) i)
  (format nil "~A~%" i))
(format *2* "COOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOL" )
(close *2*)
