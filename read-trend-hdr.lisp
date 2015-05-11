;;;; read-trend-hdr.lisp

(in-package #:recoder)

(defun read-trend-hdr (in &optional (byte-number 30))
  (let ((bufer nil)   ;; Буфер для чтения данных
	(analog nil)  ;; Аналоговые сигналы
	(discret nil) ;; Дискретные сигналы
	(head-wid 30)
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
	(analog-id nil)
	(analog-description nil)
	(analog-units  nil)
	(analog-LowLimit nil)
	(analog-HighLimit nil)
  	(discret-id nil)
	(discret-description nil)
	(analog-descriptor-list nil)
	(discret-descriptor-list nil)
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
    (format t "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A" reserv total-records delta-time analog-number discret-number)
    analog-descriptor-list))
