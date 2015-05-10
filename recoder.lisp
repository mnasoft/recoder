;;;; recoder.lisp

(in-package #:recoder)

;;; "recoder" goes here. Hacks and glory await!

(defun recode-string (bufer &optional (start 0) (len (length bufer)) &key (break-nul T))
  (do*
   ( (i start (1+ i))
     (ch (gethash (nth i bufer) *cp1251*) (gethash (nth i bufer) *cp1251*))
     (str-rez ""))
   ( (or (>= i (+ start len))
	 (and break-nul (= 0 (nth i bufer)))) 
    str-rez)
    (setf str-rez (concatenate 'string str-rez ch))))

(defun open-trd-file(path)
  "Выполняет открытие файла тренда"
  (open path :element-type 'unsigned-byte))

(defun read-trd-file(in byte-number)
  "Выполняет чтение bite-number из потока in"
  (let ((lst nil))
    (dotimes (i byte-number)
      (push (read-byte in) lst))
    (reverse lst)))

(defun list-to-int(list-of-int &optional (len (length list-of-int )))
  "Выполняет преобразование списка целых чисел 
находящихся в диапазоне 0 - 255 в целое число"
  (let ((rez 0))
    (dotimes (i len rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (nth i list-of-int)))))

(defun read-trd-file-short(in &optional (len 2))
  "Выполняет чтение short из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-int(in &optional (len 4))
  "Выполняет чтение int из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-long(in &optional (len 4))
  "Выполняет чтение long из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-long-long(in &optional (len 8)))
  "Выполняет чтение long-long из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-float(in &optional (len 4))
  "Выполняет чтение float из потока in"
    (ie3fp:decode-ieee-float(list-to-int(read-trd-file in len))))

(defun read-trd-file-double(in &optional (len 8))
  "Выполняет чтение doudle из потока in"
  (ie3fp:decode-ieee-double (list-to-int(read-trd-file in len))))

(defun read-trd-file-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
  (ie3fp:decode-ieee-quad (list-to-int(read-trd-file in len))))


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

;;;;(defparameter in (open-trd-file #p"/home/namatv/MyDoc/git/clisp/recoder/230415_191202.trd"))
;;;;(defparameter in (open-trd-file #p"/home/namatv/1.txt"))

(progn
  (defparameter in
    (open-trd-file #p"/home/namatv/MyDoc/git/clisp/recoder/230415_191202.trd"))
  (defparameter analog-signal-list(read-trend-hdr in))
  (close in))


