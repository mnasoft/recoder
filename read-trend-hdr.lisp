;;;; read-trend-hdr.lisp

(in-package #:recoder)

(progn
  (defparameter *head-id-wid*              5 "Строка идентификации файла тренда")
  (defparameter *head-version-wid*         1 "Версия данных тренда")
  (defparameter *head-date-wid*            3 "День Месяц Год-2000")
  (defparameter *head-time-wid*            3 "Час Минута Секунда")
  (defparameter *signal-id-wid*           10 "Длина строки обозначения сигнала")
  (defparameter *signal-description-wid*  40 "Длина строки описания сигнала")
  (defparameter *signal-units-wid*         8 "Длина строки размерности аналогового сигнала")
  (defparameter *signal-LowLimit-wid*      8 "Ширина поля для нижней  границы диапазона аналогового сигнала")
  (defparameter *signal-HighLimit-wid*     8 "Ширина поля для верхней границы диапазона аналогового сигнала")
  (defparameter *head-wid*                30 "Общая длина заголовка")
  (defparameter *analog-wid* (+ *signal-id-wid* *signal-description-wid* *signal-units-wid* *signal-LowLimit-wid* *signal-HighLimit-wid*)
    "Длина заголовка одной записи аналогового сигнала")
  (defparameter *discret-wid* (+ *signal-id-wid* *signal-description-wid* )
    "Длина заголовка одной записи дискретного сигнала")
  (defparameter *ushort-max* (- (expt 2 16) 1))
  )

(defun read-trend-hdr (in)
  (let ((bufer nil)                   ;; Буфер для чтения данных

	(id nil)		      ;; Строка идентификации
	(version nil)		      ;; Версия данных трендера
	(date-day nil)		      ;; День
	(date-month nil)	      ;; Месяц 
	(date-year nil)		      ;; Год-2000;
	(time-hour nil)		      ;; Час
	(time-minute nil)	      ;; Минута
	(time-second nil)	      ;; Секунда
	(reserv nil)		      ;; Резерв
	(total-records nil)	      ;; Общее число записей
	(delta-time nil)	      ;; Интервал времени
	(analog-number nil)	      ;; Количество аналоговых сигналов
        (discret-number nil)	      ;; Количество дискретных сигналов

	(analog-id nil)		      ;;
	(analog-description nil)      ;;
	(analog-units  nil)	      ;;
	(analog-LowLimit nil)	      ;;
	(analog-HighLimit nil)	      ;;
  	(discret-id nil)	      ;;
	(discret-description nil)     ;;
	(analog-descriptor-list nil)  ;;
	(discret-descriptor-list nil) ;;
	)
    (setf
     id             (recode-string (read-trd-file in *head-id-wid*))
     version        (car(read-trd-file in *head-version-wid*))
     bufer          (read-trd-file in *head-date-wid*)
     date-day       (first bufer)
     date-month     (second bufer)
     date-year      (+ 2000 (third bufer))
     bufer          (read-trd-file in *head-time-wid*)
     time-hour      (first bufer)
     time-minute    (second bufer)
     time-second    (third bufer)
     reserv         (read-trd-file-short in)
     total-records  (read-trd-file-long in)
     delta-time     (read-trd-file-double in)
     analog-number  (read-trd-file-short in)
     discret-number (read-trd-file-short in))
    (dotimes (i analog-number (setf analog-descriptor-list (nreverse analog-descriptor-list)))
      (setf analog-id (recode-string (read-trd-file in *signal-id-wid*))
	    analog-description (recode-string (read-trd-file in *signal-description-wid*))
	    analog-units  (recode-string (read-trd-file in *signal-units-wid*))
	    analog-LowLimit (read-trd-file-double in)
	    analog-HighLimit (read-trd-file-double in))
      (push (list analog-id
		  analog-description
		  analog-units
		  analog-LowLimit
		  analog-HighLimit) analog-descriptor-list))
    (dotimes (i discret-number (setf discret-descriptor-list (nreverse discret-descriptor-list)))
      (setf discret-id (recode-string (read-trd-file in *signal-id-wid*))
	    discret-description (recode-string (read-trd-file in *signal-description-wid*)))
      (push (list discret-id
		  discret-description
		  ) discret-descriptor-list))
    (format t "~S ~A ~S-~S-~S_~S:~S:~S "
	    id version date-year date-month date-day time-hour time-minute time-second)
    (format t "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    reserv total-records delta-time analog-number discret-number)
    (list
     analog-descriptor-list discret-descriptor-list)))

(defun read-trend-header (in)
  (let ((bufer nil)	     ;; Буфер для чтения данных

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
     id             (recode-string (read-trd-file in *head-id-wid*))
     version        (car (read-trd-file in *head-version-wid*))
     bufer          (read-trd-file in *head-date-wid*)
     date-day       (first bufer)
     date-month     (second bufer)
     date-year      (+ 2000 (third bufer))
     bufer          (read-trd-file in *head-time-wid*)
     time-hour      (first bufer)
     time-minute    (second bufer)
     time-second    (third bufer)
     reserv         (read-trd-file-short in)
     total-records  (read-trd-file-long in)
     delta-time     (read-trd-file-double in)
     analog-number  (read-trd-file-short in)
     discret-number (read-trd-file-short in))
    (format t "id=~S~%version=~A~%date=~S-~S-~S~%time=~S:~S:~S"
	    id version date-year date-month date-day time-hour time-minute time-second)
    (format t "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    reserv total-records delta-time analog-number discret-number)
    (list id version
	  date-day date-month date-year 
	  time-hour time-minute time-second
	  reserv total-records
	  delta-time analog-number discret-number)))

(defun trend-header-length ()
  "Возвращает длину заготовка тренда"
  30)

(defun read-trend-analog-descriptor-list (in analog-number)
  (let ((analog-id nil)
	(analog-description nil)
	(analog-units  nil)
	(analog-LowLimit nil)
	(analog-HighLimit nil)
	(analog-descriptor-list nil))
    (dotimes (i analog-number (nreverse analog-descriptor-list))
      (setf analog-id          (recode-string (read-trd-file in *signal-id-wid*))
	    analog-description (recode-string (read-trd-file in *signal-description-wid*))
	    analog-units       (recode-string (read-trd-file in *signal-units-wid*))
	    analog-LowLimit    (read-trd-file-double in)
	    analog-HighLimit   (read-trd-file-double in))
      (push (list analog-id
		  analog-description
		  analog-units
		  analog-LowLimit
		  analog-HighLimit)
	    analog-descriptor-list))))

(defun read-trend-discret-descriptor-list (in discret-number)
  (let (
  	(discret-id nil)	     ;;
	(discret-description nil)    ;;
	(discret-descriptor-list nil) ;;
	)
    (dotimes (i discret-number (nreverse discret-descriptor-list))
      (setf discret-id (recode-string (read-trd-file in *signal-id-wid*))
	    discret-description (recode-string (read-trd-file in *signal-description-wid*)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass a-signal ()
  ((a-signal-num         :accessor a-signal-num         :initarg :a-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (a-signal-id          :accessor a-signal-id          :initarg :a-signal-id          :initform nil :documentation "Обозначение сигнала")
   (a-signal-description :accessor a-signal-description :initarg :a-signal-description :initform nil :documentation "Описание сигнала")
   (a-signal-units       :accessor a-signal-units       :initarg :a-signal-units       :initform nil :documentation "Размерность аналогового сигнала")
   (a-signal-min         :accessor a-signal-min         :initarg :a-signal-min         :initform nil :documentation "Нижняя граница диапазона аналогового сигнала")
   (a-signal-max         :accessor a-signal-max         :initarg :a-signal-max         :initform nil :documentation "Верхняя граница диапазона аналогового сигнала"))
  (:documentation "Описание аналогового сигнала"))

(defmethod print-object ((x a-signal) stream)
  (format stream "~S ~S [~A ~A] ~S ~S" (a-signal-num x) (a-signal-id x) (a-signal-min x) (a-signal-max x) (a-signal-units x) (a-signal-description x)))

(defmethod a-signal-value ((x a-signal) ushort-int)
  (+ (a-signal-min x)
     (* (- (a-signal-max x)
	   (a-signal-min x))
	(/ ushort-int *ushort-max*))))

(multiple-value-bind (rez n file-status)
    (read-trd-file-short in)
  (if file-status
      (progn
	(setf analog-tmp (+ low-b (* (- up-b low-b) (/ rez 65536)))
	      lst (cdr lst))
	(push analog-tmp analog-record))
      (return (values analog-record i nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass d-signal ()
  ((d-signal-num         :accessor d-signal-num         :initarg :d-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (d-signal-id          :accessor d-signal-id          :initarg :d-signal-id          :initform nil :documentation "Обозначение сигнала")
   (d-signal-description :accessor d-signal-description :initarg :d-signal-description :initform nil :documentation "Описание сигнала"))
  (:documentation "Описание дискретного сигнала"))

(defmethod print-object ((x d-signal) stream)
  (format stream "~S ~S ~S" (d-signal-num x) (d-signal-id x) (d-signal-description x)))

(defclass trd ()
  ((trd-file-name         :accessor trd-file-name      :initarg :trd-file-name      :initform nil :documentation "Имя файла в файловой системе")
   (trd-file-descr        :accessor trd-file-descr                                  :initform nil :documentation "Файл тренда")
   (trd-id-string         :accessor trd-id-string                                   :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (trd-version           :accessor trd-version                                     :initform nil :documentation "Версия тренда")
   (trd-date-time         :accessor trd-date-time                                   :initform nil :documentation "Дата и время начала создания тренда")
   (trd-reserv            :accessor trd-reserv                                      :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (trd-total-records     :accessor trd-total-records                               :initform nil :documentation "Общее число записей в тренде")
   (trd-delta-time        :accessor trd-delta-time                                  :initform nil :documentation "Интервал между записями тренда")
   (trd-analog-number     :accessor trd-analog-number                               :initform nil :documentation "Количество аналоговых сигналов")
   (trd-discret-number    :accessor trd-discret-number                              :initform nil :documentation "Количество дискретных сигналов")
   (trd-analog-ht         :accessor trd-analog-ht                                   :initform nil :documentation "Список аналоговых сигналов (мб вектор)")
   (trd-discret-ht        :accessor trd-discret-ht                                  :initform nil :documentation "Список дискретных сигналов (мб вектор)"))
  (:documentation ""))

(defmethod trd-open ((x trd))
  "Выполняет открытие файла тренда включая:
- чтение заголовка;
- разбор аналоговых сигналов;
- разбор дискретных сигналов"
  (when (null (trd-file-descr x))
    (setf (trd-file-descr x) (open-trd-file-read (trd-file-name x)))
    (let ((in (trd-file-descr x)) (bufer nil) (date-day nil) (date-month nil) (date-year nil) (time-hour nil) (time-minute nil) (time-second nil)
	  (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil)
	  (discret-id nil) (discret-description nil))
      (setf (trd-id-string x)      (recode-string (read-trd-file in *head-id-wid*))
	    (trd-version x)        (car (read-trd-file in *head-version-wid*))
	    bufer                  (read-trd-file in *head-date-wid*)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (read-trd-file in *head-time-wid*)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (trd-date-time x)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (trd-reserv x)         (read-trd-file-short in)
	    (trd-total-records x)  (read-trd-file-long in)
	    (trd-delta-time x)     (read-trd-file-double in)
	    (trd-analog-number x)  (read-trd-file-short in)
	    (trd-discret-number x) (read-trd-file-short in)
	    (trd-analog-ht x)      (make-hash-table :test #'equal :size (trd-analog-number x))
	    (trd-discret-ht x)     (make-hash-table :test #'equal :size (trd-discret-number x)))
      (setf (trd-total-records x)
	    (/ (- (file-length (trd-file-descr x)) (trd-start-offset x))
	       (trd-record-length *t*)))
      (dotimes (i (trd-analog-number x) 'done)
	(setf analog-id          (recode-string (read-trd-file in *signal-id-wid*))
	      analog-description (recode-string (read-trd-file in *signal-description-wid*))
	      analog-units       (recode-string (read-trd-file in *signal-units-wid*))
	      analog-min         (read-trd-file-double in)
	      analog-max         (read-trd-file-double in)
	      (gethash analog-id (trd-analog-ht x)) (make-instance 'a-signal
								   :a-signal-num i
								   :a-signal-id  analog-id
								   :a-signal-description analog-description
								   :a-signal-units analog-units
								   :a-signal-min analog-min
								   :a-signal-max analog-max)))
      (dotimes (i (trd-discret-number x) 'done)
	(setf discret-id          (recode-string (read-trd-file in *signal-id-wid*))
	      discret-description (recode-string (read-trd-file in *signal-description-wid*))
	      (gethash discret-id (trd-discret-ht x)) (make-instance 'd-signal
								     :d-signal-num i
								     :d-signal-id discret-id
								     :d-signal-description discret-description)))))
  x)

(defmethod trd-start-offset ((x trd))
  "Смещение для первой (нулевой) записи тренда"
    (+ *head-wid*
    (* (trd-analog-number x) *analog-wid*)
    (* (trd-discret-number x) *discret-wid*)))

(defmethod trd-record-length ((x trd))
  "Длина одной записи тренда"
    (+ (* (trd-analog-number x) 2)
       (ceiling (/ (trd-discret-number x) 8))))

(defmethod print-object ((x trd) stream)
  (when (trd-file-descr *t*)
    (format stream "Path= ~S~%" (trd-file-name x) )
    (format stream "id=~S version=~A " (trd-id-string x) (trd-version x))
    (format stream "[ ")
    (mnas-string:print-universal-time (trd-date-time x) stream)
    (format stream " ; ")
    (mnas-string:print-universal-time (trd-date-time-end x) stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (trd-reserv x) (trd-total-records x) (trd-delta-time x) (trd-analog-number x) (trd-discret-number x))))

(defmethod trd-date-time-end ((x trd))
  "Возвращает время окончания тренда. время возвращается в универсальном формате (universal-time)"
  (+ (trd-date-time x)
     (floor (* (trd-total-records x) (trd-delta-time x)))))


(a-signal-value (gethash "EN1" (trd-analog-ht *t*)) (* *ushort-max* 0.0125))

(defmethod trd-get-analog-signal-number-list ( (x trd) rec-number signal-string-list)
  (when (< -1 rec-number (trd-total-records x))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x) (* rec-number (trd-record-length x))))
    (let ((v-sh (make-array (trd-analog-number x) :element-type 'integer))
	  (v-fl (make-array (trd-analog-number x) :element-type 'float)))
      (dotimes (i (trd-analog-number x) 'done)
	(setf (svref v-sh i)
	      (read-trd-file-short (trd-file-descr x))))
      v-sh)))

(trd-get-analog-signal-number-list *t* 18 '("EN1"))


(* 172.8 5)

(trd-total-records *t*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (defparameter *t* (make-instance 'trd :trd-file-name "~/develop/TRD/20170111_161429.trd"))

;;;; (trd-open *t*)
(file-position (trd-file-descr *t*))

(trd-start-offset *t*)
(trd-record-length *t*)

;;;; (trd-total-records-number *t*)

;;;; (trd-record-length *t*)

;;;; (file-length (trd-file-descr *t*))

;;;; (trd-total-records-number *t*)

;;;; (trd-start-offset *t*)

;;;; (trd-total-records *t*)

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-discret-ht *t*) )

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-analog-ht *t*) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
