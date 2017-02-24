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

(defmethod trd-date-time-end ((x trd))
  "Возвращает время окончания тренда. время возвращается в универсальном формате (universal-time)"
  (+ (trd-date-time x)
     (floor (* (trd-total-records x) (trd-delta-time x)))))

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

(defmethod trd-analog-signal-list ( (x trd) signal-string-list)
  "Возвращает список сигналов тренда trd, 
которые соответствуют списку обозначений сигналов из списка signal-string-list"
  (when  (trd-file-descr *t*)
    (mapcar #'(lambda(el)
		(gethash el (trd-analog-ht x)))
	    signal-string-list)))

(defmethod trd-value-list ( (x trd) rec-number signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (when (and (trd-file-descr *t*) (< -1 rec-number (trd-total-records x)))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x) (* rec-number (trd-record-length x))))
    (let* ((v-sh (make-array (trd-analog-number x) :element-type 'integer)))
      (dotimes (i (trd-analog-number x) 'done)
	(setf (svref v-sh i)
	      (read-trd-file-short (trd-file-descr x))))
      (mapcar #'(lambda(el) (a-signal-value el (svref v-sh (a-signal-num el))))
	      signal-list))))

(defmethod trd-value-record-by-udate ( (x trd) udate)
  "Возвращает номер записи по универсальному времени"
  (floor (- udate (trd-date-time x)) (trd-delta-time x)))

(defmethod trd-value-list-by-udate ( (x trd) udate signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (trd-value-list x
		  (trd-value-record-by-udate x udate)
		  signal-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *t* (make-instance 'trd :trd-file-name "~/develop/TRD/DM80L№1-100-10/NIL-6/20170123_090853.trd"))
  (trd-open *t*))

(trd-value-record-by-udate *t* (+  100 ))

"~/develop/TRD/DM80L№1-100-10/CPIPES/"

(progn (defparameter *GT-3-t-1-8-lst* (trd-analog-signal-list *t* '("TSI_T000" "TSI_T030" "TSI_T031" "TSI_T032" "TSI_T033" "TSI_T001" "TSI_T002" "TSI_T003"))) *GT-3-t-1-8-lst*)

(progn (defparameter *GT-3-t-9-16-lst* (trd-analog-signal-list *t* '("TSI_T004" "TSI_T035" "TSI_T036" "TSI_T037" "TSI_T038" "TSI_T005" "TSI_T006" "TSI_T007"))) *GT-3-t-9-16-lst*)




(progn (defparameter *GT-15-t-1-8-lst* (trd-analog-signal-list *t* '("TSI_T111" "TSI_T112" "TSI_T113" "TSI_T114" "TSI_T080" "TSI_T081" "TSI_T082" "TSI_T115"))) *GT-15-t-1-8-lst*)






(progn
  (defparameter
      *a-s-lst*
    (trd-analog-signal-list
     *t*
     '("GQ010" "EN1" "EN2" "T04" "dT04plus" "dT04minus" "T01" "P02" "FQ110" "FQ010"
       "EB100" "EB110" "EB120"
       "FA010" "FA020"
       )))
  *a-s-lst*)

(trd-value-list-by-udate *t*  (encode-universal-time 04 18 11 23 1 2018) *GT-3-t-9-16-lst*)

(trd-value-list *t* 220 *a-s-lst*)
(close (trd-file-descr *t*))

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-analog-ht *t*) )

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-discret-ht *t*) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (trd-get-analog-signal-number-list *t* 21 '("EN1" "T04mid"))

;;;; (a-signal-value (gethash "EN1" (trd-analog-ht *t*)) (* *ushort-max* 0.0125))

;;;; (trd-total-records *t*)

;;;; (file-position (trd-file-descr *t*))

;;;; (trd-start-offset *t*)

;;;; (trd-record-length *t*)

;;;; (trd-total-records-number *t*)

;;;; (trd-record-length *t*)

;;;; (file-length (trd-file-descr *t*))

;;;; (trd-total-records-number *t*)

;;;; (trd-start-offset *t*)

;;;; (trd-total-records *t*)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 0 "TSI_T000" [0.0d0 1300.0d0] "°C" "3ЖТ_1"
30 "TSI_T030" [0.0d0 1300.0d0] "°C" "3ЖТ_2"
31 "TSI_T031" [0.0d0 1300.0d0] "°C" "3ЖТ_3"
32 "TSI_T032" [0.0d0 1300.0d0] "°C" "3ЖТ_4"
33 "TSI_T033" [0.0d0 1300.0d0] "°C" "3ЖТ_5"
 1 "TSI_T001" [0.0d0 1300.0d0] "°C" "3ЖТ_6"
 2 "TSI_T002" [0.0d0 1300.0d0] "°C" "3ЖТ_7"
 3 "TSI_T003" [0.0d0 1300.0d0] "°C" "3ЖТ_8"
 4 "TSI_T004" [0.0d0 1300.0d0] "°C" "3ЖТ_9"
35 "TSI_T035" [0.0d0 1300.0d0] "°C" "3ЖТ_10"
36 "TSI_T036" [0.0d0 1300.0d0] "°C" "3ЖТ_11"
37 "TSI_T037" [0.0d0 1300.0d0] "°C" "3ЖТ_12"
38 "TSI_T038" [0.0d0 1300.0d0] "°C" "3ЖТ_13"
 5 "TSI_T005" [0.0d0 1300.0d0] "°C" "3ЖТ_14"
 6 "TSI_T006" [0.0d0 1300.0d0] "°C" "3ЖТ_15"
 7 "TSI_T007" [0.0d0 1300.0d0] "°C" "3ЖТ_16"
 8 "TSI_T008" [0.0d0 1300.0d0] "°C" "3ЖТ_17"
39 "TSI_T039" [0.0d0 1300.0d0] "°C" "3ЖТ_18"
40 "TSI_T040" [0.0d0 1300.0d0] "°C" "3ЖТ_19"
41 "TSI_T041" [0.0d0 1300.0d0] "°C" "3ЖТ_20"

42 "TSI_T042" [0.0d0 1300.0d0] "°C" "3ЖТ_21"
43 "TSI_T043" [0.0d0 1300.0d0] "°C" "3ЖТ_21"

 9 "TSI_T009" [0.0d0 1300.0d0] "°C" "3ЖТ_22"
10 "TSI_T010" [0.0d0 1300.0d0] "°C" "3ЖТ_23"
11 "TSI_T011" [0.0d0 1300.0d0] "°C" "3ЖТ_24"
44 "TSI_T044" [0.0d0 1300.0d0] "°C" "3ЖТ_25"
45 "TSI_T045" [0.0d0 1300.0d0] "°C" "3ЖТ_26"

47 "TSI_T047" [0.0d0 1300.0d0] "°C" "3ЖТ_28"
46 "TSI_T046" [0.0d0 1300.0d0] "°C" "3ЖТ_29"
12 "TSI_T012" [0.0d0 1300.0d0] "°C" "3ЖТ_30"
13 "TSI_T013" [0.0d0 1300.0d0] "°C" "3ЖТ_31"
14 "TSI_T014" [0.0d0 1300.0d0] "°C" "3ЖТ_32"
48 "TSI_T048" [0.0d0 1300.0d0] "°C" "3ЖТ_33"
49 "TSI_T049" [0.0d0 1300.0d0] "°C" "3ЖТ_34"
34 "TSI_T034" [0.0d0 1300.0d0] "°C" "3ЖТ_35"

15 "TSI_T015" [0.0d0 1300.0d0] "°C" "3ЖТ_36"
50 "TSI_T050" [0.0d0 1300.0d0] "°C" "3ЖТ_36"

51 "TSI_T051" [0.0d0 1300.0d0] "°C" "3ЖТ_37"
16 "TSI_T016" [0.0d0 1300.0d0] "°C" "3ЖТ_38"
17 "TSI_T017" [0.0d0 1300.0d0] "°C" "3ЖТ_39"
18 "TSI_T018" [0.0d0 1300.0d0] "°C" "3ЖТ_40"
52 "TSI_T052" [0.0d0 1300.0d0] "°C" "3ЖТ_41"
53 "TSI_T053" [0.0d0 1300.0d0] "°C" "3ЖТ_42"
54 "TSI_T054" [0.0d0 1300.0d0] "°C" "3ЖТ_43"
19 "TSI_T019" [0.0d0 1300.0d0] "°C" "3ЖТ_44"
55 "TSI_T055" [0.0d0 1300.0d0] "°C" "3ЖТ_45"
56 "TSI_T056" [0.0d0 1300.0d0] "°C" "3ЖТ_46"
57 "TSI_T057" [0.0d0 1300.0d0] "°C" "3ЖТ_47"
20 "TSI_T020" [0.0d0 1300.0d0] "°C" "3ЖТ_48"
58 "TSI_T058" [0.0d0 1300.0d0] "°C" "3ЖТ_49"
59 "TSI_T059" [0.0d0 1300.0d0] "°C" "3ЖТ_50"
60 "TSI_T060" [0.0d0 1300.0d0] "°C" "3ЖТ_51"
61 "TSI_T061" [0.0d0 1300.0d0] "°C" "3ЖТ_52"
21 "TSI_T021" [0.0d0 1300.0d0] "°C" "3ЖТ_53"
22 "TSI_T022" [0.0d0 1300.0d0] "°C" "3ЖТ_54"
23 "TSI_T023" [0.0d0 1300.0d0] "°C" "3ЖТ_55"
62 "TSI_T062" [0.0d0 1300.0d0] "°C" "3ЖТ_56"
65 "TSI_T065" [0.0d0 1300.0d0] "°C" "3ЖТ_57"
64 "TSI_T064" [0.0d0 1300.0d0] "°C" "3ЖТ_58"
63 "TSI_T063" [0.0d0 1300.0d0] "°C" "3ЖТ_59"
67 "TSI_T067" [0.0d0 1300.0d0] "°C" "3ЖТ_60"
24 "TSI_T024" [0.0d0 1300.0d0] "°C" "3ЖТ_61"
25 "TSI_T025" [0.0d0 1300.0d0] "°C" "3ЖТ_62"
26 "TSI_T026" [0.0d0 1300.0d0] "°C" "3ЖТ_63"
68 "TSI_T068" [0.0d0 1300.0d0] "°C" "3ЖТ_64"
69 "TSI_T069" [0.0d0 1300.0d0] "°C" "3ЖТ_65"
66 "TSI_T066" [0.0d0 1300.0d0] "°C" "3ЖТ_66"
70 "TSI_T070" [0.0d0 1300.0d0] "°C" "3ЖТ_67"
71 "TSI_T071" [0.0d0 1300.0d0] "°C" "3ЖТ_68"

27 "TSI_T027" [0.0d0 1300.0d0] "°C" "3ЖТ_70"
28 "TSI_T028" [0.0d0 1300.0d0] "°C" "3ЖТ_71"
72 "TSI_T072" [0.0d0 1300.0d0] "°C" "3ЖТ_72"
73 "TSI_T073" [0.0d0 1300.0d0] "°C" "3ЖТ_73"
74 "TSI_T074" [0.0d0 1300.0d0] "°C" "3ЖТ_74"
29 "TSI_T029" [0.0d0 1300.0d0] "°C" "3ЖТ_75"
75 "TSI_T075" [0.0d0 1300.0d0] "°C" "3ЖТ_76"












76 "TSI_T076" [0.0d0 1300.0d0] "°C" ""
77 "TSI_T077" [0.0d0 1300.0d0] "°C" ""
78 "TSI_T078" [0.0d0 1300.0d0] "°C" ""
79 "TSI_T079" [0.0d0 1300.0d0] "°C" ""


110 "TSI_T111" [0.0d0 1300.0d0] "°C" "15ЖТ_1"
111 "TSI_T112" [0.0d0 1300.0d0] "°C" "15ЖТ_2"
112 "TSI_T113" [0.0d0 1300.0d0] "°C" "15ЖТ_3"
113 "TSI_T114" [0.0d0 1300.0d0] "°C" "15ЖТ_4"
80 "TSI_T080" [0.0d0 1300.0d0] "°C" "15ЖТ_5"
81 "TSI_T081" [0.0d0 1300.0d0] "°C" "15ЖТ_6"
82 "TSI_T082" [0.0d0 1300.0d0] "°C" "15ЖТ_7"
114 "TSI_T115" [0.0d0 1300.0d0] "°C" "15ЖТ_8"
115 "TSI_T116" [0.0d0 1300.0d0] "°C" "15ЖТ_9"
116 "TSI_T117" [0.0d0 1300.0d0] "°C" "15ЖТ_10"
117 "TSI_T118" [0.0d0 1300.0d0] "°C" "15ЖТ_11"
118 "TSI_T119" [0.0d0 1300.0d0] "°C" "15ЖТ_12"
83 "TSI_T083" [0.0d0 1300.0d0] "°C" "15ЖТ_13"
84 "TSI_T084" [0.0d0 1300.0d0] "°C" "15ЖТ_14"
85 "TSI_T085" [0.0d0 1300.0d0] "°C" "15ЖТ_15"

86 "TSI_T086" [0.0d0 1300.0d0] "°C" "15ЖТ_16"
119 "TSI_T120" [0.0d0 1300.0d0] "°C" "15ЖТ_16"

120 "TSI_T121" [0.0d0 1300.0d0] "°C" "15ЖТ_17"
121 "TSI_T122" [0.0d0 1300.0d0] "°C" "15ЖТ_18"
122 "TSI_T123" [0.0d0 1300.0d0] "°C" "15ЖТ_19"
123 "TSI_T124" [0.0d0 1300.0d0] "°C" "15ЖТ_20"
87 "TSI_T087" [0.0d0 1300.0d0] "°C" "15ЖТ_21"
88 "TSI_T088" [0.0d0 1300.0d0] "°C" "15ЖТ_22"
89 "TSI_T089" [0.0d0 1300.0d0] "°C" "15ЖТ_23"
124 "TSI_T125" [0.0d0 1300.0d0] "°C" "15ЖТ_24"
125 "TSI_T126" [0.0d0 1300.0d0] "°C" "15ЖТ_25"
126 "TSI_T127" [0.0d0 1300.0d0] "°C" "15ЖТ_26"
127 "TSI_T128" [0.0d0 1300.0d0] "°C" "15ЖТ_27"
128 "TSI_T129" [0.0d0 1300.0d0] "°C" "15ЖТ_28"
129 "TSI_T130" [0.0d0 1300.0d0] "°C" "15ЖТ_29"
90 "TSI_T090" [0.0d0 1300.0d0] "°C" "15ЖТ_30"
91 "TSI_T091" [0.0d0 1300.0d0] "°C" "15ЖТ_31"
130 "TSI_T131" [0.0d0 1300.0d0] "°C" "15ЖТ_32"
131 "TSI_T132" [0.0d0 1300.0d0] "°C" "15ЖТ_33"
132 "TSI_T133" [0.0d0 1300.0d0] "°C" "15ЖТ_34"
133 "TSI_T134" [0.0d0 1300.0d0] "°C" "15ЖТ_35"
134 "TSI_T135" [0.0d0 1300.0d0] "°C" "15ЖТ_36"
92 "TSI_T092" [0.0d0 1300.0d0] "°C" "15ЖТ_37"
93 "TSI_T093" [0.0d0 1300.0d0] "°C" "15ЖТ_38"
94 "TSI_T094" [0.0d0 1300.0d0] "°C" "15ЖТ_39"
135 "TSI_T136" [0.0d0 1300.0d0] "°C" "15ЖТ_40"
136 "TSI_T137" [0.0d0 1300.0d0] "°C" "15ЖТ_41"
137 "TSI_T138" [0.0d0 1300.0d0] "°C" "15ЖТ_42"
95 "TSI_T095" [0.0d0 1300.0d0] "°C" "15ЖТ_43"

138 "TSI_T139" [0.0d0 1300.0d0] "°C" "15ЖТ_45"
97 "TSI_T097" [0.0d0 1300.0d0] "°C" "15ЖТ_45"

139 "TSI_T140" [0.0d0 1300.0d0] "°C" "15ЖТ_46"
98 "TSI_T098" [0.0d0 1300.0d0] "°C" "15ЖТ_47"
99 "TSI_T099" [0.0d0 1300.0d0] "°C" "15ЖТ_48"
140 "TSI_T141" [0.0d0 1300.0d0] "°C" "15ЖТ_49"
141 "TSI_T142" [0.0d0 1300.0d0] "°C" "15ЖТ_50"
142 "TSI_T143" [0.0d0 1300.0d0] "°C" "15ЖТ_51"
143 "TSI_T144" [0.0d0 1300.0d0] "°C" "15ЖТ_52"

96 "TSI_T096" [0.0d0 1300.0d0] "°C" "15ЖТ_54"
100 "TSI_T100" [0.0d0 1300.0d0] "°C" "15ЖТ_55"
144 "TSI_T145" [0.0d0 1300.0d0] "°C" "15ЖТ_56"
101 "TSI_T101" [0.0d0 1300.0d0] "°C" "15ЖТ_57"
145 "TSI_T146" [0.0d0 1300.0d0] "°C" "15ЖТ_58"
146 "TSI_T147" [0.0d0 1300.0d0] "°C" "15ЖТ_59"
147 "TSI_T151" [0.0d0 1300.0d0] "°C" "15ЖТ_60"

102 "TSI_T102" [0.0d0 1300.0d0] "°C" "15ЖТ_62"
148 "TSI_T152" [0.0d0 1300.0d0] "°C" "15ЖТ_63"
149 "TSI_T153" [0.0d0 1300.0d0] "°C" "15ЖТ_64"
150 "TSI_T154" [0.0d0 1300.0d0] "°C" "15ЖТ_65"
151 "TSI_T155" [0.0d0 1300.0d0] "°C" "15ЖТ_66"

152 "TSI_T156" [0.0d0 1300.0d0] "°C" "15ЖТ_67"
153 "TSI_T157" [0.0d0 1300.0d0] "°C" "15ЖТ_67"

154 "TSI_T158" [0.0d0 1300.0d0] "°C" "15ЖТ_68"
103 "TSI_T103" [0.0d0 1300.0d0] "°C" "15ЖТ_69"
104 "TSI_T104" [0.0d0 1300.0d0] "°C" "15ЖТ_70"
155 "TSI_T159" [0.0d0 1300.0d0] "°C" "15ЖТ_71"
156 "TSI_T160" [0.0d0 1300.0d0] "°C" "15ЖТ_72"
157 "TSI_T161" [0.0d0 1300.0d0] "°C" "15ЖТ_73"
158 "TSI_T162" [0.0d0 1300.0d0] "°C" "15ЖТ_74"
159 "TSI_T163" [0.0d0 1300.0d0] "°C" "15ЖТ_75"
160 "TSI_T164" [0.0d0 1300.0d0] "°C" "15ЖТ_76"


105 "TSI_T105" [0.0d0 1300.0d0] "°C" ""
106 "TSI_T106" [0.0d0 1300.0d0] "°C" ""
107 "TSI_T108" [0.0d0 1300.0d0] "°C" ""
108 "TSI_T109" [0.0d0 1300.0d0] "°C" ""
109 "TSI_T110" [0.0d0 1300.0d0] "°C" ""


