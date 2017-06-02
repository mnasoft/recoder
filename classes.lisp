;;;; classes.lisp

(in-package #:recoder)

(defun array-to-list (arr)
  (let ((rez nil)
	(arr-dim (array-dimension arr 0)))
    (dotimes (i arr-dim rez)
      (push (aref arr (- arr-dim i 1)) rez))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass trend ()
  ((head-wid            :allocation :class            :initform  30 :reader   head-wid            :documentation "Длина всех полей заголовка файла-тренда")
   (head-id-wid         :allocation :class            :initform   5 :reader   head-id-wid         :documentation "Длина поля строки-идентификатора файла тренда \"Trend\"")
   (head-version-wid    :allocation :class            :initform   1 :reader   head-version-wid    :documentation "Длина поля версии формата файла-тренда тренда")
   (head-date-wid       :allocation :class            :initform   3 :reader   head-date-wid       :documentation "Длина полей даты начала записи тренда: день; месяц; год-2000")
   (head-time-wid       :allocation :class            :initform   3 :reader   head-time-wid       :documentation "Длина полей времени начала записи тренда: час; минута; секунда")
   (analog-wid          :allocation :class            :initform  74 :reader   analog-wid          :documentation "Длина поля дескриптора аналогового сигнала")
   (discret-wid         :allocation :class            :initform  50 :reader   discret-wid         :documentation "Длина поля дескриптора дискретного сигнала")
   (file-name           :initarg :file-name           :initform ""  :accessor file-name           :documentation "Имя файла, в котором находятся данные тренда")
   (file-descriptor     :initarg :file-descriptor     :initform nil :accessor file-descriptor     :documentation "Дескриптор файла-тренда")
   (is-file-opened      :initarg :is-file-opened      :initform nil :accessor is-file-opened      :documentation "Состояние файла-тренда: nil - файл тренда не открыт; T - файл тренда открыт")
   (id                  :initarg :id                  :initform nil :accessor id                  :documentation "Строка идентификации")
   (version             :initarg :version             :initform nil :accessor version             :documentation "Версия данных трендера")
   (date-day            :initarg :date-day            :initform nil :accessor date-day            :documentation "День")
   (date-month          :initarg :date-month          :initform nil :accessor date-month          :documentation "Месяц")
   (date-year           :initarg :date-year           :initform nil :accessor date-year           :documentation "Год-2000")
   (time-hour           :initarg :time-hour           :initform nil :accessor time-hour           :documentation "Час")
   (time-minute         :initarg :time-minute         :initform nil :accessor time-minute         :documentation "Минута")
   (time-second         :initarg :time-second         :initform nil :accessor time-second         :documentation "Секунда")
   (reserv              :initarg :reserv              :initform nil :accessor reserv              :documentation "Резерв")
   (total-records       :initarg :total-records       :initform nil :accessor total-records       :documentation "Общее число записей")
   (delta-time          :initarg :delta-time          :initform nil :accessor delta-time          :documentation "Интервал времени")
   (analog-number       :initarg :analog-number       :initform nil :accessor analog-number       :documentation "Количество аналоговых сигналов")
   (discret-number      :initarg :discret-number      :initform nil :accessor discret-number      :documentation "Количество дискретных сигналов")
   (analog-descriptors  :initarg :analog-descriptors  :initform nil :accessor analog-descriptors  :documentation "List of descriptors of analog signals wich is into trend")
   (discret-descriptors :initarg :discret-descriptors :initform nil :accessor discret-descriptors :documentation "List of descriptors of discret signals wich is into trend")
   )
  (:documentation ""))

(defmethod print-object :before ((x trend) s) (format s "#trend("))

(defmethod print-object ((x trend) s) 
  (format s "~%Id~15T= ~S~%Version~15T= ~A~%Date-Time~15T= ~S-~S-~S ~S:~S:~S"
	  (id x) (version x) (date-year x) (date-month x) (date-day x) (time-hour x) (time-minute x) (time-second x))
  (format s "~%Reserv~15T= ~A~%Total-records~15T= ~A~%Delta-time~15T= ~A~%Analog-number~15T= ~A~%Discret-number~15T= ~A"
	  (reserv x) (total-records x) (delta-time x) (analog-number x) (discret-number x))
  (format s "~%File-name~15T= ~S~%File-opened~15T= ~S" (file-name x) (is-file-opened x))
  (format s "~%~S" (analog-descriptors x))
  (format s "~%~S" (discret-descriptors x)))

(defmethod print-object :after ((x trend) s) (format s ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass analog-hdr ()
  ((analog-id-wid          :allocation :class  :initform  10 :reader   analog-id-wid           :documentation "")
   (analog-description-wid :allocation :class  :initform  40 :reader   analog-description-wid  :documentation "")
   (analog-units-wid       :allocation :class  :initform   8 :reader   analog-units-wid        :documentation "")
   (analog-LowLimit-wid    :allocation :class  :initform   8 :reader   analog-LowLimit-wid     :documentation "")
   (analog-HighLimit-wid   :allocation :class  :initform   8 :reader   analog-HighLimit-wid    :documentation "")
   (analog-id                                  :initform nil :accessor analog-id               :documentation "Обозначение аналогового сигнала; char[10]")
   (analog-description                         :initform nil :accessor analog-description      :documentation "Описание аналогового сигнала; char[40]")
   (analog-units                               :initform nil :accessor analog-units            :documentation "Размернсть аналогового сигнала; char[8]")
   (analog-LowLimit                            :initform nil :accessor analog-LowLimit         :documentation "Нижняя граница аналогового сигнала; double")
   (analog-HighLimit                           :initform nil :accessor analog-HighLimit        :documentation "Верхняя граница аналогового сигнала; double"))
  (:documentation "Дескриптор аналогового сигнала"))

(defmethod print-object :before ((x analog-hdr) s) (format s "#analog-hdr("))

(defmethod print-object  ((x analog-hdr) s) (format s "~S ~S ~S ~S ~S" (analog-id x) (analog-LowLimit x) (analog-HighLimit x) (analog-units x) (analog-description x)))

(defmethod print-object :after ((x analog-hdr) s) (format s ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass discret-hdr ()
  ((discret-id-wid          :allocation :class  :initform 10  :reader   discret-id-wid          :documentation "Длина обозначения")
   (discret-description-wid :allocation :class  :initform 40  :reader   discret-description-wid :documentation "Длина комментария")
   (discret-id                                  :initform nil :accessor discret-id              :documentation "Обозначение аналогового сигнала; char[10]")
   (discret-description                         :initform nil :accessor discret-description     :documentation "Описание аналогового сигнала; char[40]"))
    (:documentation "Дескриптор дискретного сигнала"))

(defmethod print-object :before ((x discret-hdr) s) (format s "#discret-hdr("))

(defmethod print-object ((x discret-hdr) s) (format s "~S ~S" (discret-id x) (discret-description x)))

(defmethod print-object :after ((x discret-hdr) s) (format s ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Выполняет чтение заголовка файла тренда"
  (if (null (is-file-opened tr)) (open-trd-file tr))
  (when (file-position (file-descriptor tr) 0)
    (let ((bufer nil)) ;; Буфер для чтения данных
      (setf
       (id tr) (recode-string (read-trd-file  (file-descriptor tr) (head-id-wid tr)))
       (version tr)  (car(read-trd-file  (file-descriptor tr) (head-version-wid tr)))
       bufer (read-trd-file  (file-descriptor tr) (head-date-wid tr))
       (date-day tr) (first bufer)
       (date-month tr) (second bufer)
       (date-year tr) (+ 2000 (third bufer))
       bufer (read-trd-file  (file-descriptor tr) (head-time-wid tr))
       (time-hour tr) (first bufer)
       (time-minute tr) (second bufer)
       (time-second tr) (third bufer)
       (reserv tr) (read-trd-file-short (file-descriptor tr))
       (total-records tr) (read-trd-file-long (file-descriptor tr))
       (delta-time tr) (read-trd-file-double (file-descriptor tr))
       (analog-number tr) (read-trd-file-short (file-descriptor tr))
       (discret-number tr) (read-trd-file-short (file-descriptor tr))))))

(defmethod read-analog-descriptor-list ((tr trend))
  (when (file-position (file-descriptor tr) (head-wid tr))
    (let ((a-hdr nil))
      (setf (analog-descriptors tr) (make-array (analog-number tr)))
      (dotimes (i (analog-number tr) (analog-descriptors tr))
	(setf a-hdr                      (make-instance 'analog-hdr)
	      (analog-id a-hdr)          (recode-string (read-trd-file (file-descriptor tr) (analog-id-wid a-hdr)))
	      (analog-description a-hdr) (recode-string (read-trd-file (file-descriptor tr) (analog-description-wid a-hdr)))
	      (analog-units a-hdr)       (recode-string (read-trd-file (file-descriptor tr) (analog-units-wid a-hdr)))
	      (analog-LowLimit a-hdr)    (read-trd-file-double (file-descriptor tr))
	      (analog-HighLimit a-hdr)   (read-trd-file-double (file-descriptor tr))
	      (aref (analog-descriptors tr) i) a-hdr)))))

(defmethod read-discret-descriptor-list ((tr trend))
  (when (file-position (file-descriptor tr) (+ (head-wid tr) (analog-length tr)))
    (let ((d-hdr nil))
      (setf (discret-descriptors tr) (make-array (discret-number tr)))
      (dotimes (i (discret-number tr) (discret-descriptors tr))
	(setf d-hdr                       (make-instance 'discret-hdr)
	      (discret-id d-hdr)          (recode-string (read-trd-file (file-descriptor tr) (discret-id-wid d-hdr)))
	      (discret-description d-hdr) (recode-string (read-trd-file (file-descriptor tr) (discret-description-wid d-hdr)))
	      (aref (discret-descriptors tr) i) d-hdr)))))

(defmethod initialize-instance :after ((tr trend) &key)
  "Выполняет инициализацию полей тренда"
  (read-header tr)
  (read-analog-descriptor-list tr)
  (read-discret-descriptor-list tr))

(defmethod analog-length ((tr trend)) (* (analog-wid tr)  (analog-number tr)))

(defmethod discret-length ((tr trend)) (* (discret-wid tr) (discret-number tr)))

(defmethod head-length ((tr trend))
  (+ (head-wid tr) (analog-length tr) (discret-length tr)))

(defmethod record-length ((tr trend))
  "Возвращает длину одной записи файла-треда;"
  (+ (* (analog-number tr) 2)
     (ceiling (discret-number tr) 8)))

(defmethod culc-records-number ((tr trend))
  "Вычисляет количество записей тренда"
  (floor (- (file-length (file-descriptor tr))
	(head-length tr))
	 (record-length tr)))

(defmethod find-analog-position-by-name ((tr trend) str-list)
  "Выполняет поиск позиций аналоговых сигналов в записях трендов по их именам"
  (mapcar #'(lambda(el)
	      (position el (analog-descriptors tr) :test #'string= :key #'(lambda (el) (analog-id el))))
	  str-list))

(defmethod file-record-position ((tr trend) i)
  "Перематывает позицию чтения-записи файла-треднда, в положение  начинается i-товая запись;"
  (file-position
   (file-descriptor tr)
   (+ (head-length tr) (* i (record-length tr)))))

(defmethod get-record ((tr trend) i &optional (a-items (let ((rez nil)) (dotimes (i (analog-number tr) (nreverse rez)) (push i rez)))))
  (file-record-position tr i)
  (let ((a-arr (make-array (analog-number tr) :element-type 'float :initial-element -10.0))
	(a-ref nil)
	(f-dat nil))
    (dotimes (j (analog-number tr) a-arr)
      (setf
       f-dat (read-trd-file-short (file-descriptor tr))
       a-ref (aref (analog-descriptors *tr*) j)
       (aref a-arr j) (+ (analog-lowlimit a-ref) (* (- (analog-highlimit a-ref) (analog-lowlimit a-ref)) (/ f-dat 65535)))))))

(defmethod print-csv ((tr trend) s &key (start 0) (end (culc-records-number tr)) (delta 50))
  "Вывод всех аналоговых сигналов тренда tr в файл s, начиная с записи start и заканчивая записью end;
При этом выводится каждая delta-вая запись;
Пример использования:"
  (progn (format s "\"DateTime\"; ")
	 (dotimes (i (analog-number *tr*) 'done)
	   (format s "~S; " (analog-id (aref (analog-descriptors *tr*) i))))
	 (format s "~%"))
  (do ((i start (+ i delta))
       (e-time (encode-universal-time
		(time-second tr) (time-minute tr) (time-hour tr)
		(date-day tr) (date-month tr) (date-year tr))))
      ((>= i end) 'done)
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time (round (+ e-time (* i (delta-time tr)))))
      (format s "\"~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D\"; " year month date hour minute second))
    (format s "~{~10,4,,,,,'EE~^; ~}~%" (array-to-list (get-record tr i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "/home/namatv/My/git/Trends/ДМ80№1/090415_150604.trd"

(defparameter *tr* (make-instance 'trend :file-name "/home/namatv/My/git/Trends/тренды для 11 отдела/20150409_144519.trd"))

<<<<<<< HEAD
=======
(defparameter *tr* (make-instance 'trend :file-name "d:/home/_namatv/_WorkPlan/2015/My/git/Trends/тренды для 11 отдела/20150409_144519.trd"))

>>>>>>> a9838052c2c9c5c1729971550ce0e7bc4987915b
(with-open-file
    (s "/home/namatv/123.csv" :direction :output :if-exists  :supersede)
  (print-csv *tr* s :start 205 :delta (* 5 6)))

;;(let ((ref (aref (analog-descriptors *tr*) 17))) (- (analog-highlimit ref) (analog-lowlimit ref)))

;;(find-analog-number-by-name *tr* (list "EN110" "ET230-1" "ET230-2" "ET240-1" "ET240-2"))

;;(close-trd-file *tr*)
<<<<<<< HEAD
=======

>>>>>>> a9838052c2c9c5c1729971550ce0e7bc4987915b
