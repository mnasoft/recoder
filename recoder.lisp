;;;; recoder.lisp

(in-package #:recoder)

;;; "recoder" goes here. Hacks and glory await!

(defun transpose (list)
  "Выполняет транспонирование"
  (apply #'mapcar #'list list))

(defun time-universal-encode (year month day hour min sec)
  "Функция кодирования в универсальный формат времени"
  (encode-universal-time sec min hour day month year))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recode-string (bufer &optional (start 0) (len (length bufer)) &key (break-nul T) (code-page *cp1251*))
  "Выполняет преобразование символов, передаваемых в параметре bufer,
имеющих кодировку code-page (*cp1251*|*cp866*), в кодировку utf8."
  (do*
   ( (i start (1+ i))
     (ch (gethash (nth i bufer) code-page) (gethash (nth i bufer) code-page))
     (str-rez ""))
   ( (or (>= i (+ start len))
	 (and break-nul (= 0 (nth i bufer)))) 
    str-rez)
    (setf str-rez (concatenate 'string str-rez ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (defparameter *mid-value-number-offset*  10))

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
   (trd-analog-ht         :accessor trd-analog-ht                                   :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (trd-discret-ht        :accessor trd-discret-ht                                  :initform nil :documentation "Хеш-таблица дискретных сигналов"))
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
	       (trd-record-length x)))
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

(defmethod trd-read-header((x trd))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (trd-file-descr x))
    (setf (trd-file-descr x) (open-trd-file-read (trd-file-name x)))
    (let ((in (trd-file-descr x)) (bufer nil) (date-day nil) (date-month nil) (date-year nil) (time-hour nil) (time-minute nil) (time-second nil))
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
	    (trd-discret-number x) (read-trd-file-short in))
      (setf (trd-total-records x)
	    (/ (- (file-length (trd-file-descr x)) (trd-start-offset x))
	       (trd-record-length x)))))
  x)


(defmethod trd-read-analog-ht((x trd))
  (when (null (trd-analog-ht x))
    (setf (trd-analog-ht x)  (make-hash-table :test #'equal :size (trd-analog-number x)))
    (file-position (trd-file-descr x) *head-wid*)
    (let ((in (trd-file-descr x)) (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil))
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
								   :a-signal-max analog-max))))))

(defmethod trd-read-discret-ht((x trd))
  (when (null (trd-discret-ht x))
    (setf (trd-discret-ht x) (make-hash-table :test #'equal :size (trd-discret-number x)))
    (file-position (trd-file-descr x) (+ *head-wid* (* (trd-analog-number x) *analog-wid*)))
    (let ((in (trd-file-descr x)) (discret-id nil) (discret-description nil))
      (dotimes (i (trd-discret-number x) 'done)
	(setf discret-id          (recode-string (read-trd-file in *signal-id-wid*))
	      discret-description (recode-string (read-trd-file in *signal-description-wid*))
	      (gethash discret-id (trd-discret-ht x)) (make-instance 'd-signal
								     :d-signal-num i
								     :d-signal-id discret-id
								     :d-signal-description discret-description))))))



(defmethod trd-close ((x trd))
  "Выполняет закрытие файла тренда"
  (when (trd-file-descr x)
    (close (trd-file-descr x))
    (setf (trd-file-descr x) nil)))

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
  (when (trd-file-descr x)
    (format stream "Path= ~S~%" (trd-file-name x) )
    (format stream "id=~S version=~A " (trd-id-string x) (trd-version x))
    (format stream "[ ")
    (mnas-string:print-universal-time (trd-date-time x) :stream stream)
    (format stream " ; ")
    (mnas-string:print-universal-time (trd-date-time-end x) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (trd-reserv x) (trd-total-records x) (trd-delta-time x) (trd-analog-number x) (trd-discret-number x))
    (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-analog-ht x) )
    (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-discret-ht x) )))

(defmethod trd-analog-signal-list ( (x trd) signal-string-list)
  "Возвращает список сигналов тренда trd, 
которые соответствуют списку обозначений сигналов из списка signal-string-list"
  (when  (trd-file-descr x)
    (mapcar #'(lambda(el)
		(gethash el (trd-analog-ht x)))
	    signal-string-list)))

(defmethod trd-values-by-rec-number ( (x trd) rec-number signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (when (and (trd-file-descr x) (< -1 rec-number (trd-total-records x)))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x) (* rec-number (trd-record-length x))))
    (let* ((v-sh (make-array (trd-analog-number x) :element-type 'integer)))
      (dotimes (i (trd-analog-number x) 'done)
	(setf (svref v-sh i)
	      (read-trd-file-short (trd-file-descr x))))
      (mapcar #'(lambda(el) (a-signal-value el (svref v-sh (a-signal-num el))))
	      signal-list))))

(defmethod trd-record-number-by-udate ( (x trd) udate)
  "Возвращает номер записи по универсальному времени"
  (floor (- udate (trd-date-time x)) (trd-delta-time x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-values-by-universal-date ( (x trd) udate signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (trd-values-by-rec-number x
		  (trd-record-number-by-udate x udate)
		  signal-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-mid-values-by-udate ( (x trd) udate signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список средних значений параметров "
  (when  (trd-file-descr x)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-udate x udate) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-values-by-rec-number x (+ n-start i) signal-list) rez))))
      (mapcar #'math:averange-value rezult))))

(defmethod trd-mid-values-by-snames ( (x trd) udate snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список средних значений параметров, 
записанных в тренде trd в момент времени udate для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-mid-values-by-udate x udate (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-stddev-values-by-udate ( (x trd) udate signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени udate для списка сигналов signal-list;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-udate x udate) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-values-by-rec-number x (+ n-start i) signal-list) rez))))
      (mapcar #'math:standard-deviation rezult))))

(defmethod trd-stddev-values-by-snames ( (x trd) udate snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени udate для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-stddev-values-by-udate x udate (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-trd (trd-fname html-fname str-signal-list time ht-sname-oboznach)
  "Вывод в данных из тренда в файл trd-fname в файл html-fname;
Данные выводятся по строкам;
trd-fname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time              - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:"
    (let ((trd (make-instance 'trd :trd-file-name trd-fname)))
      (trd-open trd)
      (let* ((s-list (trd-analog-signal-list trd str-signal-list))
	     (data (mapcar #'(lambda (el) (trd-mid-values-by-udate trd el          s-list)) time))
     	     (dev  (mapcar #'(lambda (el) (trd-stddev-values-by-udate trd el       s-list)) time)))
	(setf data (append data dev))
	(push (mapcar #'(lambda (el) (a-signal-units el))                          s-list) data)
	(push (mapcar #'(lambda (el) (a-signal-id el))                             s-list) data)
	(push (mapcar #'(lambda (el) (gethash (a-signal-id el) ht-sname-oboznach)) s-list) data)
	(push (mapcar #'(lambda (el) (a-signal-description el) )                   s-list) data)
	(html-table:list-list-html-table data html-fname))))

(defun make-transpose-html-trd (trd-fname html-fname str-signal-list time ht-sname-oboznach)
  "Вывод в данных из тренда в файл trd-fname в файл html-fname;
Данные выводятся по столбцам;
trd-fname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time              - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:"
      (let ((trd (make-instance 'trd :trd-file-name trd-fname)))
      (trd-open trd)
      (let* ((s-list (trd-analog-signal-list trd str-signal-list))
	     (data (mapcar #'(lambda (el) (trd-mid-values-by-udate trd el          s-list)) time))
     	     (dev  (mapcar #'(lambda (el) (trd-stddev-values-by-udate trd el       s-list)) time)))
	(setf data (append data dev))
	(push (mapcar #'(lambda (el) (a-signal-units el))                          s-list) data)
	(push (mapcar #'(lambda (el) (a-signal-id el))                             s-list) data)
	(push (mapcar #'(lambda (el) (gethash (a-signal-id el) ht-sname-oboznach)) s-list) data)
	(push (mapcar #'(lambda (el) (a-signal-description el) )                   s-list) data)
	(html-table:list-list-html-table (transpose data) html-fname))))

(defun get-trd-by-utime-dirname (utime dir-name &key (extension "trd"))
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в каталоге dir-name
"
  (let ((rezult nil))
    (mapcar
     #'(lambda (el)
	 (let ((trd (make-instance 'trd :trd-file-name el)))
	   (trd-open trd)
	   (if (<= (trd-date-time trd) utime (trd-date-time-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     (mnas-path:find-filename dir-name extension))
    rezult))
