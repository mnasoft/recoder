;;;; ./src/trd/trd.lisp

(defpackage #:recoder/trd
  (:use #:cl #:mnas-string/print #:recoder/binary #:recoder/d-signal #:recoder/a-signal)
  (:export trd-open
           trd-close)
  (:export *trd*)
  (:export <trd>
	   <trd>-total-records
	   <trd>-delta-time
           <trd>-analog-number
           <trd>-file-descr
           <trd>-discret-number
           <trd>-id-string
           <trd>-version
	   <trd>-file-name
           <trd>-reserv
           <trd>-utime-start
           <trd>-analog-ht 
           <trd>-discret-ht)
  (:export trd-analog-length-byte
           trd-discret-length-byte
           trd-discret-offset
           trd-start-offset
           trd-record-length
           trd-utime-end)  
  (:export trd-separate-signals
           trd-separate-a-signals
           trd-separate-d-signals
           trd-separate-not-signals)
  (:export trd-a-units
           trd-a-ids)
  (:export trd-record-number-to-udate
           trd-record-number-by-utime
           trd-record-number-by-udate)
  (:export make-html-trd-foo
	   make-html-trd)
  (:export find-trd-by-utime-dirname
	   time-universal-encode)
  (:export trd-utime-by-record-number))

;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(in-package #:recoder/trd)

(defconstant +head-id-wid+              5 "Строка идентификации файла тренда, char[5]")

(defconstant +head-version-wid+         1 "Версия данных тренда, char[1]")

(defconstant +head-date-wid+            3 "День Месяц Год-2000 char[3]")

(defconstant +head-time-wid+            3 "Час Минута Секунда char[3]")

(defconstant +head-wid+                30 "Общая длина заголовка, char[30]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defconstant +signal-id-wid+           10 "Длина строки обозначения сигнала, char[10]")

(defconstant +signal-description-wid+  40 "Длина строки описания сигнала, char[40] ")

(defconstant +signal-units-wid+         8 "Длина строки размерности аналогового сигнала, char[8]")

(defconstant +signal-LowLimit-wid+      8 "Ширина поля для нижней  границы диапазона аналогового сигнала, double = char[8]")

(defconstant +signal-HighLimit-wid+     8 "Ширина поля для верхней границы диапазона аналогового сигнала, double = char[8]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +analog-wid+ (+ +signal-id-wid+ +signal-description-wid+ +signal-units-wid+ +signal-LowLimit-wid+ +signal-HighLimit-wid+)
  "Длина заголовка одной записи аналогового сигнала")

(defconstant +discret-wid+ (+ +signal-id-wid+ +signal-description-wid+ )
  "Длина заголовка одной записи дискретного сигнала")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <trd> ()
  ((file-name         :accessor <trd>-file-name      :initarg :file-name   :initform nil :documentation "Имя файла в файловой системе")
   (file-descr        :accessor <trd>-file-descr                           :initform nil :documentation "Файл тренда")
   (id-string         :accessor <trd>-id-string                            :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (version           :accessor <trd>-version                              :initform nil :documentation "Версия тренда")
   (utime-start       :accessor <trd>-utime-start                          :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (reserv            :accessor <trd>-reserv                               :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (total-records     :accessor <trd>-total-records                        :initform nil :documentation "Общее число записей в тренде")
   (delta-time        :accessor <trd>-delta-time                           :initform nil :documentation "Интервал между записями тренда")
   (analog-number     :accessor <trd>-analog-number                        :initform nil :documentation "Количество аналоговых сигналов")
   (discret-number    :accessor <trd>-discret-number                       :initform nil :documentation "Количество дискретных сигналов")
   (analog-ht         :accessor <trd>-analog-ht                            :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (discret-ht        :accessor <trd>-discret-ht                           :initform nil :documentation "Хеш-таблица дискретных сигналов"))
  (:documentation "@b(Описание:) класс @b(<trd>) служит для предоставления
интерфейса к файлу-тренду, содержащему записи аналоговых и дискретных сигналов.

Файл-тренд состоит из:
@begin(enum)
@item(Записи заголовка тренда;)
@item(Записей дескрипторов (описаний) аналоговых сигналов, см. <a-signal>;)
@item(Записей дескрипторов (описаний) дискретных сигналов, см. <d-signal>;)
@item(Записей аналоговых и дискретных сигналов, состоящий из последовательно записанных списка аналоговых сигналов и упакованного списка дискретных сигналов;

4.1 Каждый аналоговый сигнал кодируется целочисленным значением длиной 2 байта, 
его вычисляется по формуле: rez=analog-LowLimit+(i*(analog-HighLimit-analog-LowLimit)/65535)

4.2 Каждый дискретный сигнал кодируется одним битом информации 0|1)
@end(enum)


Заголовок тренда имеет следующую структуру:
@begin(table)
@begin(row) @cell(Поле)             @cell(Дина поля, байт)  @cell(Примечание) @end(row)
@begin(row) @cell(id)               @cell(5)                @cell(Строка идентификации) @end(row)
@begin(row) @cell(version)          @cell(1)                @cell(Версия данных трендера) @end(row)
@begin(row) @cell(date-day)         @cell(1)                @cell(Число месяца) @end(row)
@begin(row) @cell(date-month)       @cell(1)                @cell(Порядковый номер месяца) @end(row)
@begin(row) @cell(date-year)        @cell(1)                @cell(Год-2000) @end(row)
@begin(row) @cell(time-hour)        @cell(1)                @cell(Час) @end(row)
@begin(row) @cell(time-minute)      @cell(1)                @cell(Минута) @end(row)
@begin(row) @cell(time-second)      @cell(1)                @cell(Секунда) @end(row)
@begin(row) @cell(reserv)           @cell(2)                @cell(Резерв - содержит сумму аналоговых и дискретных сигналов до 2^16=65536 шт) @end(row)
@begin(row) @cell(total-records)    @cell(4)                @cell(Количество записей, содержащееся в тренде до 2^32=4294967296 шт) @end(row)
@begin(row) @cell(delta-time)       @cell(8)                @cell(Интервал времени между записями, с) @end(row)
@begin(row) @cell(analog-number)    @cell(2)                @cell(Количество аналоговых сигналов до 2^16=65536 шт) @end(row)
@begin(row) @cell(discret-number)   @cell(2)                @cell(Количество дискретных сигналов до 2^16=65536 шт) @end(row)
@end(table)

При записи в тренд на восемь дискретных сигналов отводится один байт.

Сигналы упаковываются побайтно слева-направо."))

(defmethod print-object ((trd <trd>) stream)
  (format stream "Path= ~S~%" (<trd>-file-name trd) )
  (when (<trd>-file-descr trd)
    (format stream "id=~S version=~A " (<trd>-id-string trd) (<trd>-version trd))
    (format stream "[ ")
    (day-time (<trd>-utime-start trd) :stream stream)
    (format stream " ; ")
    (day-time (trd-utime-end trd) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (<trd>-reserv trd) (<trd>-total-records trd) (<trd>-delta-time trd) (<trd>-analog-number trd) (<trd>-discret-number trd))
    (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (<trd>-analog-ht trd) )
    (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (<trd>-discret-ht trd) )))

(defmethod trd-open ((trd <trd>))
  "@b(Описание:) trd-open выполняет открытие файла тренда включая:
@begin(list)
 @item(чтение заголовка;)
 @item(разбор аналоговых сигналов;)
@item(разбор дискретных сигналов.)
@end(list)
"
  (trd-read-header trd)
  (trd-read-analog-ht trd )
  (trd-read-discret-ht trd)
  trd)

(defmethod trd-read-header ((trd <trd>))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (<trd>-file-descr trd))
    (setf (<trd>-file-descr trd) (open-b-read (<trd>-file-name trd)))
    (let ((in (<trd>-file-descr trd))
          (bufer nil)
          (date-day nil)
          (date-month nil)
          (date-year nil)
          (time-hour nil)
          (time-minute nil)
          (time-second nil))
      (setf (<trd>-id-string trd)      (decode-string (b-read in +head-id-wid+))
	    (<trd>-version trd)        (car (b-read in +head-version-wid+))
	    bufer                  (b-read in +head-date-wid+)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (b-read in +head-time-wid+)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (<trd>-utime-start trd)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (<trd>-reserv trd)         (b-read-short in)
	    (<trd>-total-records trd)  (b-read-long in)
	    (<trd>-delta-time trd)     (b-read-double in)
	    (<trd>-analog-number trd)  (b-read-short in)
	    (<trd>-discret-number trd) (b-read-short in))
      (setf (<trd>-total-records trd)
	    (/ (- (file-length (<trd>-file-descr trd)) (trd-start-offset trd))
	       (trd-record-length trd)))))
  trd)

(defmethod trd-read-analog-ht((trd <trd>))
  "Выполняет разбор аналоговых сигналов"
  (when (null (<trd>-analog-ht trd))
    (setf (<trd>-analog-ht trd)  (make-hash-table :test #'equal :size (<trd>-analog-number trd)))
    (file-position (<trd>-file-descr trd) +head-wid+)
    (let ((in (<trd>-file-descr trd)) (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil))
      (dotimes (i (<trd>-analog-number trd) 'done)
	(setf analog-id          (decode-string (b-read in +signal-id-wid+))
	      analog-description (decode-string (b-read in +signal-description-wid+))
	      analog-units       (decode-string (b-read in +signal-units-wid+))
	      analog-min         (b-read-double in)
	      analog-max         (b-read-double in)
	      (gethash analog-id (<trd>-analog-ht trd)) (make-instance '<a-signal>
								     :num i
								     :id  analog-id
								     :description analog-description
								     :units analog-units
								     :min analog-min
								     :max analog-max))))))

(defmethod trd-read-discret-ht((trd <trd>))
  "Выполняет разбор дискретных сигналов"
  (when (null (<trd>-discret-ht trd))
    (setf (<trd>-discret-ht trd) (make-hash-table :test #'equal :size (<trd>-discret-number trd)))
    (file-position (<trd>-file-descr trd) (+ +head-wid+ (* (<trd>-analog-number trd) +analog-wid+)))
    (let ((in (<trd>-file-descr trd)) (discret-id nil) (discret-description nil))
      (dotimes (i (<trd>-discret-number trd) 'done)
	(setf discret-id          (decode-string (b-read in +signal-id-wid+))
	      discret-description (decode-string (b-read in +signal-description-wid+))
	      (gethash discret-id (<trd>-discret-ht trd)) (make-instance '<d-signal>
								       :num i
								       :id discret-id
								       :description discret-description))))))

(defmethod trd-close ((trd <trd>))
  "@b(Описание:) метод @b(trd-close) выполняет закрытие файла тренда"
  (when (<trd>-file-descr trd)
    (close (<trd>-file-descr trd))
    (setf (<trd>-file-descr trd) nil)))

(defmethod trd-start-offset ((trd <trd>))
  "@b(Описание:) метод @b(trd-start-offset) возвращает смещение, 
выраженное в байтах, первой (нулевой) записи тренда."
  (+ +head-wid+
     (* (<trd>-analog-number trd) +analog-wid+)
     (* (<trd>-discret-number trd) +discret-wid+)))

(defmethod trd-analog-length-byte ((trd <trd>))
  "@b(Описание:) метод @b(trd-analog-length-byte) возвращает 
длину занимаемую аналоговыми сигналами одной записи тренда.
"
  (* (<trd>-analog-number trd) 2))

(defmethod trd-discret-length-byte ((trd <trd>))
  "@b(Описание:) метод @b(trd-discret-length-byte) возвращает
количество байт необходимое для записи дискретных сигналов
одной записи.
"
  (ceiling (/ (<trd>-discret-number trd) 8)))

(defmethod trd-record-length ((trd <trd>))
  "@b(Описание:) метод @b(trd-record-length) возвращает
длину одной записи тренда."
  (+  (trd-analog-length-byte trd)  (trd-discret-length-byte trd)))

(defmethod trd-discret-offset ((trd <trd>))
  "@b(Описание:) метод @b(trd-discret-offset) возвращает 
смещение в байтах от начала записи до начала записи дискретных сигналов."
  (trd-analog-length-byte trd))

(defmethod trd-utime-end ((trd <trd>))
  "@b(Описание:) метод @b(trd-utime-end) возвращает время окончания тренда.
Время возвращается в универсальном формате (universal-time)"
  (+ (<trd>-utime-start trd)
     (floor (* (<trd>-total-records trd) (<trd>-delta-time trd)))))



(defmethod trd-record-number-by-utime ((trd <trd>) utime)
  "@b(Описание:) метод @b(trd-record-number-by-utime) возвращает номер
 записи по универсальному времени."
  (floor (- utime (<trd>-utime-start trd)) (<trd>-delta-time trd)))

(defmethod trd-utime-by-record-number ((trd <trd>) record-number)
  "@b(Описание:) метод @b(trd-record-number-by-utime)
 Возвращает номер записи по универсальному времени"
  (+ (<trd>-utime-start trd) (floor record-number (/ 1 (<trd>-delta-time trd)))))

(defmethod trd-discret-by-rec-number ( (trd <trd>) rec-number d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (when (and (<trd>-file-descr trd) (< -1 rec-number (<trd>-total-records trd)))
    (file-position (<trd>-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (<trd>-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signal-list))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Singal separation to analog|discret|unexpected ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-separate-signals ((trd <trd>) singnal-str-list)
  "@b(Описание:) метод @b(trd-separate-signals) выполняет проверку
 того, что имена сигналов, заданные в переменной singnal-str-list,
 присутствуют для данного тренда в перечне аналоговых сигналов или
 дискретных сигналов или отсутствуют в обоих перечнях.

 Возвращает список элементами которого являются:
@begin(list)
 @item(список аналоговых сигналов;)
 @item(список дискретных сигналов;) 
 @item(список строк с именами не соответствующими ни аналоговым ни
       дискретным сигналам.)
@end(list)"
  (let ((a-rez nil)
	(d-rez nil)
	(error-rez nil)
	(error-fl t))
    (mapcar #'(lambda (el)
		(setf error-fl t)
		(multiple-value-bind (v r) (gethash  el (<trd>-analog-ht trd ) )
		  (when r
		    (push v a-rez)
		    (setf error-fl nil)))
		(multiple-value-bind (v r) (gethash  el (<trd>-discret-ht trd ) )
		  (when r (push v d-rez)
			(setf error-fl nil)))
		(when error-fl (push el error-rez)))
	    singnal-str-list)
    (list (nreverse a-rez) (nreverse d-rez) (nreverse error-rez))))

(defmethod trd-separate-a-signals ((trd <trd>) singnal-str-list)
  "Выделяет из переменной singnal-str-list аналоговые сигналы"
  (first (trd-separate-signals trd singnal-str-list)))

(defmethod trd-separate-d-signals ((trd <trd>) singnal-str-list)
  "Выделяет из переменной singnal-str-list дискретные сигналы"
  (second (trd-separate-signals trd singnal-str-list)))

(defmethod trd-separate-not-signals ((trd <trd>) singnal-str-list)
  "Выделяет из переменной singnal-str-list неожиданные сигналы."
  (second (trd-separate-signals trd singnal-str-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-record-number-to-udate ((trd <trd>) rec-number)
  "trd-record-number-to-udate"
  (+ (<trd>-utime-start trd) (round (* rec-number (<trd>-delta-time  trd)))))

(defmethod trd-record-number-by-udate ((trd <trd>) udate)
  "trd-record-number-by-udate"
  (round
   (/
    (- udate (<trd>-utime-start trd))
    (<trd>-delta-time trd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun time-universal-encode (year month day hour min sec)
  "@b(Описание:) функция @b(time-universal-encode) возвращает время в
  универсальном формате. Аналогична вызову функции
  @b(encode-universal-time) с параметрами следующими в обратнром
  порядке.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (time-universal-encode 2021 08 30 10 00 00 ) => 3839295600
@end(code)"
  (encode-universal-time sec min hour day month year))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-trd (trd-fname html-fname str-signal-list time-lst ht-sname-oboznach &key (transpose nil))
  "Вывод в данных из тренда в файл trd-fname в файл html-fname;
Данные выводятся по столбцам;
trd-fname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time-lst          - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:
"
  (let ((trd (make-instance '<trd> :file-name trd-fname)))
    (trd-open trd)
    (let* ((s-list (trd-analog-signal-list trd str-signal-list))
	   (rez nil)
	   (data (mapcar #'(lambda (el) (trd-analog-mid-by-udate trd el    s-list)) time-lst))
	   (dev  (mapcar #'(lambda (el) (trd-analog-stddev-by-udate trd el s-list)) time-lst))
	   (d-time-str (mapcar #'(lambda (tm ) (list (date tm :stream nil)
						     (day-time tm :stream nil)))
			       time-lst)))
      (setf data  (mapcar #'(lambda (tm da) (append tm da)) d-time-str data)
	    dev   (mapcar #'(lambda (tm dv) (append tm dv)) d-time-str dev))
      (setf rez (append  data dev ))
      (push (append '("YYYY-MM-DD" "hh:mm:ss") (mapcar #'(lambda (el) (<a-signal>-units el)) s-list) ) rez)
      (push (append '("-" "-") (mapcar #'(lambda (el) (<a-signal>-id el))    s-list) ) rez)
      (push (append '("Date" "Time") (mapcar #'(lambda (el) (gethash (<a-signal>-id el) ht-sname-oboznach)) s-list)) rez)
      (push (append '("Дата" "Время") (mapcar #'(lambda (el) (<a-signal>-description el)) s-list)) rez)
      (when transpose (setf rez (transpose rez)))
      (html-table:list-list-html-table rez html-fname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-trd-by-utime-dirname (utime dir-name &key (extension "trd"))
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в каталоге dir-name
"
  (let ((rezult nil))
    (mapc  
     #'(lambda (el)
	 (let ((trd (make-instance '<trd> :file-name el)))
	   (trd-open trd)
	   (if (<= (<trd>-utime-start trd) utime (trd-utime-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     (mnas-path:find-filename dir-name extension))
    rezult))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-trd-foo (trd-dname html-fname str-signal-list time-lst ht-sname-oboznach &key (transpose nil))
  "Вывод в данных из тренда в файл trd-dname в файл html-fname;
Данные выводятся по столбцам;
trd-dname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time-lst          - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:

"
  (let ((trd-lst (mapcar #'(lambda (ut) (find-trd-by-utime-dirname ut trd-dname)) time-lst))
	(rez                  nil)
	(data                 nil)
	(dev                  nil)
	(d-time-str           nil)
	
	(<a-signal>-units       nil)
	(<a-signal>-id          nil)
	(ht-sname             nil)
	(<a-signal>-description nil)	
	)
    (mapc #'(lambda (trd time)
	      (trd-open trd)
	      (let ((s-list (trd-analog-signal-list trd str-signal-list)))
		(setf <a-signal>-units (mapcar #'(lambda (el) (<a-signal>-units el)) s-list)
		      <a-signal>-id (mapcar #'(lambda (el) (<a-signal>-id el))       s-list)
		      ht-sname (mapcar #'(lambda (el) (gethash (<a-signal>-id el) ht-sname-oboznach)) s-list)
		      <a-signal>-description (mapcar #'(lambda (el) (<a-signal>-description el)) s-list)
		      
		      )

		(push (trd-analog-mid-by-utime trd time s-list)    data)
		(push (trd-analog-stddev-by-utime trd time s-list) dev)
		(push (list (date time :stream nil) (day-time time :stream nil)) d-time-str)))
	  trd-lst time-lst)
    (setf data (reverse data)
	  dev  (reverse dev)
	  d-time-str (reverse d-time-str)
	  )
    (setf data  (mapcar #'(lambda (tm da) (append tm da)) d-time-str data)
	  dev   (mapcar #'(lambda (tm dv) (append tm dv)) d-time-str dev))
    (setf rez (append  data dev ))
    (push (append '("YYYY-MM-DD" "hh:mm:ss") <a-signal>-units ) rez)
    (push (append '("-" "-") <a-signal>-id )                    rez)
    (push (append '("Date" "Time") ht-sname  ) rez)
    (push (append '("Дата" "Время") <a-signal>-description) rez)
    (when transpose (setf rez (transpose rez)))
    (html-table:list-list-html-table rez html-fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod trd-a-ids (a-sig-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-ids) возвращает имена 
идентификаторов аналоговых сигналов.
@begin(list)
 @item(a-sig-names - список имен сигналов; )
 @item(trd         - тренд. )
@end(list)
"
  (mapcar
   #'(lambda (el)
       (<a-signal>-id
	(gethash el (<trd>-analog-ht trd))))
   a-sig-names))

(defmethod trd-a-units (a-sig-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-units) возвращает размерности 
аналоговых сигналов.
@begin(list)
 @item(a-sig-names - список имен сигналов;)
 @item( trd        - тренд.)
@end(list)
"
  (mapcar
   #'(lambda (el)
       (<a-signal>-units
	(gethash el (<trd>-analog-ht trd))))
   a-sig-names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-fname*
  (concatenate 'string
	       (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
  "Для примеров.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd* (make-instance '<trd> :file-name *trd-fname*))

(trd-open *trd*)
