;;;; ./src/trd/trd.lisp

(defpackage :recoder/trd
  (:use #:cl
        #:mnas-string/print
        #:recoder/binary
        #:recoder/d-signal
        #:recoder/a-signal)
  (:nicknames "R/TRD")
  (:export trd-open
           trd-close
           )
  (:export *trd*
           *trd-fname*)
  (:export <trd>
	   records
	   increment
           a-number
           file-descr
           d-number
           id-string
           version
	   file-name
           reserv
           utime-start
           analog-ht 
           discret-ht)
  (:export analog-length
           discret-length
           discret-offset
           start-offset
           record-length
           utime-end)  
  (:export record->utime
           utime->record)
  (:export time-universal-encode)
  (:intern read-analog-ht
           read-discret-ht)
  )

(in-package :recoder/trd)

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
  ((file-name         :accessor file-name      :initarg :file-name   :initform nil :documentation "Имя файла в файловой системе")
   (file-descr        :accessor file-descr                           :initform nil :documentation "Файл тренда")
   (id-string         :accessor id-string                            :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (version           :accessor version                              :initform nil :documentation "Версия тренда")
   (utime-start       :accessor utime-start                          :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (reserv            :accessor reserv                               :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (records           :accessor records                              :initform nil :documentation "Общее число записей в тренде")
   (increment         :accessor increment                            :initform nil :documentation "Интервал между записями тренда, с")
   (analog-number     :accessor a-number                             :initform nil :documentation "Количество аналоговых сигналов")
   (discret-number    :accessor d-number                             :initform nil :documentation "Количество дискретных сигналов")
   (analog-ht         :accessor analog-ht                            :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (discret-ht        :accessor discret-ht                           :initform nil :documentation "Хеш-таблица дискретных сигналов"))
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
  (format stream "Path= ~S~%" (file-name trd) )
  (when (file-descr trd)
    (format stream "id=~S version=~A " (id-string trd) (version trd))
    (format stream "[ ")
    (day-time (utime-start trd) :stream stream)
    (format stream " ; ")
    (day-time (utime-end trd) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (reserv trd) (records trd) (increment trd) (a-number trd) (d-number trd))
    (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (analog-ht trd) )
    (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (discret-ht trd) )))

(defmethod trd-open ((trd <trd>))
  "@b(Описание:) trd-open выполняет открытие файла тренда включая:
@begin(list)
 @item(чтение заголовка;)
 @item(разбор аналоговых сигналов;)
@item(разбор дискретных сигналов.)
@end(list)
"
  (trd-read-header trd)
  (read-analog-ht trd )
  (read-discret-ht trd)
  trd)

(defmethod trd-read-header ((trd <trd>))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (file-descr trd))
    (setf (file-descr trd) (open-b-read (file-name trd)))
    (let ((in (file-descr trd))
          (bufer nil)
          (date-day nil)
          (date-month nil)
          (date-year nil)
          (time-hour nil)
          (time-minute nil)
          (time-second nil))
      (setf (id-string trd)        (b-read-string in +head-id-wid+)
	    (version trd)          (car (b-read in +head-version-wid+))
	    bufer                  (b-read in +head-date-wid+)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (b-read in +head-time-wid+)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (utime-start trd)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (reserv trd)           (b-read-short in)
	    (records trd)          (b-read-long in)
	    (increment trd)        (b-read-double in)
	    (a-number trd)         (b-read-short in)
	    (d-number trd)         (b-read-short in))
      (setf (records trd)
	    (/ (- (file-length (file-descr trd)) (start-offset trd))
	       (record-length trd)))))
  trd)

(defmethod read-analog-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-analog-ht) выполняет разбор аналоговых сигналов."
  (when (null (analog-ht trd))
    (setf (analog-ht trd)  (make-hash-table :test #'equal :size (a-number trd)))
    (file-position (file-descr trd) +head-wid+)
    (let ((in (file-descr trd))
          (analog-id nil)
          (analog-description nil)
          (analog-units  nil)
          (analog-min nil)
          (analog-max nil))
      (dotimes (i (a-number trd) 'done)
	(setf analog-id          (b-read-string in +signal-id-wid+) 
	      analog-description (b-read-string in +signal-description-wid+)
	      analog-units       (b-read-string in +signal-units-wid+)
	      analog-min         (b-read-double in)
	      analog-max         (b-read-double in)
	      (gethash analog-id (analog-ht trd)) (make-instance
                                                   '<a-signal>
						   :num i
						   :id  analog-id
						   :description analog-description
						   :units analog-units
						   :min analog-min
						   :max analog-max))))))

(defmethod read-discret-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-discret-ht) выполняет разбор дискретных сигналов."
  (when (null (discret-ht trd))
    (setf (discret-ht trd) (make-hash-table :test #'equal :size (d-number trd)))
    (file-position (file-descr trd) (+ +head-wid+ (* (a-number trd) +analog-wid+)))
    (let ((in (file-descr trd)) (discret-id nil) (discret-description nil))
      (dotimes (i (d-number trd) 'done)
	(setf discret-id          (b-read-string in +signal-id-wid+)
	      discret-description (b-read-string in +signal-description-wid+)
	      (gethash discret-id (discret-ht trd)) (make-instance
                                                     '<d-signal>
						     :num i
						     :id discret-id
						     :description discret-description))))))

(defmethod trd-close ((trd <trd>))
  "@b(Описание:) метод @b(trd-close) выполняет закрытие файла тренда."
  (when (file-descr trd)
    (close (file-descr trd))
    (setf (file-descr trd) nil)))

(defmethod start-offset ((trd <trd>))
  "@b(Описание:) метод @b(start-offset) возвращает смещение, 
выраженное в байтах, первой (нулевой) записи тренда."
  (+ +head-wid+
     (* (a-number trd) +analog-wid+)
     (* (d-number trd) +discret-wid+)))

(defmethod analog-length ((trd <trd>))
  "@b(Описание:) метод @b(analog-length) возвращает 
длину занимаемую аналоговыми сигналами одной записи тренда.
"
  (* (a-number trd) 2))

(defmethod discret-length ((trd <trd>))
  "@b(Описание:) метод @b(discret-length) возвращает количество байт
необходимое для записи дискретных сигналов одной записи."
  (ceiling (/ (d-number trd) 8)))

(defmethod record-length ((trd <trd>))
  "@b(Описание:) метод @b(record-length) возвращает
длину одной записи тренда."
  (+  (analog-length trd)  (discret-length trd)))

(defmethod discret-offset ((trd <trd>))
  "@b(Описание:) метод @b(discret-offset) возвращает смещение в байтах
от начала записи до начала записи дискретных сигналов."
  (analog-length trd))


(defmethod utime->record ((trd <trd>) utime)
  "@b(Описание:) метод @b(utime->record) возвращает номер
 записи по универсальному времени."
  (floor
   (- utime (utime-start trd))
   (increment trd)))

(defmethod record->utime ((trd <trd>) record)
  "@b(Описание:) метод @b(record->utime) возвращает время в
универсальном формате по номеру записи."
  (+ (utime-start trd)
     (floor record
            (/ (increment trd)))))

(defmethod utime-end ((trd <trd>))
  "@b(Описание:) метод @b(utime-end) возвращает время окончания тренда.
Время возвращается в универсальном формате (universal-time)"
  (record->utime trd (records trd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun time-universal-encode (year month day hour min sec)
  "@b(Описание:) функция @b(time-universal-encode) возвращает время в
  универсальном формате. Аналогична вызову функции
  @b(encode-universal-time) с параметрами следующими в обратном
  порядке.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (time-universal-encode 2021 08 30 10 00 00 ) => 3839295600
@end(code)"
  (encode-universal-time sec min hour day month year))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-fname*
  (concatenate 'string
	       (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
  "Для примеров.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd* (make-instance '<trd> :file-name *trd-fname*))

(trd-open *trd*)
