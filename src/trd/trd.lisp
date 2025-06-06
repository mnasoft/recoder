;;;; ./src/trd/trd.lisp

(defpackage :recoder/trd
  (:use #:cl)
  (:nicknames "R/TRD")
  (:export trd-open
           trd-close
           )
  (:export *trd*
           *trd-fname*)
  (:export <trd>
           <trd>-file-name   ;; file-name
           <trd>-file-descr  ;; file-descr
           <trd>-id-string   ;; id-string
           <trd>-version     ;; version
           <trd>-utime-start ;; utime-start
           <trd>-reserv      ;; reserv
	   <trd>-records     ;; records
	   <trd>-increment   ;; increment
           <trd>-a-number    ;; a-number 
           <trd>-d-number    ;; d-number
           <trd>-analog-ht   ;; analog-ht 
           <trd>-discret-ht  ;; discret-ht
           )
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
  (:documentation
   "@b(Описание:) пакет @b(recoder/trd)
")
  )

   "@begin(section) @title(Обзор)

 Пакет @b(Recoder/Trd) содержит:
 @b(функции:)
@begin(list)
 @item(@b(trd-open) - открытие файла тренда;)
 @item(@b(trd-close) - закрытие файла тренда;)
@end(list)

@end(section)
"

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
  ((file-name         :accessor <trd>-file-name :initarg :file-name  :initform nil :documentation "Имя файла в файловой системе")
   (file-descr        :accessor <trd>-file-descr                     :initform nil :documentation "Дескриптор файла-тренда")
   (id-string         :accessor <trd>-id-string                      :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (version           :accessor <trd>-version                        :initform nil :documentation "Версия тренда")
   (utime-start       :accessor <trd>-utime-start                    :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (reserv            :accessor <trd>-reserv                               :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (records           :accessor <trd>-records                              :initform nil :documentation "Общее число записей в тренде")
   (increment         :accessor <trd>-increment                            :initform nil :documentation "Интервал между записями тренда, с")
   (a-number          :accessor <trd>-a-number                       :initform nil :documentation "Количество аналоговых сигналов")
   (d-number          :accessor <trd>-d-number                       :initform nil :documentation "Количество дискретных сигналов")
   (analog-ht         :accessor <trd>-analog-ht                      :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (discret-ht        :accessor <trd>-discret-ht                     :initform nil :documentation "Хеш-таблица дискретных сигналов"))
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
    (mnas-string/print:day-time (<trd>-utime-start trd) :stream stream)
    (format stream " ; ")
    (mnas-string/print:day-time (utime-end trd) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (<trd>-reserv trd) (<trd>-records trd) (<trd>-increment trd) (<trd>-a-number trd) (<trd>-d-number trd))
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
  (read-analog-ht trd )
  (read-discret-ht trd)
  trd)

(defmethod trd-read-header ((trd <trd>))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (<trd>-file-descr trd))
    (setf (<trd>-file-descr trd) (r/bin:open-b-read (<trd>-file-name trd)))
    (let ((in (<trd>-file-descr trd))
          (bufer nil)
          (date-day nil)
          (date-month nil)
          (date-year nil)
          (time-hour nil)
          (time-minute nil)
          (time-second nil))
      (setf (<trd>-id-string trd)        (r/bin:b-read-string in +head-id-wid+)
	    (<trd>-version trd)          (car (r/bin:b-read in +head-version-wid+))
	    bufer                  (r/bin:b-read in +head-date-wid+)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (r/bin:b-read in +head-time-wid+)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (<trd>-utime-start trd)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (<trd>-reserv trd)           (r/bin:b-read-short in)
	    (<trd>-records trd)          (r/bin:b-read-long in)
	    (<trd>-increment trd)        (r/bin:b-read-double in)
	    (<trd>-a-number trd)         (r/bin:b-read-short in)
	    (<trd>-d-number trd)         (r/bin:b-read-short in))
      (setf (<trd>-records trd)
	    (/ (- (file-length (<trd>-file-descr trd)) (start-offset trd))
	       (record-length trd)))))
  trd)

(defmethod read-analog-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-analog-ht) выполняет разбор аналоговых сигналов."
  (when (null (<trd>-analog-ht trd))
    (setf (<trd>-analog-ht trd)  (make-hash-table :test #'equal :size (<trd>-a-number trd)))
    (file-position (<trd>-file-descr trd) +head-wid+)
    (let ((in (<trd>-file-descr trd))
          (analog-id nil)
          (analog-description nil)
          (analog-units  nil)
          (analog-min nil)
          (analog-max nil))
      (dotimes (i (<trd>-a-number trd) 'done)
	(setf analog-id          (r/bin:b-read-string in +signal-id-wid+)) 
	(setf analog-description (r/bin:b-read-string in +signal-description-wid+))
        (setf analog-units       (r/bin:b-read-string in +signal-units-wid+))
        (setf analog-min         (r/bin:b-read-double in))
        (setf analog-max         (r/bin:b-read-double in))
        (setf (gethash analog-id (<trd>-analog-ht trd))
              (make-instance 'r/a-sig:<a-signal> :num i
	                                         :id  analog-id
	                                         :description analog-description
	                                         :units analog-units
	                                         :min analog-min
	                                         :max analog-max))))))

(defmethod read-discret-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-discret-ht) выполняет разбор дискретных сигналов."
  (when (null (<trd>-discret-ht trd))
    (setf (<trd>-discret-ht trd) (make-hash-table :test #'equal :size (<trd>-d-number trd)))
    (file-position (<trd>-file-descr trd) (+ +head-wid+ (* (<trd>-a-number trd) +analog-wid+)))
    (let ((in (<trd>-file-descr trd)) (discret-id nil) (discret-description nil))
      (dotimes (i (<trd>-d-number trd) 'done)
	(setf discret-id          (r/bin:b-read-string in +signal-id-wid+))
	(setf discret-description (r/bin:b-read-string in +signal-description-wid+))
        (setf (gethash discret-id (<trd>-discret-ht trd))
              (make-instance 'r/d-sig:<d-signal> :num i
			                         :id discret-id
			                         :description discret-description))))))

(defmethod trd-close ((trd <trd>))
  "@b(Описание:) метод @b(trd-close) выполняет закрытие файла тренда."
  (when (<trd>-file-descr trd)
    (close (<trd>-file-descr trd))
    (setf (<trd>-file-descr trd) nil)))

(defmethod start-offset ((trd <trd>))
  "@b(Описание:) метод @b(start-offset) возвращает смещение, 
выраженное в байтах, первой (нулевой) записи тренда."
  (+ +head-wid+
     (* (<trd>-a-number trd) +analog-wid+)
     (* (<trd>-d-number trd) +discret-wid+)))

(defmethod analog-length ((trd <trd>))
  "@b(Описание:) метод @b(analog-length) возвращает 
длину занимаемую аналоговыми сигналами одной записи тренда.
"
  (* (<trd>-a-number trd) 2))

(defmethod discret-length ((trd <trd>))
  "@b(Описание:) метод @b(discret-length) возвращает количество байт
необходимое для записи дискретных сигналов одной записи."
  (ceiling (/ (<trd>-d-number trd) 8)))

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
   (- utime (<trd>-utime-start trd))
   (<trd>-increment trd)))

(defmethod record->utime ((trd <trd>) record)
  "@b(Описание:) метод @b(record->utime) возвращает время в
универсальном формате по номеру записи."
  (+ (<trd>-utime-start trd)
     (floor record
            (/ (<trd>-increment trd)))))

(defmethod utime-end ((trd <trd>))
  "@b(Описание:) метод @b(utime-end) возвращает время окончания тренда.
Время возвращается в универсальном формате (universal-time)"
  (record->utime trd (<trd>-records trd)))

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
