;;;; a-signal-defmethods.lisp

(defpackage #:recoder/classes
  (:use #:cl)
  (:nicknames "R/C")
  (:export <d-signal>
           <d-signal>-num
           <d-signal>-id
	   <d-signal>-description
           )
  (:export <a-signal>
	   <a-signal>-units
	   <a-signal>-num
           <a-signal>-id
           <a-signal>-description
	   <a-signal>-min
           <a-signal>-max )
  (:export <trd>
           <trd>-file-name   ;; file-name
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
           <trd>-oc-i-sream  ;; oc-i-sream 
           )
  )

(in-package :recoder/classes)

(defclass <d-signal> ()
  ((num         :accessor <d-signal>-num         :initarg :num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (id          :accessor <d-signal>-id          :initarg :id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (description :accessor <d-signal>-description :initarg :description :initform nil :documentation "Описание сигнала, char[40]"))
  (:documentation "Дескриптор (описание) дискретного сигнала.

 Запись дескриптора аналогового сигнала во внутреннем представлении
файла-тренда имеет следующую структуру:

@begin(table)
@begin(row) @cell(Поле)               @cell(Длина поля)@cell(Примечание) @end(row)
@begin(row) @cell(discret-id)         @cell(10)        @cell(Обозначение дискретного сигнала) @end(row)
@begin(row) @cell(discret-description)@cell(40)        @cell(Описание дискретного сигнала) @end(row)
@end(table)"))

(defclass <a-signal> ()
  ((num         :accessor <a-signal>-num         :initarg :num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (id          :accessor <a-signal>-id          :initarg :id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (description :accessor <a-signal>-description :initarg :description :initform nil :documentation "Описание сигнала, char[40]")
   (units       :accessor <a-signal>-units       :initarg :units       :initform nil :documentation "Размерность аналогового сигнала, char[8]")
   (min         :accessor <a-signal>-min         :initarg :min         :initform float-features:double-float-positive-infinity
                :documentation "Нижняя граница диапазона аналогового сигнала, double = char[8]")
   (max         :accessor <a-signal>-max         :initarg :max         :initform float-features:double-float-negative-infinity
                :documentation "Верхняя граница диапазона аналогового сигнала, double = char[8]"))
  (:documentation "Дескриптор (описание) аналогового сигнала.

Запись дескриптора аналогового сигнала имеет следующую структуру:

@begin(table)
@begin(row) @cell(Поле)               @cell(Длина поля) @cell(Примечание) @end(row)
@begin(row) @cell(analog-id)          @cell(10)         @cell(Обозначение аналогового сигнала)     @end(row)
@begin(row) @cell(analog-description) @cell(40)         @cell(Описание аналогового сигнала)        @end(row)
@begin(row) @cell(analog-units)       @cell(8)          @cell(Размернсть аналогового сигнала)      @end(row)
@begin(row) @cell(analog-LowLimit)    @cell(8)          @cell(Нижняя граница аналогового сигнала)  @end(row)
@begin(row) @cell(analog-HighLimit)   @cell(8)          @cell(Верхняя граница аналогового сигнала) @end(row)
@end(table)

"))

(defclass <trd> ()
  ((file-name         :accessor <trd>-file-name :initarg :file-name  :initform nil :documentation "Имя файла в файловой системе")
   (id-string         :accessor <trd>-id-string                      :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (version           :accessor <trd>-version                        :initform nil :documentation "Версия тренда")
   (utime-start       :accessor <trd>-utime-start                    :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (reserv            :accessor <trd>-reserv                         :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (records           :accessor <trd>-records                        :initform nil :documentation "Общее число записей в тренде")
   (increment         :accessor <trd>-increment                      :initform nil :documentation "Интервал между записями тренда, с")
   (a-number          :accessor <trd>-a-number                       :initform nil :documentation "Количество аналоговых сигналов")
   (d-number          :accessor <trd>-d-number                       :initform nil :documentation "Количество дискретных сигналов")
   (analog-ht         :accessor <trd>-analog-ht                      :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (discret-ht        :accessor <trd>-discret-ht                     :initform nil :documentation "Хеш-таблица дискретных сигналов")
   (oc-i-sream        :accessor <trd>-oc-i-sream                     :initform nil :documentation "Поток чтения октетов, представляющих записи тренда")
   )
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
