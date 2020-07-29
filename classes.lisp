;;;; classes.lisp

(annot:enable-annot-syntax)

(in-package #:recoder)

@annot.class:export-class
@annot.class:export-accessors
(defclass <a-signal> ()
  ((a-signal-num         :accessor a-signal-num         :initarg :a-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (a-signal-id          :accessor a-signal-id          :initarg :a-signal-id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (a-signal-description :accessor a-signal-description :initarg :a-signal-description :initform nil :documentation "Описание сигнала, char[40]")
   (a-signal-units       :accessor a-signal-units       :initarg :a-signal-units       :initform nil :documentation "Размерность аналогового сигнала, char[8]")
   (a-signal-min         :accessor a-signal-min         :initarg :a-signal-min         :initform nil :documentation "Нижняя граница диапазона аналогового сигнала, double = char[8]")
   (a-signal-max         :accessor a-signal-max         :initarg :a-signal-max         :initform nil :documentation "Верхняя граница диапазона аналогового сигнала, double = char[8]"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@annot.class:export-class
@annot.class:export-accessors
(defclass <d-signal> ()
  ((d-signal-num         :accessor d-signal-num         :initarg :d-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (d-signal-id          :accessor d-signal-id          :initarg :d-signal-id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (d-signal-description :accessor d-signal-description :initarg :d-signal-description :initform nil :documentation "Описание сигнала, char[40]"))
  (:documentation "Дескриптор (описание) дискретного сигнала.

Запись дескриптора аналогового сигнала во внутреннем представлении
файло-тренда имеет следующую структуру:
@begin(table)
@begin(row) @cell(Поле)               @cell(Длина поля)@cell(Примечание) @end(row)
@begin(row) @cell(discret-id)         @cell(10)        @cell(Обозначение дискретного сигнала) @end(row)
@begin(row) @cell(discret-description)@cell(40)        @cell(Описание дискретного сигнала) @end(row)
@end(table)
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.class:export-class
@annot.class:export-accessors
(defclass <trd> ()
  ((trd-file-name         :accessor trd-file-name      :initarg :trd-file-name      :initform nil :documentation "Имя файла в файловой системе")
   (trd-file-descr        :accessor trd-file-descr                                  :initform nil :documentation "Файл тренда")
   (trd-id-string         :accessor trd-id-string                                   :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (trd-version           :accessor trd-version                                     :initform nil :documentation "Версия тренда")
   (trd-utime-start       :accessor trd-utime-start                                 :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (trd-reserv            :accessor trd-reserv                                      :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (trd-total-records     :accessor trd-total-records                               :initform nil :documentation "Общее число записей в тренде")
   (trd-delta-time        :accessor trd-delta-time                                  :initform nil :documentation "Интервал между записями тренда")
   (trd-analog-number     :accessor trd-analog-number                               :initform nil :documentation "Количество аналоговых сигналов")
   (trd-discret-number    :accessor trd-discret-number                              :initform nil :documentation "Количество дискретных сигналов")
   (trd-analog-ht         :accessor trd-analog-ht                                   :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (trd-discret-ht        :accessor trd-discret-ht                                  :initform nil :documentation "Хеш-таблица дискретных сигналов"))
  (:documentation "<trd> - класс служащий для предоставления интерфейса к файлу-тренду, содержащему записи аналоговых и дискретных параметров.

Файл-тренд состоит из:
@begin(enum)
@item(Записи заголовка тренда;)
@item(Записей дескрипторов (описаний) аналоговых сигналов, см. <a-signal>;)
@item(Записей дескрипторов (описаний) дискретных сигналов, см. <d-signal>;)
@item(Записей анналоговых и дискретных сигналов, состоящий из последовательно записанных списка аналоговых сигналов и упакованного списка дискретных сигналов;

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
