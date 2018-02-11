;;;; classes.lisp

(in-package #:recoder)

(defclass a-signal ()
  ((a-signal-num         :accessor a-signal-num         :initarg :a-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (a-signal-id          :accessor a-signal-id          :initarg :a-signal-id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (a-signal-description :accessor a-signal-description :initarg :a-signal-description :initform nil :documentation "Описание сигнала, char[40]")
   (a-signal-units       :accessor a-signal-units       :initarg :a-signal-units       :initform nil :documentation "Размерность аналогового сигнала, char[8]")
   (a-signal-min         :accessor a-signal-min         :initarg :a-signal-min         :initform nil :documentation "Нижняя граница диапазона аналогового сигнала, double = char[8]")
   (a-signal-max         :accessor a-signal-max         :initarg :a-signal-max         :initform nil :documentation "Верхняя граница диапазона аналогового сигнала, double = char[8]"))
  (:documentation "Дескриптор (описание) аналогового сигнала
Запись дескриптора аналогового сигнала имеет следующую структуру:
|--------------------+-------+-------------------------------------|
| Поле               | Длина | Примечание                          |
|                    |  поля |                                     |
|--------------------+-------+-------------------------------------|
| analog-id          |    10 | Обозначение аналогового сигнала     |
| analog-description |    40 | Описание аналогового сигнала        |
| analog-units       |     8 | Размернсть аналогового сигнала      |
| analog-LowLimit    |     8 | Нижняя граница аналогового сигнала  |
| analog-HighLimit   |     8 | Верхняя граница аналогового сигнала |
|                    |       |                                     |
|--------------------+-------+-------------------------------------|"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass d-signal ()
  ((d-signal-num         :accessor d-signal-num         :initarg :d-signal-num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (d-signal-id          :accessor d-signal-id          :initarg :d-signal-id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (d-signal-description :accessor d-signal-description :initarg :d-signal-description :initform nil :documentation "Описание сигнала, char[40]"))
  (:documentation "Дескриптор (описание) дискретного сигнала
Запись дескриптора аналогового сигнала во внутреннем представлении
файло-тренда имеет следующую структуру:
|---------------------+-------+---------------------------------|
| Поле                | Длина | Примечание                      |
|                     | поля, |                                 |
|                     | байт  |                                 |
|---------------------+-------+---------------------------------|
| discret-id          | 10    | Обозначение дискретного сигнала |
| discret-description | 40    | Описание дискретного сигнала    |
|---------------------+-------+---------------------------------|
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass trd ()
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
  (:documentation "trd - класс служащий для предоставления интерфейса к файлу-тренду, содержащему записи аналоговых и дискретных параметров;
Файл-тренд состоит из:
1 Записи заголовка тренда;
Заголовок тренда имеет следующую структуру:
|----------------+-------+----------------------------------------------------------------------------|
| Поле           |  Дина | Примечание                                                                 |
|                | поля, |                                                                            |
|                |  байт |                                                                            |
|----------------+-------+----------------------------------------------------------------------------|
| id             |     5 | Строка идентификации                                                       |
| version        |     1 | Версия данных трендера                                                     |
| date-day       |     1 | Число месяца                                                               |
| date-month     |     1 | Порядковый номер месяца                                                    |
| date-year      |     1 | Год-2000                                                                   |
| time-hour      |     1 | Час                                                                        |
| time-minute    |     1 | Минута                                                                     |
| time-second    |     1 | Секунда                                                                    |
| reserv         |     2 | Резерв -- содержит сумму аналоговых и дискретных сигналов до 2^16=65536 шт |
| total-records  |     4 | Количество записей, содержащееся в тренде до 2^32=4294967296 шт            |
| delta-time     |     8 | Интервал времени между записями, с                                         |
| analog-number  |     2 | Количество аналоговых сигналов до 2^16=65536 шт                            |
| discret-number |     2 | Количество дискретных сигналов до 2^16=65536 шт                            |
|----------------+-------+----------------------------------------------------------------------------|
2 Записей дескрипторов (описаний) аналоговых сигналов, см. a-signal;
3 Записей дескрипторов (описаний) дискретных сигналов, см. d-signal; 
4 Записей анналоговых и дискретных сигналов, состоящий из последовательно записанных списка аналоговых сигналов и упакованного списка дискретных сигналов;
4.1 Каждый аналоговый сигнал кодируется целочисленным значением длиной 2 байта,
его вычисляется по формуле: rez=analog-LowLimit+(i*(analog-HighLimit-analog-LowLimit)/65535)
4.2 Каждый дискретный сигнал кодируется одним битом информации 0|1;
При записи в тренд на восемь дискретных сигналов отводится один байт;
Сигналы упаковываются побайтно справа-направо"))
