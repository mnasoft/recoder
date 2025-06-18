;;;; ./src/trd/trd.lisp

(defpackage :recoder/trd
  (:use #:cl)
  (:nicknames "R/TRD")
  (:export trd-open
           trd-close
           )
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
           <trd>-oc-i-sream  ;; oc-i-sream 
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
  (:export a-signal-list
           d-signal-list
           )
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

(defconstant +signal-LowLimit-wid+      8 "Ширина поля для нижней  границы диапазона аналогового сигнала, double = char[8]")

(defconstant +signal-HighLimit-wid+     8 "Ширина поля для верхней границы диапазона аналогового сигнала, double = char[8]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +analog-wid+ (+ r/c:+signal-id-wid+
                             r/c:+signal-description-wid+
                             r/c:+signal-units-wid+
                             +signal-LowLimit-wid+
                             +signal-HighLimit-wid+)
  "Длина заголовка одной записи аналогового сигнала")

(defconstant +discret-wid+ (+ r/c:+signal-id-wid+
                              r/c:+signal-description-wid+ )
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

(defmethod print-object ((trd <trd>) stream)
  (format stream "Path= ~S~%" (<trd>-file-name trd) )
  (when t #+nil (<trd>-file-descr trd)
        (format stream "id=~S version=~A " (<trd>-id-string trd) (<trd>-version trd))
        (when (<trd>-utime-start trd) 
          (format stream "[ ")
          (mnas-string/print:day-time (<trd>-utime-start trd) :stream stream)
          (format stream " ; ")
          (mnas-string/print:day-time (utime-end trd) :stream stream)
          (format stream " ]"))
        (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	        (<trd>-reserv trd) (<trd>-records trd) (<trd>-increment trd) (<trd>-a-number trd) (<trd>-d-number trd))
        (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
        (alexandria:maphash-values #'(lambda (v) (format stream "~S~%" v)) (<trd>-analog-ht trd) )
        (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
        (alexandria:maphash-values #'(lambda (v) (format stream "~S~%" v)) (<trd>-discret-ht trd) )))

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
            (<trd>-reserv trd)           (r/bin:b-read-ushort in)
            (<trd>-records trd)          (r/bin:b-read-uint in)
            (<trd>-increment trd)        (r/bin:b-read-double in)
            (<trd>-a-number trd)         (r/bin:b-read-ushort in)
            (<trd>-d-number trd)         (r/bin:b-read-ushort in))
      (setf (<trd>-records trd)
            (/ (- (file-length (<trd>-file-descr trd)) (start-offset trd))
               (record-length trd)))))
  trd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod r/g:read-obj ((trd <trd>) in)
  (block header 
    (setf (<trd>-id-string trd) (r/bin:b-read-string in +head-id-wid+))
    (setf (<trd>-version trd)   (r/bin:b-read-uchar in))
    (let ((date-day             (r/bin:b-read-uchar in))
          (date-month           (r/bin:b-read-uchar in))
          (date-year             (+ 2000 (r/bin:b-read-uchar in)))
	  (time-hour            (recoder/binary:b-read-uchar in))
	  (time-minute          (r/bin:b-read-uchar in))
	  (time-second          (r/bin:b-read-uchar in)))
      (setf (<trd>-utime-start trd)
            (encode-universal-time time-second time-minute time-hour
                                   date-day date-month date-year)))
    (setf (<trd>-reserv trd)           (r/bin:b-read-ushort in))
    (setf (<trd>-records trd)          (r/bin:b-read-uint in)) ;; Считали как правило 0
    (setf (<trd>-increment trd)        (r/bin:b-read-double in))
    (setf (<trd>-a-number trd)         (r/bin:b-read-ushort in))
    (setf (<trd>-d-number trd)         (r/bin:b-read-ushort in))
    (setf (<trd>-records trd) ;; Определили число записей
	  (/ (- (file-length in) (start-offset trd))
	     (record-length trd))))
  (block analog-ht
    (setf (<trd>-analog-ht trd)  (make-hash-table :test #'equal :size (<trd>-a-number trd)))
    (dotimes (i (<trd>-a-number trd) 'done)
      (let ((a-signal (make-instance 'r/a-sig:<a-signal> :num i)))
        (r/g:read-obj a-signal in)
        (setf (gethash (r/a-sig:<a-signal>-id a-signal) (<trd>-analog-ht trd))
              a-signal))))
  (block discret-ht
    (setf (<trd>-discret-ht trd)
          (make-hash-table :test #'equal :size (<trd>-d-number trd)))
    (dotimes (i (<trd>-d-number trd) 'done)
      (let ((d-signal (make-instance 'r/d-sig:<d-signal> :num i)))
        (r/g:read-obj d-signal in)
        (setf (gethash (r/d-sig:<d-signal>-id d-signal) (<trd>-discret-ht trd))
              d-signal))))
  (block data
    (with-open-stream (out (trivial-octet-streams:make-octet-output-stream))
      (loop :for i :from 0 :below (* (record-length trd) (<trd>-records trd))
            :do (r/bin:b-write-uchar
                 (r/bin:b-read-uchar in) out))
      (setf (<trd>-oc-i-sream trd)
            (trivial-octet-streams:make-octet-input-stream
             (trivial-octet-streams:get-output-stream-octets out))))))

(defmethod r/g:write-obj ((trd <trd>) out)
  (block header 
    (r/bin:b-write-string (r/trd:<trd>-id-string trd)
                          out (length (r/trd:<trd>-id-string trd)))
    (r/bin:b-write-uchar (<trd>-version trd) out )
    (multiple-value-bind (time-second time-minute time-hour
                          date-day date-month date-year)
        (decode-universal-time (<trd>-utime-start trd))
      (r/bin:b-write-uchar date-day out)
      (r/bin:b-write-uchar date-month out)
      (r/bin:b-write-uchar (- date-year 2000) out)
      (r/bin:b-write-uchar time-hour out)
      (r/bin:b-write-uchar	time-minute out)
      (r/bin:b-write-uchar	time-second out))
    (r/bin:b-write-ushort  (<trd>-reserv trd)    out)
    (r/bin:b-write-uint    0
                           #+nil(<trd>-records trd)
                           out)
    (r/bin:b-write-double (<trd>-increment trd) out)
    (r/bin:b-write-ushort  (<trd>-a-number trd)  out)
    (r/bin:b-write-ushort  (<trd>-d-number trd)  out))
  (block analog-ht
    (alexandria:maphash-values
     #'(lambda (a-signal) (r/g:write-obj a-signal out))
     (<trd>-analog-ht trd))
    )
  (block discret-ht
    (alexandria:maphash-values
     #'(lambda (d-signal) (r/g:write-obj d-signal out))
     (<trd>-discret-ht trd)))
  (block data
    (setf (trivial-octet-streams::index (<trd>-oc-i-sream trd))
          0)
    (loop :for i :from 0 :below (* (record-length trd) (<trd>-records trd))
            :do (r/bin:b-write-uchar
                 (r/bin:b-read-uchar (<trd>-oc-i-sream trd)) out))))

(defmethod r/g:write-obj ((trd <trd>) (path pathname))
  (when (string/= (pathname-type path) "trd")
    (error "~S" (pathname-type path)))
  (r/bin:with-open-file-b-out (out path)
    (r/g:write-obj trd out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
(defmethod read-analog-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-analog-ht) выполняет разбор аналоговых сигналов."
  (when (null (<trd>-analog-ht trd))
    (setf (<trd>-analog-ht trd)  (make-hash-table :test #'equal :size (<trd>-a-number trd)))
    (file-position (<trd>-file-descr trd) +head-wid+)
    (let ((in (<trd>-file-descr trd)))
      (dotimes (i (<trd>-a-number trd) 'done)
        (let ((a-signal (make-instance 'r/a-sig:<a-signal> :num i)))
          (r/g:read-obj a-signal in)
          (setf (gethash (r/a-sig:<a-signal>-id a-signal) (<trd>-analog-ht trd))
                a-signal))))))

(defmethod read-discret-ht ((trd <trd>))
  "@b(Описание:) метод @b(read-discret-ht) выполняет разбор дискретных сигналов."
  (when (null (<trd>-discret-ht trd))
    (setf (<trd>-discret-ht trd)
          (make-hash-table :test #'equal :size (<trd>-d-number trd)))
    (file-position (<trd>-file-descr trd)
                   (+ +head-wid+ (* (<trd>-a-number trd) +analog-wid+)))
    (let ((in (<trd>-file-descr trd)))
      (dotimes (i (<trd>-d-number trd) 'done)
        (let ((d-signal (make-instance 'r/d-sig:<d-signal> :num i)))
          (r/g:read-obj d-signal in)
          (setf (gethash (r/d-sig:<d-signal>-id d-signal) (<trd>-discret-ht trd))
                d-signal))))))

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


(defmethod a-signal-list ((trd <trd>))
  (when (<trd>-analog-ht trd)
    (sort (alexandria:hash-table-values (<trd>-analog-ht trd))
          #'< :key #'r/a-sig:<a-signal>-num)))

(defmethod d-signal-list ((trd <trd>))
  (when (<trd>-discret-ht trd)
    (sort (alexandria:hash-table-values (<trd>-discret-ht trd))
          #'< :key #'r/d-sig:<d-signal>-num)))
