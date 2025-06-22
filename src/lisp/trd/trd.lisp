;;;; ./src/trd/trd.lisp

(defpackage :recoder/trd
  (:use #:cl)
  (:nicknames "R/TRD")
  (:export trd-open)
  (:export analog-length
           discret-length
           discret-offset
           start-offset
           record-length
           utime-end)  
  (:export record->utime
           utime->record)
  (:export time-universal-encode)
  (:export *Convert-Excel-To-Txt-ps1*
           fname-xls->trd
           fname-xls->txt
           )
  (:intern read-analog-ht
           read-discret-ht)
  (:documentation
   "@b(Описание:) пакет @b(recoder/trd)"))

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

(defconstant +analog-wid+ (+ r/const:+signal-id-wid+
                             r/const:+signal-description-wid+
                             r/const:+signal-units-wid+
                             +signal-LowLimit-wid+
                             +signal-HighLimit-wid+)
  "Длина заголовка одной записи аналогового сигнала")

(defconstant +discret-wid+ (+ r/const:+signal-id-wid+
                              r/const:+signal-description-wid+ )
  "Длина заголовка одной записи дискретного сигнала")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *trd-print-format* :long)

(defun trd-print-format ()
  *trd-print-format*)

(defun (setf trd-print-format) (value)
  (declare (type (member :long :short) value))
  (setf *trd-print-format* value))

(setf (trd-print-format) :short)
#+nil (setf (trd-print-format) :long)

(defmethod print-object ((trd r/c:<trd>) stream)
  (print-unreadable-object (trd stream :type t :identity t)  
    (when (eq (trd-print-format) :long)
      (format stream "~%")
      (format stream "Path           = ~S~%" (r/c:<trd>-file-name trd))
      (format stream "id             = ~S~%" (r/c:<trd>-id-string trd))
      (format stream "version        = ~A~%" (r/c:<trd>-version trd))
      (when (r/c:<trd>-utime-start trd)
        (format stream "date           = " )
        (mnas-string/print:date (r/c:<trd>-utime-start trd) :stream stream)
        (format stream "~%" )
        (format stream "time           = [ ")
        (mnas-string/print:day-time (r/c:<trd>-utime-start trd) :stream stream)
        (format stream " ; ")
        (mnas-string/print:day-time (utime-end trd) :stream stream)
        (format stream " ]~%"))
      (format stream "Reserv         = ~A~%" (r/c:<trd>-reserv trd))
      (format stream "Total-records  = ~A~%" (r/c:<trd>-records trd))
      (format stream "Delta-time     = ~A~%" (r/c:<trd>-increment trd))
      (format stream "Analog-number  = ~A~%" (r/c:<trd>-a-number trd)) 
      (format stream "Discret-number = ~A~%" (r/c:<trd>-d-number trd))
      (when (r/c:<trd>-analog-ht trd)
        (format stream "==================================================~%")
        (format stream "            Перечень аналоговых сигналов          ~%")
        (format stream "==================================================~%")
        (alexandria:maphash-values
         #'(lambda (v)
             (format stream "~S~%" v))
         (r/c:<trd>-analog-ht trd)))
      (when (r/c:<trd>-discret-ht trd)
        (format stream "==================================================~%")
        (format stream "            Перечень дискретных сигналов          ~%")
        (format stream "==================================================~%")
        (alexandria:maphash-values
         #'(lambda (v)
             (format stream "~S~%" v))
         (r/c:<trd>-discret-ht trd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod r/g:read-obj ((trd r/c:<trd>) in)
  (block header 
    (setf (r/c:<trd>-id-string trd) (m-bin:b-read-string in +head-id-wid+))
    (setf (r/c:<trd>-version trd)   (m-bin:b-read-uchar in))
    (let ((date-day             (m-bin:b-read-uchar in))
          (date-month           (m-bin:b-read-uchar in))
          (date-year             (+ 2000 (m-bin:b-read-uchar in)))
	  (time-hour            (m-bin:b-read-uchar in))
	  (time-minute          (m-bin:b-read-uchar in))
	  (time-second          (m-bin:b-read-uchar in)))
      (setf (r/c:<trd>-utime-start trd)
            (encode-universal-time time-second time-minute time-hour
                                   date-day date-month date-year)))
    (setf (r/c:<trd>-reserv trd)           (m-bin:b-read-ushort in))
    (setf (r/c:<trd>-records trd)          (m-bin:b-read-uint in)) ;; Считали как правило 0
    (setf (r/c:<trd>-increment trd)        (m-bin:b-read-double in))
    (setf (r/c:<trd>-a-number trd)         (m-bin:b-read-ushort in))
    (setf (r/c:<trd>-d-number trd)         (m-bin:b-read-ushort in))
    (setf (r/c:<trd>-records trd) ;; Определили число записей
	  (/ (- (file-length in) (start-offset trd))
	     (record-length trd))))
  (block analog-ht
    (setf (r/c:<trd>-analog-ht trd)  (make-hash-table :test #'equal :size (r/c:<trd>-a-number trd)))
    (dotimes (i (r/c:<trd>-a-number trd) 'done)
      (let ((a-signal (make-instance 'r/c:<a-signal> :num i)))
        (r/g:read-obj a-signal in)
        (setf (gethash (r/c:<a-signal>-id a-signal) (r/c:<trd>-analog-ht trd))
              a-signal))))
  (block discret-ht
    (setf (r/c:<trd>-discret-ht trd)
          (make-hash-table :test #'equal :size (r/c:<trd>-d-number trd)))
    (dotimes (i (r/c:<trd>-d-number trd) 'done)
      (let ((d-signal (make-instance 'r/c:<d-signal> :num i)))
        (r/g:read-obj d-signal in)
        (setf (gethash (r/c:<d-signal>-id d-signal) (r/c:<trd>-discret-ht trd))
              d-signal))))
  (block data
    (with-open-stream (out (trivial-octet-streams:make-octet-output-stream))
      (loop :for i :from 0 :below (* (record-length trd) (r/c:<trd>-records trd))
            :do (m-bin:b-write-uchar
                 (m-bin:b-read-uchar in) out))
      (setf (r/c:<trd>-oc-i-sream trd)
            (trivial-octet-streams:make-octet-input-stream
             (trivial-octet-streams:get-output-stream-octets out))))))

(defmethod r/g:write-obj ((trd r/c:<trd>) out)
  (block header 
    (m-bin:b-write-string (r/c:<trd>-id-string trd)
                          out (length (r/c:<trd>-id-string trd)))
    (m-bin:b-write-uchar (r/c:<trd>-version trd) out )
    (multiple-value-bind (time-second time-minute time-hour
                          date-day date-month date-year)
        (decode-universal-time (r/c:<trd>-utime-start trd))
      (m-bin:b-write-uchar date-day out)
      (m-bin:b-write-uchar date-month out)
      (m-bin:b-write-uchar (- date-year 2000) out)
      (m-bin:b-write-uchar time-hour out)
      (m-bin:b-write-uchar	time-minute out)
      (m-bin:b-write-uchar	time-second out))
    (m-bin:b-write-ushort  (r/c:<trd>-reserv trd)    out)
    (m-bin:b-write-uint    0
                           #+nil(r/c:<trd>-records trd)
                           out)
    (m-bin:b-write-double (r/c:<trd>-increment trd) out)
    (m-bin:b-write-ushort  (r/c:<trd>-a-number trd)  out)
    (m-bin:b-write-ushort  (r/c:<trd>-d-number trd)  out))
  (block analog-ht
    (alexandria:maphash-values
     #'(lambda (a-signal) (r/g:write-obj a-signal out))
     (r/c:<trd>-analog-ht trd))
    )
  (block discret-ht
    (alexandria:maphash-values
     #'(lambda (d-signal) (r/g:write-obj d-signal out))
     (r/c:<trd>-discret-ht trd)))
  (block data
    (setf (trivial-octet-streams::index (r/c:<trd>-oc-i-sream trd))
          0)
    (loop :for i :from 0 :below (* (record-length trd) (r/c:<trd>-records trd))
            :do (m-bin:b-write-uchar
                 (m-bin:b-read-uchar (r/c:<trd>-oc-i-sream trd)) out))))

(defmethod r/g:write-obj ((trd r/c:<trd>) (path pathname))
  (when (string/= (pathname-type path) "trd")
    (error "~S" (pathname-type path)))
  (m-bin:with-open-file-b-out (out path)
    (r/g:write-obj trd out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
(defmethod start-offset ((trd r/c:<trd>))
  "@b(Описание:) метод @b(start-offset) возвращает смещение, 
выраженное в байтах, первой (нулевой) записи тренда."
  (+ +head-wid+
     (* (r/c:<trd>-a-number trd) +analog-wid+)
     (* (r/c:<trd>-d-number trd) +discret-wid+)))

(defmethod analog-length ((trd r/c:<trd>))
  "@b(Описание:) метод @b(analog-length) возвращает 
длину занимаемую аналоговыми сигналами одной записи тренда.
"
  (* (r/c:<trd>-a-number trd) 2))

(defmethod discret-length ((trd r/c:<trd>))
  "@b(Описание:) метод @b(discret-length) возвращает количество байт
необходимое для записи дискретных сигналов одной записи."
  (ceiling (/ (r/c:<trd>-d-number trd) 8)))

(defmethod record-length ((trd r/c:<trd>))
  "@b(Описание:) метод @b(record-length) возвращает
длину одной записи тренда."
  (+  (analog-length trd)  (discret-length trd)))

(defmethod discret-offset ((trd r/c:<trd>))
  "@b(Описание:) метод @b(discret-offset) возвращает смещение в байтах
от начала записи до начала записи дискретных сигналов."
  (analog-length trd))


(defmethod utime->record ((trd r/c:<trd>) utime)
  "@b(Описание:) метод @b(utime->record) возвращает номер
 записи по универсальному времени."
  (floor
   (- utime (r/c:<trd>-utime-start trd))
   (r/c:<trd>-increment trd)))

(defmethod record->utime ((trd r/c:<trd>) record)
  "@b(Описание:) метод @b(record->utime) возвращает время в
универсальном формате по номеру записи."
  (+ (r/c:<trd>-utime-start trd)
     (floor record
            (/ (r/c:<trd>-increment trd)))))

(defmethod utime-end ((trd r/c:<trd>))
  "@b(Описание:) метод @b(utime-end) возвращает время окончания тренда.
Время возвращается в универсальном формате (universal-time)"
  (record->utime trd (r/c:<trd>-records trd)))

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


(defmethod r/g:a-signal-list ((trd r/c:<trd>))
  (when (r/c:<trd>-analog-ht trd)
    (sort (alexandria:hash-table-values (r/c:<trd>-analog-ht trd))
          #'< :key #'r/c:<a-signal>-num)))

(defmethod r/g:d-signal-list ((trd r/c:<trd>))
  (when (r/c:<trd>-discret-ht trd)
    (sort (alexandria:hash-table-values (r/c:<trd>-discret-ht trd))
          #'< :key #'r/c:<d-signal>-num)))

(defmethod trd-open ((trd r/c:<trd>))
  (r/g:read-obj trd (r/c:<trd>-file-name trd)))
