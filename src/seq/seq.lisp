;;; ./src/trd-seq.lisp

(defpackage #:recoder/seq
  (:use #:cl
        #:mnas-string
        #:recoder/slist
        #:recoder/a-signal
        #:recoder/d-signal
        #:recoder/binary)
  (:nicknames "R/SEQ")
  (:export trd-open)
  (:export <trd-seq>
           <trd-seq>-a-sig
           <trd-seq>-d-sig
           <trd-seq>-s-sig
           )
  (:export export-to
           extract-signals
           *csv-stream*
           )
  (:intern update 
           sequence:length
           sequence:elt 
           elt-seq 
           sig 
           sig-on 
           sig-off 
           <trd-seq>-units 
           ))

(in-package #:recoder/seq)

(defclass <trd-seq> (recoder/trd:<trd> sequence)
  ((s-sig :reader   <trd-seq>-s-sig :initform nil :initarg :s-sig :documentation "Список с именами сигналов.")
   (a-sig :accessor <trd-seq>-a-sig :initform nil :documentation "Список аналоговых сигналов.")
   (d-sig :accessor <trd-seq>-d-sig :initform nil :documentation "Список дискретных сигналов.")
   (h-tbl :accessor <trd-seq>-h-tbl :initform (make-hash-table :test #'equal) :documentation
          "Хешированная таблица: 
@begin(list)
 @item(ключ - имя сигнала;)
 @item(значение - номер сигнала в записи  Список дискретных сигналов.)
@end(list) "))
  (:documentation "@b(Описание:) класс @b(<trd-seq>) реализует
 протоколы доступа к записям тренда через протоколы доступа к
 элементам последовательности.
"))

(defmethod update ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(update) 
"
  (unless (recoder/trd:<trd>-file-descr trd-seq) (recoder/trd:trd-open trd-seq))
  (let ((a-sig (recoder/slist:a-signals trd-seq (<trd-seq>-s-sig trd-seq)))
        (d-sig (recoder/slist:d-signals trd-seq (<trd-seq>-s-sig trd-seq))))
    (setf (<trd-seq>-a-sig trd-seq) a-sig)
    (setf (<trd-seq>-d-sig trd-seq) d-sig)
    (with-slots (s-sig) trd-seq
      (setf s-sig
            (append (mapcar #'recoder/a-signal:<a-signal>-id (<trd-seq>-a-sig trd-seq))
                    (mapcar #'recoder/d-signal:<d-signal>-id (<trd-seq>-d-sig trd-seq)))))
    (clrhash (<trd-seq>-h-tbl trd-seq))
    (loop :for s :in (<trd-seq>-s-sig trd-seq)
	  :for i :from 0 :below (length (<trd-seq>-s-sig trd-seq))
	  :do  (setf (gethash s (<trd-seq>-h-tbl trd-seq)) i ))
    trd-seq))

(defmethod trd-open ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(trd-open) выполняет отркытие файла тренда,
ассоциированного с объектом @b(trd-seq).
"
  (recoder/trd:trd-open trd-seq))

(defmethod (setf <trd-seq>-s-sig) (new-value (trd-seq <trd-seq>))
  "@b(Описание:) метод @b(setf <trd-seq>-s-sig)
"
  (unless (recoder/trd:<trd>-file-descr trd-seq) (recoder/trd:trd-open trd-seq))
  (with-slots (s-sig) trd-seq
    (setf s-sig new-value)
    (update trd-seq)))

(defmethod sequence:length ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(sequence:length)
"
  (unless (recoder/trd:<trd>-file-descr trd-seq) (recoder/trd:trd-open trd-seq))
  (recoder/trd:<trd>-total-records trd-seq))

(defmethod sequence:elt ((trd-seq <trd-seq>) index)
  "@b(Описание:) метод @b(sequence:elt)
"
  (unless (recoder/trd:<trd>-file-descr trd-seq) (recoder/trd:trd-open trd-seq))
  (let ((a-sig (<trd-seq>-a-sig trd-seq))
        (d-sig (<trd-seq>-d-sig trd-seq)))
    (coerce
     (append #+nil (list (recoder/trd:record->utime trd-seq index))
             (when a-sig (recoder/get:trd-analog-by-record  trd-seq index a-sig))
             (when d-sig (recoder/get:trd-discret-by-record trd-seq index d-sig)))
     'vector)))

(defmethod trd-open :after ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(trd-open :after)
"
  (update trd-seq))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod elt-seq ((trd-seq <trd-seq>) start end)
  "@b(Описание:) метод @b(elt-seq) возвращает средние значения из тренда
@b(trd-seq) в диапазоне записей от @b(start) включительно до @b(end) 
исключительно.
"
  (math/list-matr:average-col-value
   (loop :for i :from start :below end
	 :collect (coerce (elt trd-seq i) 'list))))

;;;;;;;;;;

;; (defparameter *trd-seq* (make-instance '<trd-seq> :file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_100354.trd" :s-sig *s-001*))

;; (elt-seq  *trd-seq* (- 13355 35) (- 13355 15))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sig     (key data (trd-seq <trd-seq>))
  "@b(Описание:) метод @b(sig)
"
  (svref data (gethash key (<trd-seq>-h-tbl trd-seq))))

(defmethod sig-on  (key data (trd-seq <trd-seq>))
  "@b(Описание:) метод @b(sig-on)
"
  (= 1 (sig key data trd-seq)))

(defmethod sig-off (key data (trd-seq <trd-seq>))
    "@b(Описание:) метод @b(sig-off)
"
  (= 0 (sig key data trd-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <trd-seq>-units ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(<trd-seq>-units)
"
  (append
   (mapcar #'recoder/a-signal:<a-signal>-units (<trd-seq>-a-sig trd-seq))
   (loop :for i :in (<trd-seq>-d-sig trd-seq)
	 :collect "0/1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Export-to ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric export-to (object stream &key start end by)
  (:documentation "Выводит содержимое объекта b(object) в поток @b(stream)."))

(defclass <format-stream> ()
  ((external-format :accessor <format-stream>-external-format :initform :cp1251 :initarg :external-format))
  (:documentation "@b(Описание:)  класс @b(<format-stream>) для вывода в csv формате."))

;;;;;;;;;;

(defclass <csv-stream> (<format-stream>)
  ()
  (:documentation "@b(Описание:) класс @b(<csv-stream>) для вывода в csv формате."))

;;;;;;;;;;

(defparameter *csv-stream* (make-instance '<csv-stream> :external-format :cp1251)
  "@b(Описание:) переменная @b(*csv-stream*)
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CVS export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod export-to ((trd-seq <trd-seq>) (csv-stream <csv-stream>)
                      &key
                        (start 0)
                        (end (recoder/trd:<trd>-total-records trd-seq))
                        (by 1))
  "@b(Описание:) метод @b(export-to) выполняет вывод объекта @b(trd-seq) в
поток @b(csv-stream).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-open *trd-sig*)
 (export-to *trd-sig* *csv-stream*)
@end(code)
"
  (with-open-file (os (concatenate 'string (recoder/trd:<trd>-file-name trd-seq) ".csv")
		      :direction :output :if-exists :supersede
		      :external-format (<format-stream>-external-format csv-stream))
    (format os "Time;NUM;~{~,4F~^;~}~%" (<trd-seq>-s-sig trd-seq))
    (format os "~{~,S~^;~}~%" (append '("hh:mm:ss" "NUM") (<trd-seq>-units trd-seq)))
    (loop :for i :from start :below end :by by
	  :do (format os "~S;~A;~{~,4F~^;~}~%"
		      (mnas-org-mode:utime->time (recoder/trd:record->utime trd-seq i))
		      i
		      (coerce (elt trd-seq i) 'list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-signals (fname signals &key (by 5) (start 0))
  "@b(Описание:) функция @b(extract-signals) выводит значения сигналов
  @b(signals), записанных в файл тренда с именем @b(fname).
"
  (let ((trd-seq (make-instance '<trd-seq> :file-name fname :s-sig signals)))
    (trd-open trd-seq)
    (export-to trd-seq *csv-stream* :by by :start start)))



