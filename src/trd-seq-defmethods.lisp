;;; ./src/trd-seq-defmethods.lisp

(in-package #:recoder)

(export '(<trd-seq> <trd-seq>-a-sig <trd-seq>-d-sig update <trd-seq>-s-sig))

(defclass <trd-seq> (<trd> sequence)
  ((s-sig :reader <trd-seq>-s-sig :initform nil :initarg :s-sig
          :documentation "Список с именами сигналов.")
   (a-sig :accessor <trd-seq>-a-sig :initform nil
          :documentation "Список аналоговых сигналов.")
   (d-sig :accessor <trd-seq>-d-sig :initform nil
          :documentation "Список дискретных сигналов.")
   (h-tbl :accessor <trd-seq>-h-tbl :initform (make-hash-table :test #'equal)
          :documentation "Хешированная таблица: 
@begin(list)
 @item(ключ - имя сигнала;)
 @item(значение - номер сигнала в записи  Список дискретных сигналов.)
@end(list) "))
  (:documentation "@b(Описание:) класс @b(<trd-seq>) реализует 
протоколы доступа к записям тренда через протоколы
доступа к элементам последовательности.
"))

(defmethod (setf <trd-seq>-s-sig) (new-value (trd-seq <trd-seq>))
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (with-slots (s-sig) trd-seq
    (setf s-sig new-value)
    (update trd-seq)))

(defmethod update ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(update) 
"
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (let ((sig (trd-separate-signals trd-seq (<trd-seq>-s-sig trd-seq))))
    (setf (<trd-seq>-a-sig trd-seq) (first  sig))
    (setf (<trd-seq>-d-sig trd-seq) (second sig))
    (with-slots (s-sig) trd-seq
      (setf s-sig
            (append (mapcar #'recoder/a-signal:a-signal-id (<trd-seq>-a-sig trd-seq))
                    (mapcar #'recoder/d-signal:d-signal-id (<trd-seq>-d-sig trd-seq)))))
    (clrhash (<trd-seq>-h-tbl trd-seq))
    (loop :for s :in (<trd-seq>-s-sig trd-seq)
	  :for i :from 0 :below (length (<trd-seq>-s-sig trd-seq))
	  :do  (setf (gethash s (<trd-seq>-h-tbl trd-seq)) i ))
    trd-seq))

(defmethod sequence:length ((trd-seq <trd-seq>))
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (trd-total-records trd-seq))

(defmethod sequence:elt ((trd-seq <trd-seq>) index)
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (let ((a-sig (<trd-seq>-a-sig trd-seq))
        (d-sig (<trd-seq>-d-sig trd-seq)))
    (coerce
     (append (when a-sig (trd-analog-by-rec-number  trd-seq index a-sig))
             (when d-sig (trd-discret-by-rec-number trd-seq index d-sig)))
     'vector)))

(defmethod trd-open :after ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(trd-open :after)
"
  (update trd-seq))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod elt-seq ((trd-seq <trd-seq>) start end)
  (math/list-matr:average-col-value
   (loop :for i :from start :below end
	 :collect (coerce (elt trd-seq i) 'list))))

;;;;;;;;;;

(defparameter *trd-seq*
  (make-instance '<trd-seq> :trd-file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_100354.trd"
			    :s-sig *s-001*))

(elt-seq  *trd-seq* (- 13355 35) (- 13355 15))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sig     (key data (trd-seq <trd-seq>))
  (svref data (gethash key (<trd-seq>-h-tbl trd-seq))))

(defmethod sig-on  (key data (trd-seq <trd-seq>))
  (= 1 (sig key data trd-seq)))

(defmethod sig-off (key data (trd-seq <trd-seq>))
  (= 0 (sig key data trd-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <trd-seq>-units ((trd-seq <trd-seq>))
  (append
   (mapcar #'recoder/a-signal:a-signal-units (<trd-seq>-a-sig trd-seq))
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
  (:documentation "@b(Описание:)  класс @b(<csv-stream>) для вывода в csv формате."))

;;;;;;;;;;

(export '(*csv-stream*))

(defparameter *csv-stream* (make-instance '<csv-stream> :external-format :cp1251)
  "@b(Описание:) переменная @b(*csv-stream*)
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CVS export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod export-to ((trd-seq <trd-seq>) (csv-stream <csv-stream>)
                      &key
                        (start 0)
                        (end (trd-total-records trd-seq))
                        (by 1))
  "@b(Описание:) метод @b(export-to) выполняет вывод объекта @b(trd-seq) в
поток @b(csv-stream).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-open *trd-sig*)
 (export-to *trd-sig* *csv-stream*)
@end(code)
"
  (with-open-file (os (concatenate 'string (trd-file-name trd-seq) ".csv")
		      :direction :output :if-exists :supersede
		      :external-format (<format-stream>-external-format csv-stream))
    (format os "Time;NUM;~{~,4F~^;~}~%" (<trd-seq>-s-sig trd-seq))
    (format os "~{~,S~^;~}~%" (append '("hh:mm:ss" "NUM") (<trd-seq>-units trd-seq)))
    (loop :for i :from start :below end :by by
	  :do (format os "~S;~A;~{~,4F~^;~}~%"
		      (mnas-org-mode:utime->time (trd-utime-by-record-number trd-seq i))
		      i
		      (coerce (elt trd-seq i) 'list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pulsation-template* '("EN1" "EN2" "EN3"  "EB060" "EB120" "EB130" "EB090" "T04" "Na"))

(defun extract-signals (fname signals &key (by 5))
  (let ((trd-seq (make-instance '<trd-seq> :trd-file-name fname :s-sig signals)))
    (trd-open trd-seq)
    (export-to trd-seq *csv-stream* :by by)))

#|
(extract-signals "~/org/troynich/20200907_090415.trd" *pulsation-template*)
(extract-signals "~/org/troynich/20200907_133300.trd" *pulsation-template*)

(extract-signals "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_151019.trd" *pulsation-template*)
(extract-signals "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd" *pulsation-template*)

(defparameter *trd-sig* (make-instance '<trd-seq>
                                       :trd-file-name "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_151019.trd"
                                       ;; "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
	                               :s-sig *pulsation-template*))
|#
