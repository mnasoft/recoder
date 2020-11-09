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

(defmethod sig     (key data (trd-seq <trd-seq>))
  (svref data (gethash key (<trd-seq>-h-tbl trd-seq))))

(defmethod sig-on  (key data (trd-seq <trd-seq>))
  (= 1 (sig key data trd-seq)))

(defmethod sig-off (key data (trd-seq <trd-seq>))
  (= 0 (sig key data trd-seq)))

(sig "GQ010" (elt *trd* 10000) *trd*)

(sig-off "FK310" (elt *trd* 10000) *trd*)

(<trd-seq>-d-sig *trd*)
