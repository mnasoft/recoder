(defpackage #:recoder/interval
  (:use #:cl  #:recoder/trd)
  (:nicknames "R/INTERVAL") 
  (:export trd-interval-to-secods
           trd-interval-to-minutes
           trd-interval-to-hours))

(in-package #:recoder/interval)

(defmethod trd-interval-to-secods ((trd <trd>) interval)
  "@b(Описание:) функция @b(trd-interval-to-secods) возвращает для
 тренда @b(trd) длительность временного интервала в секундах между
 записями тренда.

 @b(Переменые:)
@begin(list)
 @item(trd - объект класса <trd>;)
 @item(interval - список из двух значений, определяющих начальную и конечную записи интервала.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-interval-to-secods *trd* '(0 50)) => 12.5d0
@end(code)
"
  (* (<trd>-delta-time trd) (apply #'- (reverse interval))))

(defmethod trd-interval-to-minutes ((trd <trd>) interval)
  "@b(Описание:) функция @b(trd-interval-to-secods) возвращает для
 тренда @b(trd) длительность временного интервала в минутах между
 записями тренда."
  (* 1/60 (trd-interval-to-secods trd interval)))

(defmethod trd-interval-to-hours ((trd <trd>) interval)
  "@b(Описание:) функция @b(trd-interval-to-secods) возвращает для
 тренда @b(trd) длительность временного интервала в часах между
 записями тренда."
  (* 1/60 1/60 (trd-interval-to-secods trd interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

