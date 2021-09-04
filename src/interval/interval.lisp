(defpackage #:recoder/interval
  (:use #:cl  #:recoder/trd)
  (:export trd-interval-to-secods
           trd-interval-to-minutes
           trd-interval-to-hours))

(in-package #:recoder/interval)

(defmethod trd-interval-to-secods ((trd <trd>) interval)
  "Преобразует диапазон времени, заданный в записях, в секунды"
  (* (<trd>-delta-time trd) (apply #'- (reverse interval))))

(defmethod trd-interval-to-minutes ((trd <trd>) interval)
  "Преобразует диапазон времени, заданный в записях, в минуты"
  (* 1/60 (trd-interval-to-secods trd interval)))

(defmethod trd-interval-to-hours ((trd <trd>) interval)
  "Преобразует диапазон времени, заданный в записях, часы"
  (* 1/60 1/60 (trd-interval-to-secods trd interval)))
