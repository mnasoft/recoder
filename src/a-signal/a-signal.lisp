;;;; a-signal-defmethods.lisp

(defpackage #:recoder/a-signal
  (:use #:cl)
  (:nicknames "R/A-SIG")
  (:export <a-signal>
	   <a-signal>-units
	   <a-signal>-num
           <a-signal>-id
           <a-signal>-description
	   <a-signal>-min
           <a-signal>-max
	   )
  (:export decode-value
           encode-value)
  (:export encode-lineary) ;; куда-то перенести
  (:intern *ushort-max*))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(in-package :recoder/a-signal)

(defparameter *ushort-max* (- (expt 2 16) 1)
  "Количество градаций аналогового сигнала от 0 до *ushort-max* при записи тренда")

(defun encode-lineary (decoded-value deencoded-min decoded-max encoded-min coded-max)
  "Линейно интерполирует значение из декодированного диапазона в кодированный.
   Если значение выходит за границы, оно ограничивается."
  (let ((scaled-value
          (round
           (+ encoded-min
              (* (/ (- decoded-value decoded-min)
                    (- decoded-max decoded-min))
                 (- encoded-max encoded-min))))))
    (min (max scaled-value coded-min) coded-max)))  ;; Ограничение в диапазоне

(defclass <a-signal> ()
  ((num         :accessor <a-signal>-num         :initarg :num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (id          :accessor <a-signal>-id          :initarg :id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (description :accessor <a-signal>-description :initarg :description :initform nil :documentation "Описание сигнала, char[40]")
   (units       :accessor <a-signal>-units       :initarg :units       :initform nil :documentation "Размерность аналогового сигнала, char[8]")
   (min         :accessor <a-signal>-min         :initarg :min         :initform nil :documentation "Нижняя граница диапазона аналогового сигнала, double = char[8]")
   (max         :accessor <a-signal>-max         :initarg :max         :initform nil :documentation "Верхняя граница диапазона аналогового сигнала, double = char[8]"))
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

(defmethod print-object ((x <a-signal>) stream)
  (format stream "#a-s(~S ~S [~A ~A] ~S ~S)"
          (<a-signal>-num x)
          (<a-signal>-id x)
          (<a-signal>-min x)
          (<a-signal>-max x)
          (<a-signal>-units x)
          (<a-signal>-description x)))

#+nil
(defgeneric <a-signal>-value (a-signal ushort-int)
  (:documentation "@b(Описание:) обобщенная_функция @b(<a-signal>-value)
возвращает расшифрованное значение сигнала @b(a-signal) по его зашифрованному значению
@b(ushort-int) от 0 включтельно до 65536 исключительно."))

(defgeneric decode-value (encoded-value a-signal)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(decode-value) выполняет
раскодирование аналогового сигнала, представленного закодированным
значением @b(encoded-value) по правилу, основанному на характеристиках
дескриптора @b(a-signal)."))

(defgeneric encode-value (decoded-value a-signal)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(encode-value) выполняет
кодирование аналогового сигнала, представленного раскодированным
значением @b(decoded-value) по правилу, основанному на характеристиках
дескриптора @b(a-signal)."))


(defmethod decode-value ((encoded-value integer) (a-signal <a-signal>) )
  (+ (<a-signal>-min a-signal)
     (* (- (<a-signal>-max a-signal)
	   (<a-signal>-min a-signal))
	(/ encoded-value *ushort-max*))))

(defmethod encode-value ((decoded-value float) (a-signal <a-signal>))
  (let* ((encoded-min 0)
         (coded-max *ushort-max*)
         (scaled-value
           (round
            (+ encoded-min
               (* (/ (- decoded-value (<a-signal>-min a-signal))
                     (- (<a-signal>-max a-signal) (<a-signal>-min a-signal)))
                  (- coded-max encoded-min))))))
    (min (max scaled-value encoded-min) coded-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
