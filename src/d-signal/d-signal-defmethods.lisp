;;;; d-signal-defmethods.lisp

(in-package #:recoder/d-signal)

(export '(<d-signal> d-signal-num d-signal-id d-signal-description))
        
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


(defmethod print-object ((x <d-signal>) stream)
  (format stream "~S ~S ~S" (d-signal-num x) (d-signal-id x) (d-signal-description x)))

