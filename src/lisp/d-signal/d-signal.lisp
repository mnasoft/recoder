;;;; d-signal-defmethods.lisp

(defpackage #:recoder/d-signal
  (:use #:cl)
  (:nicknames "R/D-SIG")
  ;; #:mnas-string #:recoder/binary
  (:export <d-signal>
           <d-signal>-num
           <d-signal>-id
	   <d-signal>-description
           ))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(in-package :recoder/d-signal)

(defclass <d-signal> ()
  ((num         :accessor <d-signal>-num         :initarg :num         :initform nil :documentation "Номер сигнала в списке сигналов. Первый сигнал имеет номер 0")
   (id          :accessor <d-signal>-id          :initarg :id          :initform nil :documentation "Обозначение сигнала, char[10]")
   (description :accessor <d-signal>-description :initarg :description :initform nil :documentation "Описание сигнала, char[40]"))
  (:documentation "Дескриптор (описание) дискретного сигнала.

 Запись дескриптора аналогового сигнала во внутреннем представлении
файла-тренда имеет следующую структуру:

@begin(table)
@begin(row) @cell(Поле)               @cell(Длина поля)@cell(Примечание) @end(row)
@begin(row) @cell(discret-id)         @cell(10)        @cell(Обозначение дискретного сигнала) @end(row)
@begin(row) @cell(discret-description)@cell(40)        @cell(Описание дискретного сигнала) @end(row)
@end(table)"))

(defmethod print-object ((x <d-signal>) stream)
  (print-unreadable-object (x stream)
    (format stream "~3D ~10S ~S"
            (<d-signal>-num x)
            (<d-signal>-id x)
            (<d-signal>-description x))))

(defmethod r/g:read-obj ((d-signal <d-signal>) in)
  (setf (<d-signal>-id d-signal)
        (m-bin:b-read-string in r/c:+signal-id-wid+))
  (setf (<d-signal>-description d-signal)
        (m-bin:b-read-string in r/c:+signal-description-wid+))
  d-signal)

(defmethod r/g:write-obj ((d-signal <d-signal>) out)
  (m-bin:b-write-string (<d-signal>-id d-signal) out
                        r/c:+signal-id-wid+)
  (m-bin:b-write-string (<d-signal>-description d-signal) out
                        r/c:+signal-description-wid+)
  d-signal)

