;;;; a-signal-defmethods.lisp

(defpackage #:recoder/a-signal
  (:use #:cl)
  (:nicknames "R/A-SIG")
  (:export encode-lineary) ;; куда-то перенести
  (:export correct-range)
  (:intern *ushort-max*)
  (:export a-signal-print-format)
  )

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

(defparameter *a-signal-print-format* :long)

(defun a-signal-print-format ()
 *a-signal-print-format*)

(defun (setf a-signal-print-format) (value)
  (declare (type (member :long :short) value))
  (setf *a-signal-print-format* value))

(setf (a-signal-print-format) :short)
#+nil (setf (a-signal-print-format) :long)


(defmethod print-object ((x r/c:<a-signal>) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (when (eq (a-signal-print-format) :long)
      (format stream "~3D ~10S [~A ~A] ~S ~S"
              (r/c:<a-signal>-num x)
              (r/c:<a-signal>-id x)
              (r/c:<a-signal>-min x)
              (r/c:<a-signal>-max x)
              (r/c:<a-signal>-units x)
              (r/c:<a-signal>-description x)))))

#+nil
(defgeneric r/c:<a-signal>-value (a-signal ushort-int)
  (:documentation "@b(Описание:) обобщенная_функция @b(r/c:<a-signal>-value)
возвращает расшифрованное значение сигнала @b(a-signal) по его зашифрованному значению
@b(ushort-int) от 0 включтельно до 65536 исключительно."))

(defmethod r/g:decode-value ((encoded-value integer) (a-signal r/c:<a-signal>) )
  (+ (r/c:<a-signal>-min a-signal)
     (* (- (r/c:<a-signal>-max a-signal)
	   (r/c:<a-signal>-min a-signal))
	(/ encoded-value *ushort-max*))))

(defmethod r/g:encode-value ((decoded-value float) (a-signal r/c:<a-signal>))
  (let* ((encoded-min 0)
         (coded-max *ushort-max*)
         (scaled-value
           (round
            (+ encoded-min
               (* (/ (- decoded-value (r/c:<a-signal>-min a-signal))
                     (- (r/c:<a-signal>-max a-signal) (r/c:<a-signal>-min a-signal)))
                  (- coded-max encoded-min))))))
    (min (max scaled-value encoded-min) coded-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod r/g:read-obj ((a-signal r/c:<a-signal>) in)
  (setf (r/c:<a-signal>-id a-signal)
        (m-bin:b-read-string in r/const:+signal-id-wid+))
  (setf (r/c:<a-signal>-description a-signal)
        (m-bin:b-read-string in r/const:+signal-description-wid+))
  (setf (r/c:<a-signal>-units a-signal)
        (m-bin:b-read-string in r/const:+signal-units-wid+))
  (setf (r/c:<a-signal>-min  a-signal)
        (m-bin:b-read-double in))
  (setf (r/c:<a-signal>-max  a-signal)
        (m-bin:b-read-double in))
  a-signal)

(defmethod r/g:write-obj ((a-signal r/c:<a-signal>) out)
  (m-bin:b-write-string (r/c:<a-signal>-id a-signal) out
                        r/const:+signal-id-wid+)
  (m-bin:b-write-string (r/c:<a-signal>-description a-signal) out
                        r/const:+signal-description-wid+)
  (m-bin:b-write-string (r/c:<a-signal>-units a-signal) out
                        r/const:+signal-units-wid+)
  (m-bin:b-write-double (r/c:<a-signal>-min a-signal) out)
  (m-bin:b-write-double (r/c:<a-signal>-max a-signal) out)
  a-signal)


(defmethod correct-range ((a-signal r/c:<a-signal>))
  (let ((v-min (r/c:<a-signal>-min a-signal))
        (v-max (r/c:<a-signal>-max a-signal)))
  (cond
    ((and (= v-min v-max 0.0d0))
     (setf (r/c:<a-signal>-max a-signal) 1.0d0))
    ((and (= (- v-max v-min) 0.0d0))
     (setf (r/c:<a-signal>-max a-signal) (+ v-max 1.0d0))))))
