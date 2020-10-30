;;;; test.lisp

(defpackage #:recoder/dir
  (:use #:cl #:recoder)
  (:export analog-table)
  (:export <trd-dir>
	   <trd-tc-dir>)
  )

(in-package :recoder/dir)

(defclass <dir> ()
  ((directory :accessor <dir>-directory :initarg :directory :initform #P"~" :documentation "Каталог, из которого считываются тренды.")))

(export '(<trd-dir>))

(defclass <trd-dir> (<dir>) ())

(export '(<trd-tc-dir>))

(defclass <trd-tc-dir> (<dir>) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analog-ids ((td <trd-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список идентификаторов сигналов.
"
  signal-ids)

(defmethod analog-signals ((td <trd-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список значений сигналов,
соответствующих моментам времени из списка @b(u-times).
"
  (mapcar
   #'(lambda (ut)
       (let* ((trd   (recoder:find-trd-by-utime-dirname ut (<dir>-directory td)))
	      (a-sig (when trd (recoder:trd-analog-signal-list trd signal-ids)))
              (nils  (loop :for i :in signal-ids :collect nil)))
         (if trd
             (recoder:trd-analog-mid-by-utime trd ut a-sig)
             nils)))
   u-times))

(defmethod analog-units ((td <trd-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список размерностей
для первого найденного тренда из каталога td.
"
  (let* ((trd   (recoder:find-trd-by-utime-dirname (first u-times) (<dir>-directory td)))
         (a-sig (when trd (recoder:trd-analog-signal-list trd signal-ids)))
         (nils  (loop :for i :in signal-ids :collect nil)))
    (if trd
        (mapcar #'(lambda (a-s) (recoder/a-signal:a-signal-units a-s)) a-sig)
        nils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analog-ids ((td <trd-tc-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список идентификаторов сигналов.
"
  (let ((trd-tc (t-c:make-<trd-tc> (<dir>-directory td) "*-_.txt")))
    (t-c:<trd-tc>-header trd-tc )))

(defmethod analog-signals ((td <trd-tc-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список сигналов
"
  (let ((trd-tc (t-c:make-<trd-tc> (<dir>-directory td) "*-_.txt")))
    (mapcar
     #'(lambda (ut) (t-c:trd-analog-all-by-utime trd-tc ut))
     u-times)))

(defmethod analog-units ((td <trd-tc-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список сигналов
"
  (let ((trd-tc (t-c:make-<trd-tc> (<dir>-directory td) "*-_.txt")))
    (loop :for i :in (t-c:trd-analog-ids trd-tc) :collect "°C")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analog-table-ids (u-times &rest rest)
  (mapcar
   #'(lambda (td-signals)
       (analog-ids (first td-signals) u-times (second td-signals)))
   rest))

(defun analog-table-units (u-times &rest rest)
  (mapcar
   #'(lambda (td-signals)
       (analog-units (first td-signals) u-times (second td-signals)))
   rest))

(export '(analog-table))

(defun analog-table (u-times &rest rest)
  (let ((rez (apply #'mapcar #'append (mapcar #'(lambda (td-signals) (analog-signals (first td-signals) u-times (second td-signals))) rest))))
    (setf rez (mapcar #'(lambda (ut data) (append (mnas-org-mode:utime->date-time ut) data )) u-times rez))
    (setf rez (math/list-matr:prepend-rows
	       (list
		(append '("Дата" "Время")
			(apply #'append (apply #'analog-table-ids  (append (list u-times) rest))))
		(append '("<YYYY-MM-DD>" "hh:mm:ss")
			(apply #'append (apply #'analog-table-units (append (list u-times) rest)))))
	       rez))
    (mnas-format:round-2d-list rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
