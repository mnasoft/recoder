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
    (mnas-format:round-2d-list rez)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-C100-dir*
  (make-instance '<trd-tc-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-C100"))

(defparameter *trd-CPIPES-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES"))

(defparameter *trd-NIO-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-NIO"))

(analog-signals *trd-NIO-dir* `(,(encode-universal-time 07 12 14 29 07 2020)) nil )

(let ((ut-s `(,(encode-universal-time 07 12 14 29 07 2020))))
  (analog-table ut-s
		`(,*trd-CPIPES-dir* ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-C100-dir*   ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-NIO-dir*    ,(append (loop :for i :from 1 :to 76 :collect (format nil "T15_~2,'0D" i)) '("CF104" "CF140")))
		))

#|
(defparameter *trd-tc-dir*
  (make-instance '<trd-tc-dir> :directory "D:/home/_namatv/_WorkPlan/2020/80/Испытания 10211.ДМ80.237ПМ/trd-C100/1"))

(defparameter *trd-CPIPES-dir*
  (make-instance '<trd-dir> :directory "D:/home/_namatv/_WorkPlan/2020/80/Испытания 10211.ДМ80.237ПМ/trd-CPiPES"))


(let ((ut-s `(,(encode-universal-time 07 12 14 29 07 2020))))
  ut-s
  (analog-signals *trd-CPIPES-dir* ut-s `("GQ010" "EN1" "EN2" "EN3" "T04"))
  (analog-units   *trd-CPIPES-dir* ut-s `("GQ010" "EN1" "EN2" "EN3" "T04"))
  (analog-signals *trd-tc-dir*     ut-s `("GQ010" "EN1" "EN2" "EN3" "T04"))
;;  (analog-units   *trd-tc-dir*     ut-s `("GQ010" "EN1" "EN2" "EN3" "T04"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let* ((trd-1      (recoder:find-trd-by-utime-dirname (first u-times) *CPiPES-trd-path*))
       (a-sig-name (recoder:trd-analog-signal-list trd-1 (mapcar #'first *SAU-signals*)))
       (a-sig-des  (mapcar #'second *SAU-signals*))
       )
  )

(mnas-format:round-2d-list
 (math/list-matr:prepend-rows
  (list
   (append '("Дата"       "Время")    a-sig-des)
   (append '("-"          "-")        (mapcar #'(lambda (a-sig) (recoder/a-signal:a-signal-id    a-sig)) a-sig-name))
   (append '("ΥΥΥΥ-ΜΜ-DD" "hh:mm:ss") (mapcar #'(lambda (a-sig) (recoder/a-signal:a-signal-units a-sig)) a-sig-name))
   )
  ))

(mapcar
   #'(lambda (ut)
       (let* ((trd (recoder:find-trd-by-utime-dirname ut *CPiPES-trd-path*))
	      (a-sig (recoder:trd-analog-signal-list trd (mapcar #'first *SAU-signals*))))
	 (append `(,(mnas-org-mode:utime->date ut)
		   ,(mnas-org-mode:utime->time ut))
		 (recoder:trd-analog-mid-by-utime trd ut a-sig))))
   u-times)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

|#
