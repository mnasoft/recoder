;;;; test.lisp

(defpackage #:recoder/dir
  (:use #:cl #:recoder/trd)
  (:export analog-table)
  (:export <trd-dir>
	   <trd-tc-dir>)
  (:intern analog-ids     
           analog-signals 
           analog-units   
           ) 
  )

(in-package :recoder/dir)

(defclass <dir> ()
  ((directory :accessor <dir>-directory :initarg :directory :initform #P"~" :documentation "Каталог, из которого считываются тренды.")))

(defclass <trd-dir> (<dir>) ())

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
       (let* ((trd   (recoder/trd:find-trd-by-utime-dirname ut (<dir>-directory td)))
	      (a-sig (when trd (recoder/slist:trd-analog-signal-list trd signal-ids)))
              (nils  (loop :for i :in signal-ids :collect nil)))
         (if trd
             (recoder/get:trd-analog-mid-by-utime trd ut a-sig)
             nils)))
   u-times))

(defmethod analog-units ((td <trd-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список размерностей
для первого найденного тренда из каталога td.
"
  (let* ((trd   (recoder/trd:find-trd-by-utime-dirname (first u-times) (<dir>-directory td)))
         (a-sig (when trd (recoder/slist:trd-analog-signal-list trd signal-ids)))
         (nils  (loop :for i :in signal-ids :collect nil)))
    (if trd
        (mapcar #'(lambda (a-s) (recoder/a-signal:<a-signal>-units a-s)) a-sig)
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

(defun analog-table (u-times &rest rest)
  "@b(Описание:) функция @b(analog-table) возвращает таблицу в виде
  двумерного списка.

 @b(Переменые:)
@begin(list)
 @item(u-times - список целых чисел, значения представляющих
       универсального времени;)
 @item(rest - последующие элементы, являющиеся списками. Первыми
       элементами этих списков должны быть объекты классов: <trd-dir>
       или <trd-tc-dir>. Вторыми элементами этих списков должны быть
       списки содержащие имена сигналов (строки).)
@end(list)"
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

(defgeneric split-on-intervals-when-flag-is-on (trd d-signal-str)
  (:documentation
   "@b(Описание:) обобщенная_функция
@b(split-on-intervals-when-flag-is-on) разделяет тренд на
интервалы (выраженные в номерах записей), для которых флаг
@b(d-signal-str) имеет значение 1.
"))

(defmethod split-on-intervals-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str)
  "@b(Описание:) метод @b(split-on-intervals-when-flag-is-on)
 возвращает список, каждый элемент которого содержит два
 элемента. Первый - список интервалов, при которых значение
 дискретного флага @b(d-signal-str) равно 1.  Второй - объект тренда,
 для которого найден список интервалов.
 
 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-on-intervals-when-flag-is-on *trd-CPIPES-dir* \"Oil2Gas\")
 (((13355 14081))
 Path= \"d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd\"
 ... 
 ((10931 11252) (15413 15677) (19598 19858) (24705 24971) (29177 29440)
  (33479 33755) (37542 37813) (42104 42367))
 Path= \"d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd\")
@end(code)
"
  (let* ((trd (make-instance 'recoder/trd:<trd>))
	 (lst (apply #'append
		     (mapcar
		      #'(lambda (el)
			  (let ((rez nil))
			    (recoder/trd:trd-close trd)
			    (setf (recoder/trd:<trd>-file-name trd) el)
			    (recoder/trd:trd-open trd)
			    (setf rez (split-on-intervals-when-flag-is-on trd d-signal-str))
			    (recoder/trd:trd-close trd)
			    (when rez (list el rez))))
		      (mnas-path:find-filename (<dir>-directory trd-dir) "trd")))))
    (do ((f (first  lst) (first  lst))
	 (s (second lst) (second lst))
	 (rez nil))
	((and (null f) (null s)) (nreverse rez))
      (push (list f s) rez) (setf lst (cddr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric split-on-utimes-when-flag-is-on (trd d-signal-str)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(split-on-utimes-when-flag-is-on)
разделяет тренд (или последовательность трендов) на временные интервалы, 
для которых флаг @b(d-signal-str) имеет значение 1."))

(defmethod split-on-utimes-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str )
  "@b(Описание:) метод @b(split-on-utimes-when-flag-is-on) возвращает список, 
каждый элемент которого содержит два элемента - начало и конец временного интервала,
для которого значение дискретного флага @b(d-signal-str) равно 1.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-on-utimes-when-flag-is-on *trd-CPIPES-dir* \"Oil2Gas\")
 => ((3805688905 3805689050) (3805706924 3805707019) (3805708537 3805708637)
     ...
     (3806396457 3806396513) (3806397270 3806397324) (3806398182 3806398235))
@end(code)
"
  (let ((trd (make-instance 'recoder/trd:<trd>)))
    (apply #'append
	   (mapcar
	    #'(lambda (el)
		(let ((rez nil))
		  (recoder/trd:trd-close trd)
		  (setf (recoder/trd:<trd>-file-name trd) el)
		  (recoder/trd:trd-open trd)
		  (setf rez
			(split-on-utimes-when-flag-is-on trd d-signal-str))
		  rez))
	    (mnas-path:find-filename (<dir>-directory trd-dir) "trd")))))
