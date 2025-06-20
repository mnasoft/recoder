;;;; test.lisp

(defpackage :recoder/dir
  (:use #:cl)
  (:nicknames "R/DIR")
  (:export find-trd-by-utime-dirname
           find-trd-by-utime-files
           find-trds-by-utimes-files
           make-html-trd-foo)
  (:export analog-table)
  (:export <trd-dir>
	   <trd-tc-dir>)
  (:intern analog-ids     
           analog-signals 
           analog-units))

(in-package :recoder/dir)

(defclass <dir> ()
  ((directory :accessor <dir>-directory :initarg :directory :initform #P"~" :documentation "Каталог, из которого считываются тренды.")))

(defclass <trd-dir> (<dir>) ())

(defclass <trd-tc-dir> (<dir>) ())

#+nil (defun find-trd-by-utime-dirname (utime dir-name &key (extension "trd"))
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в каталоге dir-name.
"
  (let ((rezult nil))
    (mapc  
     #'(lambda (el)
	 (let ((trd (make-instance '<trd> :file-name el)))
	   (trd-open trd)
	   (if (<= (<trd>-utime-start trd) utime (utime-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     (mnas-path:find-filename dir-name extension))
    rezult))

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
       (let* ((trd   (find-trd-by-utime-dirname ut (<dir>-directory td)))
	      (a-sig (when trd (recoder/slist:a-signals trd signal-ids)))
              (nils  (loop :for i :in signal-ids :collect nil)))
         (if trd
             (recoder/get:trd-analog-mid-by-utime trd ut a-sig)
             nils)))
   u-times))

(defmethod analog-units ((td <trd-dir>) u-times signal-ids)
  "@b(Описание:) метод @b(analog-signals) возвращает список размерностей
для первого найденного тренда из каталога td.
"
  (let* ((trd   (find-trd-by-utime-dirname (first u-times) (<dir>-directory td)))
         (a-sig (when trd (recoder/slist:a-signals trd signal-ids)))
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
    (setf rez (math/matr:prepend-rows
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

#+nil (defmethod split-on-intervals-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str)
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
  (let* ((trd (make-instance 'r/trd:<trd>))
	 (lst (apply #'append
		     (mapcar
		      #'(lambda (el)
			  (let ((rez nil))
			    (recoder/trd:trd-close trd)
			    (setf (r/trd:<trd>-file-name trd) el)
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

#+nil (defmethod split-on-utimes-when-flag-is-on ((trd-dir <trd-dir>) d-signal-str )
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
		  (setf (r/trd:<trd>-file-name trd) el)
		  (recoder/trd:trd-open trd)
		  (setf rez
			(split-on-utimes-when-flag-is-on trd d-signal-str))
		  rez))
	    (mnas-path:find-filename (<dir>-directory trd-dir) "trd")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-html-trd-foo (trd-dname html-fname str-signal-list time-lst ht-sname-oboznach &key (transpose nil))
  "Вывод в данных из тренда в файл trd-dname в файл html-fname;
Данные выводятся по столбцам;
trd-dname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time-lst          - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:

"
  (let ((trd-lst (mapcar #'(lambda (ut) (find-trd-by-utime-dirname ut trd-dname)) time-lst))
	(rez                  nil)
	(data                 nil)
	(dev                  nil)
	(d-time-str           nil)
	
	(<a-signal>-units       nil)
	(<a-signal>-id          nil)
	(ht-sname             nil)
	(<a-signal>-description nil)	
	)
    (mapc #'(lambda (trd time)
	      (trd-open trd)
	      (let ((s-list (a-signals trd str-signal-list)))
		(setf <a-signal>-units (mapcar #'(lambda (el) (<a-signal>-units el)) s-list)
		      <a-signal>-id (mapcar #'(lambda (el) (<a-signal>-id el))       s-list)
		      ht-sname (mapcar #'(lambda (el) (gethash (<a-signal>-id el) ht-sname-oboznach)) s-list)
		      <a-signal>-description (mapcar #'(lambda (el) (<a-signal>-description el)) s-list)
		      
		      )

		(push (trd-analog-mid-by-utime trd time s-list)    data)
		(push (trd-analog-stddev-by-utime trd time s-list) dev)
		(push (list (date time :stream nil) (day-time time :stream nil)) d-time-str)))
	  trd-lst time-lst)
    (setf data (reverse data)
	  dev  (reverse dev)
	  d-time-str (reverse d-time-str)
	  )
    (setf data  (mapcar #'(lambda (tm da) (append tm da)) d-time-str data)
	  dev   (mapcar #'(lambda (tm dv) (append tm dv)) d-time-str dev))
    (setf rez (append  data dev ))
    (push (append '("YYYY-MM-DD" "hh:mm:ss") <a-signal>-units ) rez)
    (push (append '("-" "-") <a-signal>-id )                    rez)
    (push (append '("Date" "Time") ht-sname  ) rez)
    (push (append '("Дата" "Время") <a-signal>-description) rez)
    (when transpose (setf rez (transpose rez)))
    (html-table:list-list-html-table rez html-fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun find-trd-by-utime-files (utime files)
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в списке файлов files.
"
  (let ((rezult nil))
    (mapc  
     #'(lambda (el)
	 (let ((trd (make-instance '<trd> :file-name el)))
	   (trd-open trd)
	   (if (<= (<trd>-utime-start trd) utime (utime-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     files)
    rezult))

(defun find-trds-by-utimes-files (utimes files)
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в списке файлов.
"
  (loop :for ut :in utimes
        :collect
        (list ut (find-trd-by-utime-files ut files))))
