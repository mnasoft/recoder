;;;; recoder.lisp

(in-package #:recoder/trd)

(export 'apply-and )

(defun apply-and (lst)
  (mapc #'(lambda (el) (unless el (return-from  apply-and nil))) lst)
  t)

(export 'apply-or )

(defun apply-or (lst)
  (mapc #'(lambda (el) (when el (return-from apply-or t))) lst)
  nil)

(export 'time-universal-encode )

(defun time-universal-encode (year month day hour min sec)
  "Функция кодирования в универсальный формат времени"
  (encode-universal-time sec min hour day month year))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'recode-string )

(defun recode-string (bufer &key (start 0) (len (length bufer))  (break-nul T) (code-page *cp1251*))
  "@b(Описание:) recode-string выполняет преобразование символов, 
передаваемых в параметре bufer, имеющих кодировку code-page (*cp1251*|*cp866*), 
в кодировку utf8."
  (do*
   ( (i start (1+ i))
     (ch (gethash (nth i bufer) code-page) (gethash (nth i bufer) code-page))
     (str-rez ""))
   ( (or (>= i (+ start len))
	 (and break-nul (= 0 (nth i bufer)))) 
    str-rez)
    (setf str-rez (concatenate 'string str-rez ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'make-html-trd )

(defun make-html-trd (trd-fname html-fname str-signal-list time-lst ht-sname-oboznach &key (transpose nil))
  "Вывод в данных из тренда в файл trd-fname в файл html-fname;
Данные выводятся по столбцам;
trd-fname         - имя файла тренда;
html-fname        - имя html-файла;
str-signal-list   - список выводимых сигналов;
time-lst          - список, элементами которого являются универсальное время;
ht-sname-oboznach - хеш-таблица, элементами которой являются:
                    в качестве ключа    - имена сигналов;
                    в качестве значений - обозначения сигналов
Пример использования:
"
  (let ((trd (make-instance '<trd> :trd-file-name trd-fname)))
    (trd-open trd)
    (let* ((s-list (trd-analog-signal-list trd str-signal-list))
	   (rez nil)
	   (data (mapcar #'(lambda (el) (trd-analog-mid-by-udate trd el    s-list)) time-lst))
	   (dev  (mapcar #'(lambda (el) (trd-analog-stddev-by-udate trd el s-list)) time-lst))
	   (d-time-str (mapcar #'(lambda (tm ) (list (print-universal-date tm :stream nil)
						     (print-universal-time tm :stream nil)))
			       time-lst)))
      (setf data  (mapcar #'(lambda (tm da) (append tm da)) d-time-str data)
	    dev   (mapcar #'(lambda (tm dv) (append tm dv)) d-time-str dev))
      (setf rez (append  data dev ))
      (push (append '("YYYY-MM-DD" "hh:mm:ss") (mapcar #'(lambda (el) (a-signal-units el)) s-list) ) rez)
      (push (append '("-" "-") (mapcar #'(lambda (el) (a-signal-id el))    s-list) ) rez)
      (push (append '("Date" "Time") (mapcar #'(lambda (el) (gethash (a-signal-id el) ht-sname-oboznach)) s-list)) rez)
      (push (append '("Дата" "Время") (mapcar #'(lambda (el) (a-signal-description el)) s-list)) rez)
      (when transpose (setf rez (transpose rez)))
      (html-table:list-list-html-table rez html-fname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'find-trd-by-utime-dirname )

(defun find-trd-by-utime-dirname (utime dir-name &key (extension "trd"))
  "Возвращает объект тренда, для которого существуют данные на момент 
универсального времени utime в каталоге dir-name
"
  (let ((rezult nil))
    (mapc  
     #'(lambda (el)
	 (let ((trd (make-instance '<trd> :trd-file-name el)))
	   (trd-open trd)
	   (if (<= (trd-utime-start trd) utime (trd-utime-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     (mnas-path:find-filename dir-name extension))
    rezult))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'make-html-trd-foo )

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
	
	(a-signal-units       nil)
	(a-signal-id          nil)
	(ht-sname             nil)
	(a-signal-description nil)	
	)
    (mapc #'(lambda (trd time)
	      (trd-open trd)
	      (let ((s-list (trd-analog-signal-list trd str-signal-list)))
		(setf a-signal-units (mapcar #'(lambda (el) (a-signal-units el)) s-list)
		      a-signal-id (mapcar #'(lambda (el) (a-signal-id el))       s-list)
		      ht-sname (mapcar #'(lambda (el) (gethash (a-signal-id el) ht-sname-oboznach)) s-list)
		      a-signal-description (mapcar #'(lambda (el) (a-signal-description el)) s-list)
		      
		      )

		(push (trd-analog-mid-by-utime trd time s-list)    data)
		(push (trd-analog-stddev-by-utime trd time s-list) dev)
		(push (list (print-universal-date time :stream nil) (print-universal-time time :stream nil)) d-time-str)))
	  trd-lst time-lst)
    (setf data (reverse data)
	  dev  (reverse dev)
	  d-time-str (reverse d-time-str)
	  )
    (setf data  (mapcar #'(lambda (tm da) (append tm da)) d-time-str data)
	  dev   (mapcar #'(lambda (tm dv) (append tm dv)) d-time-str dev))
    (setf rez (append  data dev ))
    (push (append '("YYYY-MM-DD" "hh:mm:ss") a-signal-units ) rez)
    (push (append '("-" "-") a-signal-id )                    rez)
    (push (append '("Date" "Time") ht-sname  ) rez)
    (push (append '("Дата" "Время") a-signal-description) rez)
    (when transpose (setf rez (transpose rez)))
    (html-table:list-list-html-table rez html-fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'get-open-ternd )

(defun get-open-ternd ()
  (mnas-file-dialog:get-open-file :filetypes '(("Файлы трендов" "*.trd")) :title "Выберите файл тренда"))

(export 'get-open-ternds )

(defun get-open-ternds ()
  (mnas-file-dialog:get-open-file :filetypes '(("Файлы трендов" "*.trd")) :title "Выберите файлы трендов" :multiple t))

(export 'change-directory-default )

(defun change-directory-default () (mnas-file-dialog:change-directory-default))

(export 'trd-a-ids )

(defmethod trd-a-ids (a-sig-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-ids) возвращает имена 
идентификаторов аналоговых сигналов.
@begin(list)
 @item(a-sig-names - список имен сигналов; )
 @item(trd         - тренд. )
@end(list)
"
  (mapcar
   #'(lambda (el)
       (a-signal-id
	(gethash el (trd-analog-ht trd))))
   a-sig-names))

(export 'trd-a-units )

(defmethod trd-a-units (a-sig-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-units) возвращает размерности 
аналоговых сигналов.
@begin(list)
 @item(a-sig-names - список имен сигналов;)
 @item( trd        - тренд.)
@end(list)
"
  (mapcar
   #'(lambda (el)
       (a-signal-units
	(gethash el (trd-analog-ht trd))))
   a-sig-names))