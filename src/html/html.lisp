;;;; ./src/html/html.lisp

(defpackage :recoder/html
  (:use #:cl
        #:recoder/get
        #:recoder/d-signal
        #:recoder/a-signal
        #:mnas-string/print) ;;;; #:mnas-string/print #:recoder/binary
  (:nicknames "R/HTML")
  (:export make-html-trd))

(in-package :recoder/html)

(defun make-html-trd (trd-fname html-fname s-names utimes
                      &key
                        (sname->des nil)
                        (ht-sname->des
                         (let ((ht (make-hash-table  :test #'equal)))
                           (loop :for i :in s-names
                                 :do (setf (gethash i ht) i)
                                 :finally
                                    (progn
                                      (loop :for j :in sname->des :do
                                        (setf (gethash (first j) ht) (second j)))
                                        (return ht)))))
                        (transpose nil))
  "@b(Описание:) функция @b(make-html-trd) осуществляет вывод в данных
  из тренда в файл trd-fname в файл html-fname.

 Данные выводятся по столбцам.

 @b(Переменые:)
@begin(list)
 @item(trd-fname - имя файла тренда;)
 @item(html-fname - имя html-файла;)
 @item(s-names - список выводимых сигналов;)
 @item(utimes - список, элементами которого являются универсальное время;)
 @item(ht-sname->des - хеш-таблица, элементами которой являются в качестве: 
 ключа - имена сигналов; 
 качестве значений - обозначения сигналов.)
 @item(transpose - признак выполнения транспонирования таблицы: 
 t - выполнять транспонирование; 
 nil - не выполнять.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 
@end(code)
"
  (let ((trd (make-instance 'recoder/trd:<trd> :file-name trd-fname)))
    (recoder/trd:trd-open trd)
    (let* ((date-time-units '("Y-M-D" "h:m:s"))
           (s-list (recoder/slist:a-signals trd s-names))
           (a-units (mapcar #'(lambda (el) (<a-signal>-units el)) s-list))
           (a-ids   (mapcar #'(lambda (el) (<a-signal>-id el))    s-list))
           (a-designations (mapcar #'(lambda (el) (gethash (<a-signal>-id el) ht-sname->des)) s-list))
	   (rez nil)
	   (data (mapcar #'(lambda (el) (trd-analog-mid-by-utime trd el    s-list)) utimes)) ;; trd-analog-mid-by-udate
	   (dev  (mapcar #'(lambda (el) (trd-analog-stddev-by-utime trd el s-list)) utimes)) ;; trd-analog-stddev-by-udate
	   (d-time-str (mapcar #'(lambda (tm ) (list (date tm :stream nil)
						     (day-time tm :stream nil)))
			       utimes)))
      (setf rez
            (mapcar
             #'(lambda (tm da dv) (append tm da tm dv))
             d-time-str data dev))
      (push (append date-time-units a-units date-time-units a-units) rez)
      (push (append '("" "Values") a-ids '("" "Std-Dev") a-ids ) rez)
      (push (append '("Дата" "Время") a-designations '("Дата" "Время") a-designations) rez)
      #+nil
      (push (append '("Дата" "Время") (mapcar #'(lambda (el) (<a-signal>-description el)) s-list)) rez)
      (when transpose (setf rez (math/matr:transpose rez)))
      (html-table:list-list-html-table rez html-fname)
      rez)))

(let ((s-names '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO")))
  (make-html-trd recoder/trd:*trd-fname*
                 "~/123.html"
                 s-names
                 (list
                  (+ 10 (recoder/trd:utime-start  recoder/trd:*trd*))
                  (+ 20 (recoder/trd:utime-start  recoder/trd:*trd*))
                  (+ 30 (recoder/trd:utime-start  recoder/trd:*trd*)))
                 :sname->des '(("V2" "v<sub>2</sub>")
                               ("P02" "p<sub>02</sub>")
                               ("T2"  "t<sub>2</sub>")
                               ("ET300" "t<sub>03.MID</sub>"))))
