;;;; ./clisp/recoder/src/split/split.lisp

(defpackage #:recoder/split
  (:use #:cl)
  (:nicknames "R/SPLIT")
  (:intern apply-and
	   apply-or
	   )
  (:export split-on-intervals-of-time-when-flag-is-on
	   split-on-intervals-when-flag-is-on
   	   split-on-intervals-by-condition
	   split-on-utimes-when-flag-is-on))

(in-package :recoder/split)

(defun apply-and (lst)
  (mapc #'(lambda (el) (unless el (return-from  apply-and nil))) lst)
  t)

(defun apply-or (lst)
  (mapc #'(lambda (el) (when el (return-from apply-or t))) lst)
  nil)

(defmethod split-on-intervals-when-flag-is-on ((trd r/trd:<trd>) d-signal-str )
  "@b(Описание:) метод @b(split-on-intervals-when-flag-is-on) для 
тренда @b(trd) выполняет поиск диапазонов, для которых значение 
дискретного сигнала с именем d-signal-str имеет значение 1.

 Начало и конец диапазона выражено в порядковы номерах записи с начала тренда.
todo: доработать, чтоб возвращался последний диапазон при поднятом флаге в конце"
  (let* ((flag (gethash d-signal-str (r/trd:<trd>-discret-ht trd)))
	 (flag-lst (list flag))
	 (total-rec (r/trd:<trd>-records trd))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1)
	 (rez nil)
	 )
    (dotimes (i (r/trd:<trd>-records trd) (nreverse rez-lst))
      (setf rez (first (r/get:trd-discret-by-record-t-nil trd i flag-lst)))
      (if rez
	  (setf n-start (min i n-start)
		n-end   (max i n-end))
	  (when (< -1 n-end)
	    (push (list n-start n-end) rez-lst)
	    (setf n-start total-rec
		  n-end -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod split-on-utimes-when-flag-is-on ((trd r/trd:<trd>) d-signal-str )
  "@b(Описание:) метод @b(split-on-intervals-when-flag-is-on) для 
тренда @b(trd) выполняет поиск диапазонов, для которых значение 
дискретного сигнала с именем d-signal-str имеет значение 1.

 Начало и конец диапазона выражено в порядковы номерах записи с начала тренда.
todo: доработать, чтоб возвращался последний диапазон при поднятом флаге в конце"
  (mapcar
   #'(lambda (el)
       (list (r/trd:record->utime trd (first el))
	     (r/trd:record->utime trd (second el))))
   (split-on-intervals-when-flag-is-on trd  d-signal-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod split-on-intervals-of-time-when-flag-is-on ((trd r/trd:<trd>) d-signal-str)
  "Для тренда trd выполняет поиск диапазонов, для которых
значение сигнала d-signal-str принимало значение 1. 
И возвращает длительность этих диапазонов"
  (let ((intervals (split-on-intervals-when-flag-is-on trd d-signal-str))) ;; trd-flag-on-intervals
    (values
     (mapcar
      #'(lambda (el) (* -1 (r/trd:<trd>-increment trd) (apply #'- el)))
      intervals)
     intervals)))

(defmethod split-on-intervals-by-condition ((trd r/trd:<trd>) start-signal-str-lst end-signal-str-lst)
  "@b(Описание:) метод @b(split-on-intervals-by-condition)
Выполняет деление тренда на диапазоны.

Возвращает список.

Каждый элемент, возвращаемого списка, состоит из двух номеров записей
- начальной и конечной.

@b(Параметры:)
@begin(list)
 @item(@cl:param(trd)                  - объект типа <trd> [тренд];)
 @item(@cl:param(start-signal-str-lst) - список имен [строк] дискретных сингалов тренда;)
 @item(@cl:param(end-signal-str-lst)   - список имен [строк] дискретных сингалов тренда.)
@end(list)

 Логика деления на диапазоны следующая зависит от того, имеются-ли элементы в end-signal-str-lst.

 Если элементов в end-signal-str-lst нет:
@begin(list)
 @item(возвращаются диапазоны для, которых все сигналы, соответствующие 
       списку start-signal-str-lst установлены [равны единице].)
@end(list)

 Если элементы в end-signal-str-lst есть:
возвращаются диапазоны:
@begin(list)
 @item(в первой записи которых все сигналы, 
       соответствующие списку start-signal-str-lst установлены [равны единице];)
 @item(в следующей после последней записи которых все сигналы, 
       соответствующие списку end-signal-str-lst установлены [равны единице].)
@end(list)
"
  (let* ((start-flag-lst (mapcar
                          #'(lambda(el) (gethash el (r/trd:<trd>-discret-ht trd)))
                          start-signal-str-lst))
	 (end-flag-lst   (mapcar
                          #'(lambda(el) (gethash el (r/trd:<trd>-discret-ht trd)))
                          end-signal-str-lst))
	 (fl-start nil)
	 (fl-end   nil)
	 (total-rec (r/trd:<trd>-records trd))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1))
    (dotimes (i (r/trd:<trd>-records trd) (nreverse rez-lst))
      (setf fl-start (or fl-start (apply-and (r/get:trd-discret-by-record-t-nil trd i start-flag-lst))))
      (if fl-start
	  (progn
	    (setf fl-end nil
		  n-start (min i n-start)
		  n-end   (max i n-end))))
      (if end-flag-lst
	  (setf fl-end   (or fl-end   (apply-and
                                       (r/get:trd-discret-by-record-t-nil trd i end-flag-lst))))
	  (setf fl-end   (or fl-end   (not (apply-and
                                            (r/get:trd-discret-by-record-t-nil trd i start-flag-lst))))))
      (if (and fl-start fl-end (< -1 n-end))
	  (progn
	    (push (list n-start n-end) rez-lst)
	    (setf 	 fl-start nil
			 fl-end   nil
			 n-start total-rec
			 n-end -1))))))
