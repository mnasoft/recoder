;;;; defmethods.lisp

(in-package #:recoder)

(defmethod print-object ((x trd) stream)
  (format stream "Path= ~S~%" (trd-file-name x) )
  (when (trd-file-descr x)
    (format stream "id=~S version=~A " (trd-id-string x) (trd-version x))
    (format stream "[ ")
    (mnas-string:print-universal-time (trd-utime-start x) :stream stream)
    (format stream " ; ")
    (mnas-string:print-universal-time (trd-utime-end x) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (trd-reserv x) (trd-total-records x) (trd-delta-time x) (trd-analog-number x) (trd-discret-number x))
    (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-analog-ht x) )
    (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-discret-ht x) )))

(defmethod trd-open ((x trd))
  "Выполняет открытие файла тренда включая:
- чтение заголовка;
- разбор аналоговых сигналов;
- разбор дискретных сигналов"
  (trd-read-header x)
  (trd-read-analog-ht x )
  (trd-read-discret-ht x)
  x)

(defmethod trd-read-header((x trd))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (trd-file-descr x))
    (setf (trd-file-descr x) (open-trd-file-read (trd-file-name x)))
    (let ((in (trd-file-descr x)) (bufer nil) (date-day nil) (date-month nil) (date-year nil) (time-hour nil) (time-minute nil) (time-second nil))
      (setf (trd-id-string x)      (recode-string (read-trd-file in *head-id-wid*))
	    (trd-version x)        (car (read-trd-file in *head-version-wid*))
	    bufer                  (read-trd-file in *head-date-wid*)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (read-trd-file in *head-time-wid*)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (trd-utime-start x)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (trd-reserv x)         (read-trd-file-short in)
	    (trd-total-records x)  (read-trd-file-long in)
	    (trd-delta-time x)     (read-trd-file-double in)
	    (trd-analog-number x)  (read-trd-file-short in)
	    (trd-discret-number x) (read-trd-file-short in))
      (setf (trd-total-records x)
	    (/ (- (file-length (trd-file-descr x)) (trd-start-offset x))
	       (trd-record-length x)))))
  x)

(defmethod trd-read-analog-ht((x trd))
  "Выполняет разбор аналоговых сигналов"
  (when (null (trd-analog-ht x))
    (setf (trd-analog-ht x)  (make-hash-table :test #'equal :size (trd-analog-number x)))
    (file-position (trd-file-descr x) *head-wid*)
    (let ((in (trd-file-descr x)) (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil))
      (dotimes (i (trd-analog-number x) 'done)
	(setf analog-id          (recode-string (read-trd-file in *signal-id-wid*))
	      analog-description (recode-string (read-trd-file in *signal-description-wid*))
	      analog-units       (recode-string (read-trd-file in *signal-units-wid*))
	      analog-min         (read-trd-file-double in)
	      analog-max         (read-trd-file-double in)
	      (gethash analog-id (trd-analog-ht x)) (make-instance 'a-signal
								   :a-signal-num i
								   :a-signal-id  analog-id
								   :a-signal-description analog-description
								   :a-signal-units analog-units
								   :a-signal-min analog-min
								   :a-signal-max analog-max))))))

(defmethod trd-read-discret-ht((x trd))
  "Выполняет разбор дискретных сигналов"
  (when (null (trd-discret-ht x))
    (setf (trd-discret-ht x) (make-hash-table :test #'equal :size (trd-discret-number x)))
    (file-position (trd-file-descr x) (+ *head-wid* (* (trd-analog-number x) *analog-wid*)))
    (let ((in (trd-file-descr x)) (discret-id nil) (discret-description nil))
      (dotimes (i (trd-discret-number x) 'done)
	(setf discret-id          (recode-string (read-trd-file in *signal-id-wid*))
	      discret-description (recode-string (read-trd-file in *signal-description-wid*))
	      (gethash discret-id (trd-discret-ht x)) (make-instance 'd-signal
								     :d-signal-num i
								     :d-signal-id discret-id
								     :d-signal-description discret-description))))))

(defmethod trd-close ((x trd))
  "Выполняет закрытие файла тренда"
  (when (trd-file-descr x)
    (close (trd-file-descr x))
    (setf (trd-file-descr x) nil)))

(defmethod trd-start-offset ((x trd))
  "Смещение для первой (нулевой) записи тренда"
    (+ *head-wid*
    (* (trd-analog-number x) *analog-wid*)
    (* (trd-discret-number x) *discret-wid*)))

(defmethod trd-analog-length-byte ((x trd))
  (* (trd-analog-number x) 2))

(defmethod trd-discret-length-byte ((x trd))
  (ceiling (/ (trd-discret-number x) 8)))

(defmethod trd-record-length ((x trd))
  "Длина одной записи тренда"
  (+  (trd-analog-length-byte x)  (trd-discret-length-byte x)))

(defmethod trd-discret-offset ((x trd))
  "Смещение от начала записи до начала записи дискретных сигналов"
  (+ (* (trd-analog-number x) 2)))

(defmethod trd-utime-end ((x trd))
  "Возвращает время окончания тренда. время возвращается в универсальном формате (universal-time)"
  (+ (trd-utime-start x)
     (floor (* (trd-total-records x) (trd-delta-time x)))))

(defmethod trd-analog-signal-list ( (x trd) signal-string-list)
  "Возвращает список аналоговых сигналов тренда trd, 
которые соответствуют списку обозначений сигналов из списка signal-string-list"
  (when  (trd-file-descr x)
    (mapcar #'(lambda(el)
		(gethash el (trd-analog-ht x)))
	    signal-string-list)))

(defmethod trd-discret-signal-list ( (x trd) signal-string-list)
  "Возвращает список дискретных сигналов тренда trd, 
которые соответствуют списку обозначений сигналов из списка signal-string-list"
  (when  (trd-file-descr x)
    (mapcar #'(lambda(el)
		(gethash el (trd-discret-ht x)))
	    signal-string-list)))

(defmethod trd-analog-by-rec-number ( (x trd) rec-number signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (when (and (trd-file-descr x) (< -1 rec-number (trd-total-records x)))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x) (* rec-number (trd-record-length x))))
    (let* ((v-sh (make-array (trd-analog-number x) :element-type 'integer)))
      (dotimes (i (trd-analog-number x) 'done)
	(setf (svref v-sh i)
	      (read-trd-file-short (trd-file-descr x))))
      (mapcar #'(lambda(el) (a-signal-value el (svref v-sh (a-signal-num el))))
	      signal-list))))

(defmethod trd-record-number-by-utime ( (x trd) utime)
  "Возвращает номер записи по универсальному времени"
  (floor (- utime (trd-utime-start x)) (trd-delta-time x)))

(defmethod trd-discret-by-rec-number ( (x trd) rec-number d-signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам d-signal-list"
  (when (and (trd-file-descr x) (< -1 rec-number (trd-total-records x)))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x)
		      (* rec-number (trd-record-length x))
		      (trd-discret-offset x) ))
    (let ((s-int (list-to-int (read-trd-file (trd-file-descr x) (trd-discret-length-byte x)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (d-signal-num  el ) s-int) 1 0))
	      d-signal-list))))

(defmethod trd-discret-by-rec-number-t-nil ( (x trd) rec-number d-signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам d-signal-list"
  (when (and (trd-file-descr x) (< -1 rec-number (trd-total-records x)))
    (file-position (trd-file-descr x) 
		   (+ (trd-start-offset x)
		      (* rec-number (trd-record-length x))
		      (trd-discret-offset x) ))
    (let ((s-int (list-to-int (read-trd-file (trd-file-descr x) (trd-discret-length-byte x)))))
      (mapcar #'(lambda (el)
		  (logbitp (d-signal-num  el ) s-int))
	      d-signal-list))))


(defmethod trd-analog-by-utime ( (x trd) utime signal-list)
  "Возвращает список значений тренда trd для записи под номером rec-number,
 соответствующий сигналам signal-list"
  (trd-analog-by-rec-number x
		  (trd-record-number-by-utime x utime)
		  signal-list))

(defmethod trd-analog-mid-by-utime ( (x trd) utime signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список средних значений параметров "
  (when  (trd-file-descr x)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime x utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-analog-by-rec-number x (+ n-start i) signal-list) rez))))
      (mapcar #'math:averange-value rezult))))

(defmethod trd-analog-mid-by-snames ( (x trd) utime snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список средних значений параметров, 
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-analog-mid-by-utime x utime (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-stddev-by-utime ( (x trd) utime signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов signal-list;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime x utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-analog-by-rec-number x (+ n-start i) signal-list) rez))))
      (mapcar #'math:standard-deviation rezult))))

(defmethod trd-analog-stddev-by-snames ( (x trd) utime snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-analog-stddev-by-utime x utime (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Singal separation to analog|discret|unexpected ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-separate-signals ((x trd) singnal-str-list)
  "Выполняет проверку того, что имена сигналов, заданные в переменной singnal-str-list,
присутствуют для данного тренда в перечне аналоговых сигналов или дискретных сигналов
или отсутствуют в обоих перечнях.
   Возвращает список элементами которого являются:
- список аналоговых сигналов;
- список дискретных сигналов;
- список строк с именами не соответствующими 
  ни аналоговым ни дискретным сигналам."
  (let ((a-rez nil)
	(d-rez nil)
	(error-rez nil)
	(error-fl t))
    (mapcar #'(lambda (el)
		(setf error-fl t)
		(multiple-value-bind (v r) (gethash  el (trd-analog-ht x ) )
		  (when r
		    (push v a-rez)
		    (setf error-fl nil)))
		(multiple-value-bind (v r) (gethash  el (trd-discret-ht x ) )
		  (when r (push v d-rez)
			(setf error-fl nil)))
		(when error-fl (push el error-rez)))
	    singnal-str-list)
    (list (nreverse a-rez) (nreverse d-rez) (nreverse error-rez))))

(defmethod trd-separate-a-signals ((x trd) singnal-str-list)
  "Выделяет из переменной singnal-str-list аналоговые сигналы"
  (first (trd-separate-signals x singnal-str-list)))

(defmethod trd-separate-d-signals ((x trd) singnal-str-list)
  "Выделяет из переменной singnal-str-list дискретные сигналы"
  (second (trd-separate-signals x singnal-str-list)))

(defmethod trd-separate-not-signals ((x trd) singnal-str-list)
  "Выделяет из переменной singnal-str-list неожиданные сигналы."
  (second (trd-separate-signals x singnal-str-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CVS export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-export-csv ((x trd) a-sig-lst d-sig-lst &key (os t) (n-start 0) (n-end (trd-total-records x)))
  "Производит вывод аналоговых сигналов тренда, 
заданных параметром a-sig-lst (список сигналов),
и дискретных сигналов, заданных параметром d-sig-lst (список сигналов),
в поток os имеющий форматирование csv, начиная с номера записи n-start до номера записи n-end включительно.
Перед выводом сигналов выводятся их обозначения."
  (format os "~{~S~^,~}~%" (append (list "U") (mapcar #'(lambda (el) (a-signal-units el)) a-sig-lst) (mapcar #'(lambda (el) "0|1") d-sig-lst)))
  (format os "~{~S~^,~}~%" (append (list "N") (mapcar #'(lambda (el) (a-signal-id el)) a-sig-lst) (mapcar #'(lambda (el) (d-signal-id el)) d-sig-lst)))
  
  (do ((i (max 0 n-start) (1+ i))
       (e (min (+ 1 n-end) (trd-total-records x))))
      ((>= i e) 'done)

    (format os "~{~F~^,~}~%" (append (list (* i (trd-delta-time x)))
				    (trd-analog-by-rec-number x i a-sig-lst)
				    (trd-discret-by-rec-number x i d-sig-lst)))))

(defmethod trd-export-csv-singal-string ((x trd) signal-str-list &key (os t) (n-start 0) (n-end (trd-total-records x)))
  (let ((a-d-e (trd-separate-signals x  signal-str-list )))
    (trd-export-csv x (first a-d-e) (second a-d-e) :os os :n-start n-start :n-end n-end)
    a-d-e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Splitting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-split-on-intervals-when-flag-is-on ((x trd) d-signal-str )
  "Для тренда x выполняет поиск диапазонов, для которых значение 
дискретного сигнала с именем d-signal-str имеет значение 1.
todo: доработать, чтоб возвращался последний диапазон при поднятом флаге в конце"
  (let* (
	 (flag (gethash d-signal-str (trd-discret-ht x)))
	 (flag-lst (list flag))
	 (total-rec (trd-total-records x))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1)
	 (rez nil)
	 )
    (dotimes (i (trd-total-records x) (nreverse rez-lst))
      (setf rez (first(trd-discret-by-rec-number-t-nil x i flag-lst)))
      (if rez
	  (setf n-start (min i n-start)
		n-end   (max i n-end))
	  (when (< -1 n-end)
	    (push (list n-start n-end) rez-lst)
	    (setf n-start total-rec
		  n-end -1))))))

(defmethod trd-split-on-intervals-of-time-when-flag-is-on ((x trd) d-signal-str)
  "Для тренда x выполняет поиск диапазонов, для которых
значение сигнала d-signal-str принимало значение 1. 
И возвращает длительность этих диапазонов"
  (let ((intervals (trd-flag-on-intervals x d-signal-str)))
    (values
     (mapcar #'(lambda (el) (* -1 (trd-delta-time x) (apply #'- el))) intervals)
     intervals)))

(defmethod trd-split-on-intervals-by-condition ((x trd) start-signal-str-lst end-signal-str-lst) 
  "Выполняет деление тренда на диапазоны. Возврвщает список.
Каждый элемент, возвращаемого списка, состоит из двух номеров записей - начальной и конечной.
   Параметры:
   ==========
x                      - объект типа trd [тренд];
start-signal-str-lst   - список имен [строк] дискретных сингалов тренда;
end-signal-str-lst     - список имен [строк] дискретных сингалов тренда.
Логика деления на диапазоны следующая зависит от того, имеются-ли элементы в end-signal-str-lst.
   Если элементов в end-signal-str-lst нет:
возвращаются диапазоны для, которых все сигналы, соответствующие 
списку start-signal-str-lst установлены [равны единице].
   Если элементы в end-signal-str-lst есть:
возвращаются диапазоны:
   - в первой записи которых все сигналы, соответствующие 
списку start-signal-str-lst установлены [равны единице];
   - в следующей после последней записи которых 
все сигналы, соответствующие списку end-signal-str-lst установлены [равны единице]
"
  (let* ((start-flag-lst (mapcar #'(lambda(el) (gethash el (trd-discret-ht x))) start-signal-str-lst))
	 (end-flag-lst   (mapcar #'(lambda(el) (gethash el (trd-discret-ht x))) end-signal-str-lst))
	 (fl-start nil)
	 (fl-end   nil)
	 (total-rec (trd-total-records x))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1))
    (dotimes (i (trd-total-records x) (nreverse rez-lst))
      (setf fl-start (or fl-start (apply-and (trd-discret-by-rec-number-t-nil x i start-flag-lst))))
      (if fl-start
	  (progn
	    (setf fl-end nil
		  n-start (min i n-start)
		  n-end   (max i n-end))))
      (if end-flag-lst
	  (setf fl-end   (or fl-end   (apply-and (trd-discret-by-rec-number-t-nil x i end-flag-lst))))
	  (setf fl-end   (or fl-end   (not (apply-and (trd-discret-by-rec-number-t-nil x i start-flag-lst))))))
      (if (and fl-start fl-end (< -1 n-end))
	  (progn
	    (push (list n-start n-end) rez-lst)
	    (setf 	 fl-start nil
			 fl-end   nil
			 n-start total-rec
			 n-end -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interval-to-time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-interval-to-secods ((x trd) interval)
  "Преобразует диапазон времени, заданный в записях, в секунды"
  (* (trd-delta-time x) (apply #'- (reverse interval))))

(defmethod trd-interval-to-minutes ((x trd) interval)
    "Преобразует диапазон времени, заданный в записях, в минуты"
  (* 1/60 (trd-interval-to-secods x interval)))

(defmethod trd-interval-to-hours ((x trd) interval)
      "Преобразует диапазон времени, заданный в записях, часы"
      (* 1/60 1/60 (trd-interval-to-secods x interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-record-number-to-udate ((x trd) rec-number)
  (+ (trd-utime-start x) (round (* rec-number (trd-delta-time  x)))))

(export 'trd-record-number-to-udate)

(defmethod trd-record-number-by-udate ((x trd) udate)
  (round
   (/
    (- udate (trd-utime-start x))
    (trd-delta-time x))))

(export 'trd-record-number-by-udate)