(defpackage #:recoder/get
  (:use #:cl
        #:recoder/binary
        #:recoder/d-signal
        #:recoder/a-signal
        #:recoder/trd
        #:recoder/slist
        #:mnas-string/print)
  (:export *offset*)
  (:export trd-analog-mid-by-snames
           trd-analog-by-rec-number
           trd-analog-by-utime
           trd-analog-mid-by-utime
           trd-analog-stddev-by-snames
           trd-analog-stddev-by-utime)
  (:export trd-discret-by-rec-number
           trd-discret-by-rec-number-t-nil
           trd-discret-by-utime
           trd-discret-by-utime-t-nil)
  (:export trd-analog-discret-by-rec-number)
  (:export analogs-in-records
           analogs-in-utimes
           ))

(in-package #:recoder/get)

(defparameter *offset*  10
  "@b(Описание:) переменная @b(*offset*) количество
  записей тренда отсчитываемое влево и вправо от текущей записи для
  определения среднего значения и стандартнорго отклонения.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-analog-mid-by-snames ((trd <trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "@b(описание:) метод @b(trd-analog-mid-by-snames) возвращает список
 средних значений параметров, записанных в тренде trd в момент времени
 utime для списка сигналов, определяемых их именами snames.

 Осреднение происходит в интервале записей от @b(n-before) до @b(n-after)."
  (when  (<trd>-file-descr trd)
    (trd-analog-mid-by-utime trd utime (trd-analog-signal-list trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-stddev-by-utime ( (trd <trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
 записанных в тренде trd в момент времени utime для списка сигналов
 signal-list; Осреднение происходит в интервале записей от n-before до
 n-after.
"
  (when  (<trd>-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-analog-by-rec-number trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:standard-deviation rezult))))

(defmethod trd-analog-stddev-by-snames ((trd <trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (<trd>-file-descr trd)
    (trd-analog-stddev-by-utime trd utime (trd-analog-signal-list trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-by-rec-number ((trd <trd>) rec-number signal-list)
  "@b(Описание:) метод @b(trd-analog-by-rec-number) возвращает список
значений тренда @b(trd)  для записи под номером rec-number,
соответствующий сигналам signal-list."
  (when (and (<trd>-file-descr trd) (< -1 rec-number (<trd>-total-records trd)))
    (file-position (<trd>-file-descr trd) 
		   (+ (trd-start-offset trd) (* rec-number (trd-record-length trd))))
    (let* ((v-sh (make-array (<trd>-analog-number trd) :element-type 'integer)))
      (dotimes (i (<trd>-analog-number trd) 'done)
	(setf (svref v-sh i)
	      (b-read-short (<trd>-file-descr trd))))
      (mapcar #'(lambda(el) (<a-signal>-value el (svref v-sh (<a-signal>-num el))))
	      signal-list))))

(defmethod trd-analog-by-utime ( (trd <trd>) utime signal-list)
  "@b(Описание:) метод @b(trd-analog-by-utime) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (trd-analog-by-rec-number trd
			    (trd-record-number-by-utime trd utime)
			    signal-list))

(defmethod trd-analog-mid-by-utime ((trd <trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (<trd>-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/list-matr:transpose rez))
		     (push (trd-analog-by-rec-number trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:average-value rezult))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-discret-by-rec-number ((trd <trd>) rec-number d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number) возвращает список
 значений тренда <trd> для записи под номером rec-number,
 соответствующий сигналам d-signal-list."
  (when (and (<trd>-file-descr trd) (< -1 rec-number (<trd>-total-records trd)))
    (file-position (<trd>-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (<trd>-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signal-list))))

(defmethod trd-discret-by-rec-number-t-nil ( (trd <trd>) rec-number d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number-t-nil) возвращает 
список значений тренда trd для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (when (and (<trd>-file-descr trd) (< -1 rec-number (<trd>-total-records trd)))
    (file-position (<trd>-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (<trd>-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (logbitp (<d-signal>-num  el ) s-int))
	      d-signal-list))))

(defmethod trd-discret-by-utime ( (trd <trd>) utime d-signal-list)
  "trd-discret-by-utime"
  (trd-discret-by-rec-number trd (trd-record-number-by-utime trd utime) d-signal-list))

(defmethod trd-discret-by-utime-t-nil ( (trd <trd>) utime d-signal-list)
  "trd-discret-by-utime-t-nil"
  (trd-discret-by-rec-number-t-nil trd (trd-record-number-by-utime trd utime) d-signal-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod trd-analog-discret-by-rec-number ((trd <trd>) rec-number a-signal-list d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number) возвращает список
  значений тренда <trd> для записи под номером rec-number,
  соответствующий сигналам d-signal-list."
  (append (trd-analog-by-rec-number  trd rec-number a-signal-list)
	  (trd-discret-by-rec-number trd rec-number d-signal-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analogs-in-records ((trd <trd>) start-record end-record signal-list)
  "@b(Описание:) метод @b(analogs-in-records) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), начиная с записи @b(start-record) включительно 
до записи @b(end-record) исключительно."
  (when  (<trd>-file-descr trd)
    (math/list-matr:transpose
     (loop :for i :from start-record :below end-record
	   :collect (trd-analog-by-rec-number trd i signal-list)))))

(defmethod analogs-in-utimes ((trd <trd>) start-utime end-utime signal-list)
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (<trd>-file-descr trd)
    (analogs-in-records trd
			(trd-record-number-by-utime trd start-utime)
			(trd-record-number-by-utime trd end-utime)
			signal-list)))
