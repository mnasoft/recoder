(defpackage #:recoder/get
  (:use #:cl
        #:recoder/binary
        #:recoder/d-signal
        #:recoder/a-signal
        #:recoder/trd
        #:recoder/slist
        #:mnas-string/print)
  (:nicknames "R/GET")
  (:export *offset*)
  (:export trd-analog-mid-by-snames
           trd-analog-stddev-by-snames
           trd-analog-mid-by-utime
           trd-analog-stddev-by-utime
           trd-analog-by-record
           trd-analog-by-utime
           )
  (:export trd-discret-by-record
           trd-discret-by-record-t-nil
           trd-discret-by-utime
           trd-discret-by-utime-t-nil)
  (:export trd-analog-discret-by-record)
  (:export analogs-in-records
           analogs-in-utimes
           )
  (:export trd-a-ids
           trd-a-units
           ))

(in-package #:recoder/get)

(defparameter *offset*  10
  "@b(Описание:) переменная @b(*offset*) количество
  записей тренда отсчитываемое влево и вправо от текущей записи для
  определения среднего значения и стандартнорго отклонения.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-analog-mid-by-snames ((trd <trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "@b(описание:) метод @b(trd-analog-mid-by-snames) возвращает список
 средних значений сигналов, записанных в тренде @b(trd) в момент
 времени utime для списка сигналов, определяемых их именами snames.

 Осреднение происходит в интервале записей от @b(n-before) до @b(n-after)."
  (when  (file-descr trd)
    (trd-analog-mid-by-utime trd utime (a-signals trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-stddev-by-utime ( (trd <trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
 записанных в тренде @b(trd) в момент времени utime для списка сигналов
 signal-list; Осреднение происходит в интервале записей от n-before до
 n-after.
"
  (when  (file-descr trd)
    (let* ((rez nil)
	   (n-start (- (utime->record trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		     (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:standard-deviation rezult))))

(defmethod trd-analog-stddev-by-snames ((trd <trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (file-descr trd)
    (trd-analog-stddev-by-utime trd utime (a-signals trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-by-record ((trd <trd>) record signal-list)
  "@b(Описание:) метод @b(trd-analog-by-record) возвращает список
значений тренда @b(trd)  для записи под номером record,
соответствующий сигналам signal-list."
  (when (and (file-descr trd) (< -1 record (records trd)))
    (file-position (file-descr trd) 
		   (+ (start-offset trd) (* record (record-length trd))))
    (let* ((v-sh (make-array (a-number trd) :element-type 'integer)))
      (dotimes (i (a-number trd) 'done)
	(setf (svref v-sh i)
	      (b-read-short (file-descr trd))))
      (mapcar #'(lambda(el) (<a-signal>-value el (svref v-sh (<a-signal>-num el))))
	      signal-list))))

(defmethod trd-analog-by-utime ( (trd <trd>) utime signal-list)
  "@b(Описание:) метод @b(trd-analog-by-utime) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (trd-analog-by-record trd
			    (utime->record trd utime)
			    signal-list))

(defmethod trd-analog-mid-by-utime ((trd <trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (file-descr trd)
    (let* ((rez nil)
	   (n-start (- (utime->record trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		     (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:average-value rezult))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-discret-by-record ((trd <trd>) record d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
 значений тренда <trd> для записи под номером record,
 соответствующий сигналам d-signals."
  (when (and (file-descr trd) (< -1 record (records trd)))
    (file-position (file-descr trd) 
		   (+ (start-offset trd)
		      (* record (record-length trd))
		      (discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (file-descr trd) (discret-length trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signals))))

(defmethod trd-discret-by-record-t-nil ( (trd <trd>) record d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record-t-nil) возвращает 
список значений тренда trd для записи под номером record,
соответствующий сигналам d-signals."
  (when (and (file-descr trd) (< -1 record (records trd)))
    (file-position (file-descr trd) 
		   (+ (start-offset trd)
		      (* record (record-length trd))
		      (discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (file-descr trd) (discret-length trd)))))
      (mapcar #'(lambda (el)
		  (logbitp (<d-signal>-num  el ) s-int))
	      d-signals))))

(defmethod trd-discret-by-utime ( (trd <trd>) utime d-signals)
  "trd-discret-by-utime"
  (trd-discret-by-record trd (utime->record trd utime) d-signals))

(defmethod trd-discret-by-utime-t-nil ( (trd <trd>) utime d-signals)
  "trd-discret-by-utime-t-nil"
  (trd-discret-by-record-t-nil trd (utime->record trd utime) d-signals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod trd-analog-discret-by-record ((trd <trd>) record a-signals d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
  значений тренда <trd> для записи под номером @b(record),
  соответствующий сигналам d-signals."
  (append (trd-analog-by-record  trd record a-signals)
	  (trd-discret-by-record trd record d-signals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analogs-in-records ((trd <trd>) start-record end-record a-signals)
  "@b(Описание:) метод @b(analogs-in-records) возвращает список
значений аналоговых сигналов, содержащися в списке @b(a-signals),
тренда @b(trd), начиная с записи @b(start-record) включительно 
до записи @b(end-record) исключительно."
  (when  (file-descr trd)
    (math/matr:transpose
     (loop :for i :from start-record :below end-record
	   :collect (trd-analog-by-record trd i a-signals)))))

(defmethod analogs-in-utimes ((trd <trd>) start-utime end-utime a-signals)
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(a-signals),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (file-descr trd)
    (analogs-in-records trd
			(utime->record trd start-utime)
			(utime->record trd end-utime)
			a-signals)))

(defmethod trd-discret-by-record ( (trd <trd>) rec-number d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signals."
  (when (and (file-descr trd) (< -1 rec-number (records trd)))
    (file-position (file-descr trd) 
		   (+ (start-offset trd)
		      (* rec-number (record-length trd))
		      (discret-offset trd) ))
    (let ((s-int (list-to-int (b-read (file-descr trd) (discret-length trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-a-ids (a-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-ids) возвращает имена 
идентификаторов аналоговых сигналов.
@begin(list)
 @item(a-names - список имен сигналов;)
 @item(trd     - тренд. )
@end(list)
"
  (mapcar
   #'(lambda (a-s) (<a-signal>-id a-s))
   (recoder/slist:a-signals trd a-names)))

(defmethod trd-a-units (a-names (trd <trd>))
  "@b(Описание:) метод @b(trd-a-units) возвращает размерности 
аналоговых сигналов.
@begin(list)
 @item(a-names - список имен аналоговых сигналов;)
 @item( trd    - тренд.)
@end(list)
"
  (mapcar #'(lambda (a-s) (<a-signal>-units a-s))
          (recoder/slist:a-signals trd a-names)))
