(defpackage :recoder/get
  (:use #:cl)
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
           )
  (:documentation "@b(Описание:) пакет @b(recoder/get) предназначен для получения о
значений аналоговых и дискретных сигналов из тренда."))

(in-package :recoder/get)

(defparameter *offset*  10
  "@b(Описание:) переменная @b(*offset*) количество
  записей тренда отсчитываемое влево и вправо от текущей записи для
  определения среднего значения и стандартнорго отклонения.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric trd-analog-mid-by-snames (trd utime snames &key n-before n-after)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(trd-analog-mid-by-snames)
 возвращает список средних значений сигналов, записанных в тренде
 @b(trd) в момент времени @b(utime) для списка сигналов, определяемых
 их именами @b(snames).

 Осреднение происходит в интервале записей от @b(n-before) до @b(n-after).")
  )

(defmethod trd-analog-mid-by-snames ((trd r/trd:<trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let* ((trd (r:trd-open (mnas-path:asdf-path \"recoder\" \"trd/2018-11-06_092329.trd\")))
         (snames '(\"V2\" \"ET022\"))
         (u-start(r/trd:utime-start trd)) 
         (u-end  (r/trd:utime-end   trd))
         (p 0.5)
         (u (+ u-start (* p (- u-end u-start)))))
    (trd-analog-mid-by-snames trd u snames))  
@end(code)
"
  (when  (r/trd:<trd>-file-descr trd)
    (trd-analog-mid-by-utime trd utime (r/slist:a-signals trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-stddev-by-utime ( (trd r/trd:<trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
 записанных в тренде @b(trd) в момент времени utime для списка сигналов
 signal-list; Осреднение происходит в интервале записей от n-before до
 n-after.
"
  (when  (r/trd:<trd>-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (r/trd:utime->record trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		     (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:standard-deviation rezult))))

(defmethod trd-analog-stddev-by-snames ((trd r/trd:<trd>) utime snames &key (n-before *offset*) (n-after *offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (r/trd:<trd>-file-descr trd)
    (trd-analog-stddev-by-utime trd utime (r/slist:a-signals trd snames) :n-before n-before :n-after n-after)))

(defmethod trd-analog-by-record ((trd r/trd:<trd>) record signal-list)
  "@b(Описание:) метод @b(trd-analog-by-record) возвращает список
значений тренда @b(trd)  для записи под номером record,
соответствующий сигналам signal-list."
  (when (and (r/trd:<trd>-file-descr trd) (< -1 record (r/trd:<trd>-records trd)))
    (file-position (r/trd:<trd>-file-descr trd) 
		   (+ (r/trd:start-offset trd) (* record (r/trd:record-length trd))))
    (let* ((v-sh (make-array (r/trd:<trd>-a-number trd) :element-type 'integer)))
      (dotimes (i (r/trd:<trd>-a-number trd) 'done)
	(setf (svref v-sh i)
	      (r/bin:b-read-short (r/trd:<trd>-file-descr trd))))
      (mapcar
       #'(lambda(el)
           (r/a-sig:decode-value
            (svref v-sh (r/a-sig:<a-signal>-num el))
            el))
	      signal-list))))

(defmethod trd-analog-by-utime ( (trd r/trd:<trd>) utime signal-list)
  "@b(Описание:) метод @b(trd-analog-by-utime) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (trd-analog-by-record trd
			    (r/trd:utime->record trd utime)
			    signal-list))

(defmethod trd-analog-mid-by-utime ((trd r/trd:<trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (r/trd:<trd>-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (r/trd:utime->record trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		     (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:average-value rezult))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-discret-by-record ((trd r/trd:<trd>) record d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
 значений тренда <trd> для записи под номером record,
 соответствующий сигналам d-signals."
  (when (and (r/trd:<trd>-file-descr trd) (< -1 record (r/trd:<trd>-records trd)))
    (file-position (r/trd:<trd>-file-descr trd) 
		   (+ (r/trd:start-offset trd)
		      (* record (r/trd:record-length trd))
		      (r/trd:discret-offset trd) ))
    (let ((s-int (r/bin:list-to-int
                  (r/bin:b-read
                   (r/trd:<trd>-file-descr trd)
                   (r/trd:discret-length trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (r/d-sig:<d-signal>-num  el ) s-int) 1 0))
	      d-signals))))

(defmethod trd-discret-by-record-t-nil ( (trd r/trd:<trd>) record d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record-t-nil) возвращает 
список значений тренда trd для записи под номером record,
соответствующий сигналам d-signals."
  (when (and (r/trd:<trd>-file-descr trd) (< -1 record (r/trd:<trd>-records trd)))
    (file-position (r/trd:<trd>-file-descr trd) 
		   (+ (r/trd:start-offset trd)
		      (* record (r/trd:record-length trd))
		      (r/trd:discret-offset trd) ))
    (let ((s-int (r/bin:list-to-int
                  (r/bin:b-read
                   (r/trd:<trd>-file-descr trd)
                   (r/trd:discret-length trd)))))
      (mapcar #'(lambda (el)
		  (logbitp (r/d-sig:<d-signal>-num el ) s-int))
	      d-signals))))

(defmethod trd-discret-by-utime ( (trd r/trd:<trd>) utime d-signals)
  "trd-discret-by-utime"
  (trd-discret-by-record trd (r/trd:utime->record trd utime) d-signals))

(defmethod trd-discret-by-utime-t-nil ( (trd r/trd:<trd>) utime d-signals)
  "trd-discret-by-utime-t-nil"
  (trd-discret-by-record-t-nil trd (r/trd:utime->record trd utime) d-signals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod trd-analog-discret-by-record ((trd r/trd:<trd>) record a-signals d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
  значений тренда <trd> для записи под номером @b(record),
  соответствующий сигналам d-signals."
  (append (trd-analog-by-record  trd record a-signals)
	  (trd-discret-by-record trd record d-signals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analogs-in-records ((trd r/trd:<trd>) start-record end-record a-signals)
  "@b(Описание:) метод @b(analogs-in-records) возвращает список
значений аналоговых сигналов, содержащися в списке @b(a-signals),
тренда @b(trd), начиная с записи @b(start-record) включительно 
до записи @b(end-record) исключительно."
  (when  (r/trd:<trd>-file-descr trd)
    (math/matr:transpose
     (loop :for i :from start-record :below end-record
	   :collect (trd-analog-by-record trd i a-signals)))))

(defmethod analogs-in-utimes ((trd r/trd:<trd>) start-utime end-utime a-signals)
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(a-signals),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (r/trd:<trd>-file-descr trd)
    (analogs-in-records trd
			(r/trd:utime->record trd start-utime)
			(r/trd:utime->record trd end-utime)
			a-signals)))

(defmethod trd-discret-by-record ( (trd r/trd:<trd>) rec-number d-signals)
  "@b(Описание:) метод @b(trd-discret-by-record)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signals."
  (when (and (r/trd:<trd>-file-descr trd) (< -1 rec-number (r/trd:<trd>-records trd)))
    (file-position (r/trd:<trd>-file-descr trd) 
		   (+ (r/trd:start-offset trd)
		      (* rec-number (r/trd:record-length trd))
		      (r/trd:discret-offset trd) ))
    (let ((s-int (r/bin:list-to-int
                  (r/bin:b-read
                   (r/trd:<trd>-file-descr trd)
                   (r/trd:discret-length trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (r/d-sig:<d-signal>-num  el ) s-int) 1 0))
	      d-signals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-a-ids (a-names (trd r/trd:<trd>))
  "@b(Описание:) метод @b(trd-a-ids) возвращает имена 
идентификаторов аналоговых сигналов.
@begin(list)
 @item(a-names - список имен сигналов;)
 @item(trd     - тренд. )
@end(list)
"
  (mapcar
   #'(lambda (a-s) (r/a-sig:<a-signal>-id a-s))
   (r/slist:a-signals trd a-names)))

(defmethod trd-a-units (a-names (trd r/trd:<trd>))
  "@b(Описание:) метод @b(trd-a-units) возвращает размерности 
аналоговых сигналов.
@begin(list)
 @item(a-names - список имен аналоговых сигналов;)
 @item( trd    - тренд.)
@end(list)
"
  (mapcar #'(lambda (a-s) (r/a-sig:<a-signal>-units a-s))
          (r/slist:a-signals trd a-names)))
