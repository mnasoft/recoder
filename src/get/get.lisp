(defpackage :recoder/get
  (:use #:cl)
  (:nicknames "R/GET")
  (:export *offset*)
  (:export trd-analog-mid-by-snames ;; doc+
           trd-analog-stddev-by-snames ;; doc+
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
  (:export signal-value
           )
  (:documentation "@b(Описание:) пакет @b(recoder/get) предназначен для получения о
значений аналоговых и дискретных сигналов из тренда."))

(in-package :recoder/get)

(defparameter *offset*  10
  "@b(Описание:) переменная @b(*offset*) количество
  записей тренда отсчитываемое влево и вправо от текущей записи для
  определения среднего значения и стандартнорго отклонения.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defgeneric

(defgeneric trd-analog-mid-by-snames (trd utime snames &key n-before n-after)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(trd-analog-mid-by-snames)
 возвращает список средних значений сигналов, записанных в тренде
 @b(trd) в момент времени @b(utime) для списка сигналов, определяемых
 их именами @b(snames).

 Осреднение происходит в интервале записей от @b(n-before) до @b(n-after)."))
(defgeneric trd-analog-stddev-by-snames (trd utime snames &key n-before n-after)
  (:documentation
   "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"))
(defgeneric trd-analog-by-utime (trd utime signal-list)
  (:documentation
   "@b(Описание:) метод @b(trd-analog-by-utime) возвращает список значений
 аналоговых сигналов, содержащися в списке @b(signal-list), тренда
 @b(trd), соответствующих моменту времени @b(utime)."))
(defgeneric trd-analog-stddev-by-utime (trd utime signal-list &key n-before n-after)
  (:documentation
  "Возвращает список стандартных отклонений для параметров,
 записанных в тренде @b(trd) в момент времени utime для списка
 сигналов signal-list; Осреднение происходит в интервале записей от
 n-before до n-after."))
(defgeneric trd-analog-by-record (trd record signal-list)
  (:documentation
   "@b(Описание:) метод @b(trd-analog-by-record) возвращает список
 значений тренда @b(trd) для записи под номером record,
 соответствующий сигналам signal-list."))
(defgeneric trd-analog-mid-by-utime (trd utime signal-list &key n-before n-after)
  (:documentation
   "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
 осредненных значений аналоговых сигналов, содержащися в списке
 @b(signal-list), тренда @b(trd), соответствующих моменту времени
 @b(utime)."))
(defgeneric trd-discret-by-record (trd rec-number d-signals)
  (:documentation
   "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
значений тренда <trd> для записи под номером
rec-number,соответствующий сигналам d-signals."))
(defgeneric trd-discret-by-record-t-nil (trd record d-signals)
  (:documentation
   "@b(Описание:) метод @b(trd-discret-by-record-t-nil) возвращает 
 список значений тренда trd для записи под номером record,
 соответствующий сигналам d-signals."))
(defgeneric trd-analog-discret-by-record (trd record a-signals d-signals)
    (:documentation
       "@b(Описание:) метод @b(trd-discret-by-record) возвращает список
  значений тренда <trd> для записи под номером @b(record),
  соответствующий сигналам d-signals."))
(defgeneric trd-a-ids (a-names trd)
  (:documentation
   "@b(Описание:) метод @b(trd-a-ids) возвращает имена 
 идентификаторов аналоговых сигналов.
@begin(list)
 @item(a-names - список имен сигналов;)
 @item(trd     - тренд. )
@end(list)"))
(defgeneric trd-a-units (a-names trd)
    (:documentation
     "@b(Описание:) метод @b(trd-a-units) возвращает размерности 
аналоговых сигналов.
@begin(list)
 @item(a-names - список имен аналоговых сигналов;)
 @item( trd    - тренд.)
@end(list)"))
(defgeneric analogs-in-records (trd start-record end-record a-signals)
  (:documentation
   "@b(Описание:) метод @b(analogs-in-records) возвращает список
значений аналоговых сигналов, содержащися в списке @b(a-signals),
тренда @b(trd), начиная с записи @b(start-record) включительно 
до записи @b(end-record) исключительно."))
(defgeneric analogs-in-utimes (trd start-utime end-utime a-signals)
  (:documentation
     "@b(Описание:) метод @b(trd-analog-mid-by-utime) возвращает список
 осредненных значений аналоговых сигналов, содержащися в списке
 @b(a-signals),тренда @b(trd), соответствующих моменту времени
 @b(utime)."))
(defgeneric trd-discret-by-utime (trd utime d-signals)
  (:documentation
   "@b(Описание:) метод @b(trd-discret-by-utime) возвращает список
 значений для тренда @b(<trd>) на момент времени @b(utime),
 соответствующий дискретным сигналам из списка @b(d-signals)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defmethod

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
  (trd-analog-mid-by-utime trd
                           utime
                           (r/slist:a-signals trd snames)
                           :n-before n-before
                           :n-after n-after))

(defmethod trd-analog-stddev-by-utime ( (trd r/trd:<trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  (let* ((rez nil)
	 (n-start (- (r/trd:utime->record trd utime) n-before))
	 (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		   (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
    (mapcar #'math/stat:standard-deviation rezult)))

(defmethod trd-analog-stddev-by-snames ((trd r/trd:<trd>) utime snames &key (n-before *offset*) (n-after *offset*))
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let* ((trd (r:trd-open (mnas-path:asdf-path \"recoder\" \"trd/2018-11-06_092329.trd\")))
         (snames '(\"V2\" \"ET022\"))
         (u-start(r/trd:<trd>-utime-start trd)) 
         (u-end  (r/trd:utime-end   trd))
         (p 0.5)
         (u (+ u-start (* p (- u-end u-start)))))
  (trd-analog-mid-by-snames trd u snames))
@end(code)
"
  (trd-analog-stddev-by-utime trd utime
                              (r/slist:a-signals trd snames)
                              :n-before n-before
                              :n-after n-after))

#+nil (defmethod trd-analog-by-record ((trd r/trd:<trd>) record signal-list)

  (when (and (r/trd:<trd>-file-descr trd) (< -1 record (r/trd:<trd>-records trd)))
    (file-position (r/trd:<trd>-file-descr trd) 
		   (+ (r/trd:start-offset trd) (* record (r/trd:record-length trd))))
    (let* ((v-sh (make-array (r/trd:<trd>-a-number trd) :element-type 'integer)))
      (dotimes (i (r/trd:<trd>-a-number trd) 'done)
	(setf (svref v-sh i)
	      (r/bin:b-read-ushort (r/trd:<trd>-file-descr trd))))
      (mapcar
       #'(lambda(el)
           (r/a-sig:decode-value
            (svref v-sh (r/a-sig:<a-signal>-num el))
            el))
       signal-list))))

(defmethod trd-analog-by-record ((trd r/trd:<trd>) record signal-list)
  (signal-value trd record signal-list))

(defmethod trd-analog-by-utime ( (trd r/trd:<trd>) utime signal-list)

  (trd-analog-by-record trd
			    (r/trd:utime->record trd utime)
			    signal-list))

(defmethod trd-analog-mid-by-utime ((trd r/trd:<trd>) utime signal-list &key (n-before *offset*) (n-after *offset*))
  (let* ((rez nil)
	 (n-start (- (r/trd:utime->record trd utime) n-before))
	 (rezult (dotimes (i (+ n-before n-after 1) (math/matr:transpose rez))
		   (push (trd-analog-by-record trd (+ n-start i) signal-list) rez))))
    (mapcar #'math/stat:average-value rezult)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-discret-by-record-t-nil ((trd r/trd:<trd>) record d-signals)
  (mapcar
   #'(lambda (el)
       (when (= el 0)))
   (trd-discret-by-record trd  record d-signals)))

(defmethod trd-discret-by-utime ( (trd r/trd:<trd>) utime d-signals)
  (trd-discret-by-record trd
                         (r/trd:utime->record trd utime)
                         d-signals))

(defmethod trd-discret-by-utime-t-nil ( (trd r/trd:<trd>) utime d-signals)
  (trd-discret-by-record-t-nil trd
                               (r/trd:utime->record trd utime)
                               d-signals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-analog-discret-by-record ((trd r/trd:<trd>) record a-signals d-signals)
  (append (trd-analog-by-record  trd record a-signals)
	  (trd-discret-by-record trd record d-signals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod analogs-in-records ((trd r/trd:<trd>) start-record end-record a-signals)
    (math/matr:transpose
     (loop :for i :from start-record :below end-record
	   :collect (trd-analog-by-record trd i a-signals))))



(defmethod analogs-in-utimes ((trd r/trd:<trd>) start-utime end-utime a-signals)
  (analogs-in-records trd
		      (r/trd:utime->record trd start-utime)
		      (r/trd:utime->record trd end-utime)
		      a-signals))

(defmethod trd-discret-by-record ((trd r/trd:<trd>) rec-number d-signals)
  (signal-value trd rec-number d-signals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-a-ids (a-names (trd r/trd:<trd>))
  (mapcar
   #'(lambda (a-s) (r/a-sig:<a-signal>-id a-s))
   (r/slist:a-signals trd a-names)))



(defmethod trd-a-units (a-names (trd r/trd:<trd>))

  (mapcar #'(lambda (a-s) (r/a-sig:<a-signal>-units a-s))
          (r/slist:a-signals trd a-names)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod signal-value ((trd r/trd:<trd>) record (a-signal r/a-sig:<a-signal>))
  (when (< -1 record (r/trd:<trd>-records trd))
    (setf (trivial-octet-streams::index (r/trd:<trd>-oc-i-sream trd))
          (+ (* (r/a-sig:<a-signal>-num a-signal) 2)
             (* record (r/trd:record-length trd))))
    (r/a-sig:decode-value
     (r/bin:b-read-ushort (r/trd:<trd>-oc-i-sream trd))
     a-signal)))

(defmethod signal-value ((trd r/trd:<trd>) record (d-signal r/d-sig:<d-signal>))
  (when (< -1 record (r/trd:<trd>-records trd))
    (multiple-value-bind (offset bit-position)
        (floor (r/d-sig:<d-signal>-num d-signal) 8)
      (setf (trivial-octet-streams::index (r/trd:<trd>-oc-i-sream trd))
            (+ (* record (r/trd:record-length trd))
               (r/trd:discret-offset trd)
               offset))
      (ldb (byte 1 bit-position)
           (r/bin:b-read-uchar (r/trd:<trd>-oc-i-sream trd))))))

(defmethod signal-value ((trd r/trd:<trd>) record (signals cons))
  (mapcar #'(lambda (sig) (signal-value trd record sig)) signals))
