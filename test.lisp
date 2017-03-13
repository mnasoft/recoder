;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-analog-ht *t*) )

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-discret-ht *t*) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod mid-values-by-snames (dir-name udate snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список средних значений параметров, 
записанных в тренде trd в момент времени udate для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-mid-values-by-udate x udate (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))

(defmethod stddev-values-by-snames ( (x trd) udate snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  "Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени udate для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr x)
    (trd-stddev-values-by-udate x udate (trd-analog-signal-list x snames) :n-before n-before :n-after n-after)))


(require :mnas-path)

(defun foo (utime dir-name  &key (extension "trd"))
  (let ((rezult nil))
    (mapcar
     #'(lambda (el)
	 (let ((trd (make-instance 'trd :trd-file-name el))
	       (rez nil))
	   (trd-open trd)
	   (push (trd-date-time-end trd) rez)
	   (push (trd-date-time trd)  rez)
	   (push el rez)
	   (if (<= (second rez) (third rez))
	       (setf rezult rez)
	       (trd-close trd))
	   rez))
     (mnas-path:find-filename "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" extension))
    rezult))

(defun foo (utime dir-name &key (extension "trd"))
  (let ((rezult nil))
    (mapcar
     #'(lambda (el)
	 (let ((trd (make-instance 'trd :trd-file-name el)))
	   (trd-open trd)
	   (if (<= (trd-date-time trd) utime (trd-date-time-end trd))
	       (setf rezult trd)
	       (trd-close trd))))
     (mnas-path:find-filename dir-name extension))
    rezult))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mid-values-by-snames (dir-name utime snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-mid-values-by-snames
   (foo utime dir-name :extension extension)
   utime snames :n-before n-before :n-after n-after))

(defmethod stddev-values-by-snames ( (x trd) udate snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-stddev-values-by-snames
   (foo utime dir-name :extension extension)
   utime snames :n-before n-before :n-after n-after))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foo (time-universal-encode 2017 02 20 16 27 15)
     "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" :extension "trd")



(mid-values-by-snames
 "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/"
 utime
 snames
 )
