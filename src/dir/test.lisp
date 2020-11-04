;; ./src/dir/test.lisp

;;;; (in-package :recoder/dir)

(in-package :recoder)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-find-if-01 (predicate (trd <trd>) signal-str &key from-end (start 0) end key)
  "
- [ ] метод поиска который бы позволял:
  - искать как в прямом (увеличение записей) так и в обратном направлении;
  - начинать поиск с начала, с конца или с произвольного моммента времени;
  - искать до тех пор пока не встретится условие, определяемое функцией такого количества параметров.
"
  (let* ((s-list (trd-separate-signals trd signal-str))
	 (a-signals (first  s-list))
	 (d-signals (second s-list)))
    (trd-analog-discret-by-rec-number trd start a-signals d-signals)))

(trd-find-if-01
 #'(lambda (a-signals d-signals) nil)
 *trd*
 '("GQ010" "EN1" "EN2" "EN3" "T04" "Oil2Gas" "FK301" "FK310")
 :start 12000
 )

(trd-discret-by-rec-number  *trd* 12300  (trd-discret-signal-list *trd* '("Oil2Gas")))

(do ((i start (1+ i))) ((funcall predicate trd i )))

(trd-separate-signals *trd* '("GQ010" "EN1" "EN2" "EN3" "T04" "Oil2Gas"))

(maphash #'(lambda (k v) (format t "~S~%" k))
	 (trd-analog-ht *trd*)
	 )

(gethash "GQ010" (trd-analog-ht *trd*))

(loop :for i :from start :below (trd-total-records trd)
      )

(defparameter *trd* (make-instance
		     'recoder:<trd>
		     :trd-file-name
		     "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"))

(recoder:trd-open *trd*)

(split-on-utimes-when-flag-is-on *trd* "Oil2Gas")


(mapcar #'math/stat:average-value (analogs-in-utimes *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04"))))

(trd-analog-mid-by-utime *trd*  (+ 3806391948 30) (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))

(math/stat:max-value     (nth 2 (analogs-in-utimes *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))))
(math/stat:min-value     (nth 2 (analogs-in-utimes *trd* 3806391948 3806392012 (trd-analog-signal-list *trd* '("GQ010" "EN1" "EN2" "EN3" "T04")))))


(recoder:trd-utime-by-record-number *trd* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (require :recoder/dir)

(defparameter *trd-CPIPES-dir*
  (make-instance '<trd-dir> :directory "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per"))

(split-on-intervals-when-flag-is-on *trd-CPIPES-dir* "Oil2Gas")

(split-on-utimes-when-flag-is-on    *trd-CPIPES-dir* "Oil2Gas")



(analogs-in-utimes 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((ut-s `(,(encode-universal-time 07 12 14 29 07 2020))))
  (analog-table ut-s
		`(,*trd-CPIPES-dir* ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-C100-dir*   ("GQ010" "EN1" "EN2" "EN3" "T04"))
		`(,*trd-NIO-dir*    ,(append (loop :for i :from 1 :to 76 :collect (format nil "T15_~2,'0D" i)) '("CF104" "CF140")))
		))

(recoder:trd-split-on-intervals-of-time-when-flag-is-on trd d-signal-str) 
(recoder:trd-split-on-intervals-by-condition trd start-signal-str-lst end-signal-str-lst)
(recoder:split-on-intervals-when-flag-is-on)
