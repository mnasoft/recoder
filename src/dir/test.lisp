;; ./src/dir/test.lisp

;;;; (in-package :recoder/dir)

(in-package :recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd* (make-instance
		     'recoder:<trd-seq>
		     :trd-file-name "d:/PRG/msys32/home/namatv/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
		     :signal-strings '("GQ010" "EN1" "EN2" "EN3" "T04" "Oil2Gas" "FK280" "FK290" "FK310" "FK301")))

(elt *trd* 11338)

(position-if
 #'(lambda (el)
     (= 1
	(first (nthcdr 9 el))))
 *trd*
 :start 11252
 ) ; => 11338 (14 bits, #x2C4A)

(nthcdr 9 (elt *trd* (+ 11252 (* 5 17))))

(recoder:trd-open *trd*)

(split-on-intervals-when-flag-is-on *trd* "Oil2Gas")

((10931 11252) (15413 15677) (19598 19858) (24705 24971) (29177 29440)
 (33479 33755) (37542 37813) (42104 42367))

;;;; (trd-discret-by-rec-number  *trd* 12300  (trd-discret-signal-list *trd* '("Oil2Gas")))



(trd-separate-signals *trd* '("GQ010" "EN1" "EN2" "EN3" "T04" "Oil2Gas"))

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
