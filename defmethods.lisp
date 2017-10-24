;;;; defmethods.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    BAK    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-open ((x trd))
  "Выполняет открытие файла тренда включая:
- чтение заголовка;
- разбор аналоговых сигналов;
- разбор дискретных сигналов"
  (when (null (trd-file-descr x))
    (setf (trd-file-descr x) (open-trd-file-read (trd-file-name x)))
    (let ((in (trd-file-descr x)) (bufer nil) (date-day nil) (date-month nil) (date-year nil) (time-hour nil) (time-minute nil) (time-second nil)
	  (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil)
	  (discret-id nil) (discret-description nil))
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
	    (trd-date-time x)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (trd-reserv x)         (read-trd-file-short in)
	    (trd-total-records x)  (read-trd-file-long in)
	    (trd-delta-time x)     (read-trd-file-double in)
	    (trd-analog-number x)  (read-trd-file-short in)
	    (trd-discret-number x) (read-trd-file-short in)
	    (trd-analog-ht x)      (make-hash-table :test #'equal :size (trd-analog-number x))
	    (trd-discret-ht x)     (make-hash-table :test #'equal :size (trd-discret-number x)))
      (setf (trd-total-records x)
	    (/ (- (file-length (trd-file-descr x)) (trd-start-offset x))
	       (trd-record-length x)))
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
								   :a-signal-max analog-max)))
      (dotimes (i (trd-discret-number x) 'done)
	(setf discret-id          (recode-string (read-trd-file in *signal-id-wid*))
	      discret-description (recode-string (read-trd-file in *signal-description-wid*))
	      (gethash discret-id (trd-discret-ht x)) (make-instance 'd-signal
								     :d-signal-num i
								     :d-signal-id discret-id
								     :d-signal-description discret-description)))))
  x)


;;;;    BAK    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
