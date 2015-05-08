;;;; recoder.lisp

(in-package #:recoder)

;;; "recoder" goes here. Hacks and glory await!

(defun recode-string (bufer &optional (start 0) (len (length bufer)) &key (break-nul T))
  (do*
   ( (i start (1+ i))
     (ch (gethash (nth i bufer) *cp1251*) (gethash (nth i bufer) *cp1251*))
     (str-rez ""))
   ( (or (>= i (+ start len))
	 (and break-nul (= 0 (nth i bufer)))) 
    str-rez)
    (setf str-rez (concatenate 'string str-rez ch))))

(defun open-trd-file(path)
  "Выполняет открытие файла тренда."
  (open path :element-type 'unsigned-byte))

(defun read-trd-file(in byte-number)
  "Выполняет чтение bite-number из потока in"
  (let ((lst nil))
    (dotimes (i byte-number)
      (push (read-byte in) lst))
    (reverse lst)))

(defun read-trend-header (path &optional (byte-number 30))
  (let ((lst nil)
	(in (open path :element-type 'unsigned-byte)))
    (dotimes (i byte-number)
      (push (read-byte in) lst))
    (close in)
    (reverse lst)))

(let (
      (name-wid 10)
      (opis-wid 40)
      (dim-wid  24)
      )
  (defun analog-read(buffer num-rec)
      (list (recode-string buffer (+ start
			      (* num-rec
				 (+ name-wid opis-wid dim-wid))) name-wid)
	(recode-string buffer (+ start name-wid
			      (* num-rec (+ name-wid opis-wid dim-wid))) opis-wid)
	(recode-string buffer (+ start name-wid opis-wid
			      (* num-rec (+ name-wid opis-wid dim-wid))) dim-wid))
    ))

(let (
      (name-wid 10)
      (opis-wid 40)
      )
(defun diskret-read(buffer number)
    ))


;(let ((start 30)
;      (name-wid 10)
;      (opis-wid 40)
;      (dim-wid  24)
;      (num-rec  387)
;      )
;  (list (recode-string bbb (+ start
;			      (* num-rec
;				 (+ name-wid opis-wid dim-wid))) name-wid)
;	(recode-string bbb (+ start name-wid
;			      (* num-rec (+ name-wid opis-wid dim-wid))) opis-wid)
;	(recode-string bbb (+ start name-wid opis-wid
;			      (* num-rec (+ name-wid opis-wid dim-wid))) dim-wid)))

;(defparameter bbb (read-trend-header #p"D:/home/_namatv/git/11.trd" (+ 64676 30)))
; (read-trend-hdr #p"D:/home/_namatv/git/11.trd")



;(defparameter bbb(read-trd-file in 5))

;(read-byte in)
					;(recode-string bbb)

(defun read-trend-hdr (in &optional (byte-number 30))
  (let ((bufer nil)   ;; Буфер для чтения данных
	(analog nil)  ;; Аналоговые сигналы
	(discret nil) ;; Дискретные сигналы
	
	(head-wid 30)
	(head-id-wid 5)	     ;; Строка идентификации
	(head-version-wid 1) ;; Версия данных тренда
	(head-date-wid 3)    ;; День Месяц Год-2000
	(head-time-wid 3)    ;; Час Минута Секунда

	(id nil)	  ;; Строка идентификации
	(version nil)	  ;; Версия данных трендера
	(date-day nil)	  ;; День
	(date-month nil)  ;; Месяц 
	(date-year nil)	  ;; Год-2000;
	(time-hour nil)	  ;; Час
	(time-minute nil) ;; Минута
	(time-second nil) ;; Секунда

	(name-wid 10)
	(opis-wid 40)
	(dim-wid  24)
	(num-rec-analog 0)
	(num-rec-diskret 0)
	)
    (setf
     id (recode-string (read-trd-file in head-id-wid))
     version  (car(read-trd-file in head-version-wid))
     bufer (read-trd-file in head-date-wid)
     date-day (first bufer)
     date-month (second bufer)
     date-year (+ 2000 (third bufer))
     bufer (read-trd-file in head-time-wid)
     time-hour (first bufer)
     time-minute (second bufer)
     time-second (third bufer)
     )
    (format t "~S ~A ~S-~S-~S_~S:~S:~S "
	    id version date-year date-month date-day time-hour time-minute time-second)))

;;    (dotimes (i head-wid) (push (read-byte in) head))
;;    (setf head (reverse head))
;;    (setf num-rec-analog (+ (nth 26 head) (* 256 (nth 27 head)))
;;	  num-rec-diskret (+ (nth 28 head) (* 256 (nth 29 head))))
;;    (dotimes (i (* num-rec-analog (+ name-wid opis-wid dim-wid))) (push (read-byte in) analog))
;;    (setf analog (reverse analog))
;;    (dotimes (i (* num-rec-diskret (+ name-wid opis-wid))) (push (read-byte in) discret))
;;    (setf  discret (reverse discret))
;;    (close in)

(defun read-trd-file-short(in)
  "Выполняет чтение short из потока in"
  (let ((rez 0))
    (dotimes (i 2 rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (read-byte in)))))

(defun read-trd-file-int(in)
  "Выполняет чтение int из потока in"
  (let ((rez 0))
    (dotimes (i 4 rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (read-byte in)))))

(defun read-trd-file-long(in)
  "Выполняет чтение long из потока in"
  (let ((rez 0))
    (dotimes (i 4 rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (read-byte in)))))

(defun read-trd-file-long-long(in)
  "Выполняет чтение long-long из потока in"
  (let ((rez 0))
    (dotimes (i 8 rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (read-byte in)))))

;;;;(open-trd-file #p"/home/namatv/MyDoc/git/clisp/recoder/230415_191202.trd")

(progn
  (defparameter in (open-trd-file #p"/home/namatv/1.txt"))
  (defparameter b_1 (read-trd-file-short     in))
  (defparameter b_2 (read-trd-file-int       in))
  (defparameter b_3 (read-trd-file-long-long in))
  (close in))
