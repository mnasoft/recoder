;;; ./src/trd-seq-defmethods.lisp

(in-package #:recoder)

(export '(<trd-seq> <trd-seq>-a-sig <trd-seq>-d-sig update <trd-seq>-signal-strings))

(defclass <trd-seq> (<trd> sequence)
  ((signal-strings :accessor <trd-seq>-signal-strings :initform nil :initarg :signal-strings)
   (a-sig :accessor <trd-seq>-a-sig :initform nil :documentation "Список аналоговых сигналов.")
   (d-sig :accessor <trd-seq>-d-sig :initform nil :documentation "Список дискретных сигналов.")))

(defmethod update ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(update) 
"
  (let ((sig (trd-separate-signals trd-seq (<trd-seq>-signal-strings trd-seq))))
    (setf (<trd-seq>-a-sig trd-seq) (first  sig))
    (setf (<trd-seq>-d-sig trd-seq) (second sig))
    trd-seq))

(defmethod sequence:length ((trd-seq <trd-seq>))
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (trd-total-records trd-seq))

(defmethod sequence:elt ((trd-seq <trd-seq>) index)
  (unless (trd-file-descr trd-seq) (trd-open trd-seq))
  (let ((a-sig (<trd-seq>-a-sig trd-seq))
        (d-sig (<trd-seq>-d-sig trd-seq)))
    (append (when a-sig (trd-analog-by-rec-number  trd-seq index a-sig))
            (when d-sig (trd-discret-by-rec-number trd-seq index d-sig)))))

(defmethod trd-open :after ((trd-seq <trd-seq>))
  "@b(Описание:) метод @b(trd-open :after)
"
  (update trd-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
