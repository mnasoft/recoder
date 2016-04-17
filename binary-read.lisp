;;;; binary-read.lisp

(in-package #:recoder)

(defun list-to-int(list-of-int)
  "Выполняет преобразование списка целых чисел 
находящихся в диапазоне 0 - 255 в целое число"
  (do ((i 0 (+ i 8))
       (lst list-of-int (cdr lst))
       (rez 0))
      ((null lst) rez)
  (setf (ldb (byte 8 i) rez) (car lst))))

(defun open-trd-file-read(path)
  "Выполняет открытие файла тренда"
  (open path :element-type 'unsigned-byte))

(defun read-trd-file(in byte-number)
  "Выполняет чтение bite-number из потока in"
  (let ((lst nil)
	(bt nil))
    (dotimes (i byte-number)
      (setf bt (read-byte in nil 'eof))
      (if (eq bt 'eof)
	  (return-from read-trd-file  (values (nreverse lst) i nil))
	  (push bt lst)))
    (values (nreverse lst) byte-number t)))

(defun read-trd-file-short(in &optional (len 2))
  "Выполняет чтение short из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-int(in &optional (len 4))
  "Выполняет чтение int из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-long(in &optional (len 4))
  "Выполняет чтение long из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-long-long(in &optional (len 8))
  "Выполняет чтение long-long из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (values   (list-to-int rez) n file-stastus)))

(defun read-trd-file-float(in &optional (len 4))
  "Выполняет чтение float из потока in"
    (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (ie3fp:decode-ieee-float (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-double(in &optional (len 8))
  "Выполняет чтение doudle из потока in"
      (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (ie3fp:decode-ieee-double (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
        (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (ie3fp:decode-ieee-quad (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))
