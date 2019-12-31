;;;; binary-read.lisp

(in-package #:recoder)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"@b(Описание:) list-to-int выполняет преобразование списка целых чисел 
находящихся в диапазоне 0 - 255 в целое число"
(defun list-to-int (list-of-int)
  (do ((i 0 (+ i 8))
       (lst list-of-int (cdr lst))
       (rez 0))
      ((null lst) rez)
  (setf (ldb (byte 8 i) rez) (car lst))))

@export
@annot.doc:doc
"@b(Описание:) open-trd-file-read выполняет открытие файла тренда."
(defun open-trd-file-read (path)
  (open path :element-type 'unsigned-byte))

@export
@annot.doc:doc 
"Выполняет чтение bite-number из потока in"
(defun read-trd-file (in byte-number)
  (let ((lst nil)
	(bt nil))
    (dotimes (i byte-number)
      (setf bt (read-byte in nil 'eof))
      (if (eq bt 'eof)
	  (return-from read-trd-file  (values (nreverse lst) i nil))
	  (push bt lst)))
    (values (nreverse lst) byte-number t)))

@export
@annot.doc:doc 
"Выполняет чтение short из потока in"
(defun read-trd-file-short(in &optional (len 2))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

@export
@annot.doc:doc 
"Выполняет чтение int из потока in"
(defun read-trd-file-int(in &optional (len 4))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

@export
@annot.doc:doc 
"Выполняет чтение long из потока in"
(defun read-trd-file-long(in &optional (len 4))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

@export
@annot.doc:doc 
"Выполняет чтение long-long из потока in"
(defun read-trd-file-long-long(in &optional (len 8))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (values   (list-to-int rez) n file-stastus)))

@export
@annot.doc:doc 
"Выполняет чтение float из потока in"
(defun read-trd-file-float(in &optional (len 4))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
;;;;	(values (ie3fp:decode-ieee-float (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float32 (list-to-int rez)) n file-stastus)	
	(values 0 n file-stastus))))

@export
@annot.doc:doc 
"Выполняет чтение doudle из потока in"
(defun read-trd-file-double(in &optional (len 8))
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
;;;;	(values (ie3fp:decode-ieee-double (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float64 (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

;(defun read-trd-file-quad (in &optional (len 16))
;  "Выполняет чтение quad из потока in"
;        (multiple-value-bind (rez n file-stastus)
;      (read-trd-file in len)
;    (if file-stastus
;	(values (ie3fp:decode-ieee-quad (list-to-int rez)) n file-stastus)
;	(values 0 n file-stastus))))
