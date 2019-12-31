;;;; binary-write.lisp

(in-package #:recoder)

(annot:enable-annot-syntax)

(defun int-to-list(int-val len)
  "Выполняет преобразование целого числа в список целых 
чисел, находящихся в диапазоне от 0 до 255"
  (let ((bbb nil))
    (dotimes (i len (reverse bbb))
      (push (ldb (byte 8  (* 8 i)) int-val) bbb))))

@export
@annot.doc:doc "Выполняет открытие файла тренда"
(defun open-trd-file-write (path)
  (open path :element-type 'unsigned-byte :direction :io :if-exists :overwrite))

@export
@annot.doc:doc 
"Выполняет запись  bite-number элементов списка byte-list в поток out"
(defun write-trd-file(byte-list out &optional (byte-number (length  byte-list)))
  (dotimes (i byte-number)
    (write-byte (pop byte-list ) out)))

@export
@annot.doc:doc 
"Выполняет запись short в поток out"
(defun write-trd-file-short(int-val out &optional (len 2))
  (write-trd-file (int-to-list int-val len) out len))

@export
@annot.doc:doc 
"Выполняет запись int в поток out"
(defun write-trd-file-int(int-val out &optional (len 4))
  (write-trd-file (int-to-list int-val len) out len))

@export
@annot.doc:doc 
"Выполняет запись long в поток out"
(defun write-trd-file-long (int-val out &optional (len 4))
  (write-trd-file (int-to-list int-val len) out len))

@export
@annot.doc:doc 
"Выполняет чтение long-long из потока in"
(defun write-trd-file-long-long (int-val out &optional (len 8))
    (write-trd-file (int-to-list int-val len) out len))

@export
@annot.doc:doc 
"Выполняет чтение float из потока in"
(defun write-trd-file-float(val out &optional (len 4))
  (write-trd-file
   (int-to-list
;;;;    (ie3fp:encode-ieee-float (coerce val 'float))
    (ieee-floats:encode-float32 (coerce val 'float))    
    len)
   out
   len))

@export
@annot.doc:doc 
"Выполняет чтение doudle из потока in"
(defun write-trd-file-double(val out &optional (len 8))
  (write-trd-file
   (int-to-list
;;;;(ie3fp:encode-ieee-double (coerce val 'double-float))
    (ieee-floats:encode-float64 (coerce val 'double-float))
    len)
   out
   len))

;(defun write-trd-file-quad (val out &optional (len 16))
;  "Выполняет чтение quad из потока in"
;  (write-trd-file
;   (int-to-list
;    (ie3fp:encode-ieee-quad
;     (coerce val 'long-float))
;    len)
;   out
;   len))
