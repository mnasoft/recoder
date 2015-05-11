;;;; binary-read.lisp

(in-package #:recoder)

(defun list-to-int(list-of-int &optional (len (length list-of-int )))
  "Выполняет преобразование списка целых чисел 
находящихся в диапазоне 0 - 255 в целое число"
  (let ((rez 0))
    (dotimes (i len rez)
      (setf (ldb (byte 8  (* 8 i)) rez) (nth i list-of-int)))))

(defun open-trd-file-read(path)
  "Выполняет открытие файла тренда"
  (open path :element-type 'unsigned-byte))

(defun read-trd-file(in byte-number)
  "Выполняет чтение bite-number из потока in"
  (let ((lst nil))
    (dotimes (i byte-number)
      (push (read-byte in) lst))
    (reverse lst)))

(defun read-trd-file-short(in &optional (len 2))
  "Выполняет чтение short из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-int(in &optional (len 4))
  "Выполняет чтение int из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-long(in &optional (len 4))
  "Выполняет чтение long из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-long-long(in &optional (len 8))
  "Выполняет чтение long-long из потока in"
  (list-to-int(read-trd-file in len)))

(defun read-trd-file-float(in &optional (len 4))
  "Выполняет чтение float из потока in"
    (ie3fp:decode-ieee-float(list-to-int(read-trd-file in len))))

(defun read-trd-file-double(in &optional (len 8))
  "Выполняет чтение doudle из потока in"
  (ie3fp:decode-ieee-double (list-to-int(read-trd-file in len))))

(defun read-trd-file-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
  (ie3fp:decode-ieee-quad (list-to-int(read-trd-file in len))))
