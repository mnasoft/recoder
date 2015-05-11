;;;; binary-write.lisp

(in-package #:recoder)

(defun open-trd-file-write(path)
  "Выполняет открытие файла тренда"
  (open path :element-type 'unsigned-byte :direction :output))

(defun write-trd-file(byte-list out &optional (byte-number (length  byte-list)))
  "Выполняет чтение bite-number из потока in"
  (dotimes (i byte-number)
    (write-byte (pop byte-list ) out)))

(defun write-trd-file-short(in &optional (len 2))
  "Выполняет чтение short из потока in"
  (list-to-int(write-trd-file in len)))

(defun write-trd-file-int(in &optional (len 4))
  "Выполняет чтение int из потока in"
  (list-to-int(write-trd-file in len)))

(defun write-trd-file-long(in &optional (len 4))
  "Выполняет чтение long из потока in"
  (list-to-int(write-trd-file in len)))

(defun write-trd-file-long-long(in &optional (len 8))
  "Выполняет чтение long-long из потока in"
  (list-to-int(write-trd-file in len)))

(defun write-trd-file-float(in &optional (len 4))
  "Выполняет чтение float из потока in"
    (ie3fp:decode-ieee-float(list-to-int(write-trd-file in len))))

(defun write-trd-file-double(in &optional (len 8))
  "Выполняет чтение doudle из потока in"
  (ie3fp:decode-ieee-double (list-to-int(write-trd-file in len))))

(defun write-trd-file-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
  (ie3fp:decode-ieee-quad (list-to-int(write-trd-file in len))))
