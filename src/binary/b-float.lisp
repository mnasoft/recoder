;;;; ./src/binary/b-float.lsp

(in-package :recoder/binary)

(ieee-floats:make-float-converters encode-float32 decode-float32 8 23 nil)
(ieee-floats:make-float-converters encode-float64 decode-float64 11 52 nil)
(ieee-floats:make-float-converters encode-float128 decode-float128 15 112 nil)
;;(ieee-floats:make-float-converters encode-float80 decode-float80 15 64 nil)


(defun b-read-float (in &aux (len 4))
  "@b(Описание:) функция @b(b-read-float) выполняет чтение
 короткого (4 байта) числа с плавающей точкой из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (decode-float32 (list-to-int rez)) n file-stastus)	
	(values 0 n file-stastus))))

(defun b-read-double (in &aux (len 8))
  "@b(Описание:) функция @b(b-read-double) выполняет чтение
 длинного (8 байт) числа с плавающей точкой из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (decode-float64 (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

(defun b-read-quad (in &aux (len 16))
  "@b(Описание:) функция @b(b-read-double) выполняет чтение
 длинного (8 байт) числа с плавающей точкой из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (decode-float128 (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

(defun b-write-float (val out &aux (len 4))
  "@b(Описание:) функция @b(b-write-float) выполняет запись
короткого (4 байта) числа с плавающей точкой в бинарный поток @b(out)."
  (b-write (int-to-list (encode-float32 (coerce val 'float)) len) out))

(defun b-write-double (val out &aux (len 8))
  "@b(Описание:) функция @b(b-write-float) выполняет запись
длиного (8 байт) числа с плавающей точкой в бинарный поток @b(out)."
  (b-write (int-to-list (encode-float64 (coerce val 'double-float)) len) out))

(defun b-write-quad (val out &aux (len 16))
  "@b(Описание:) функция @b(b-write-float) выполняет запись
длиного (8 байт) числа с плавающей точкой в бинарный поток @b(out)."
  (b-write (int-to-list (encode-float128 (coerce val 'long-float)) len) out))

