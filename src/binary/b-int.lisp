(in-package :recoder/binary)

(defun list-to-int (list-of-int)
  "@b(Описание:) функция @b(list-to-int) выполняет преобразование
списка целых чисел находящихся в диапазоне 0 - 255 в целое число.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-to-int '(2 8 0 0)) => 2050
 (list-to-int '(1 0)) => 1
 (list-to-int '(0 1)) => 256
@end(code)"
  (do ((i 0 (+ i 8))
       (lst list-of-int (cdr lst))
       (rez 0))
      ((null lst) rez)
    (setf (ldb (byte 8 i) rez) (car lst))))

(defun int-to-list (int-val len)
  "Выполняет преобразование целого числа в список целых 
чисел, находящихся в диапазоне от 0 до 255.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (int-to-list 2050 4) => (2 8 0 0)
@end(code)
"
  (let ((bbb nil))
    (dotimes (i len (reverse bbb))
      (push (ldb (byte 8  (* 8 i)) int-val) bbb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun b-read (in byte-number)
  "@b(Описание:) функция @b(b-read) выполняет чтение @b(byte-number)
 количества байт из бинарного потока @b(in)."
  (let ((lst nil)
	(bt nil))
    (dotimes (i byte-number)
      (setf bt (read-byte in nil 'eof))
      (if (eq bt 'eof)
	  (return-from b-read  (values (nreverse lst) i nil))
	  (push bt lst)))
    (values (nreverse lst) byte-number t)))

(defun b-write (byte-list out &aux (byte-number (length  byte-list)))
  "@b(Описание:) функция @b(b-write) выполняет запись @b(bite-number)
 элементов списка byte-list в бинарный поток вывода @b(out)."
  (dotimes (i byte-number)
    (write-byte (pop byte-list ) out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun b-read-integer (in size &key (signed nil) (endian :little))
  "Читает целое число из бинарного потока IN.
SIZE — количество байт (1, 2, 4, 8).
SIGNED — если T, интерпретирует как знаковое число.
ENDIAN — :little или :big."
  (multiple-value-bind (bytes n status)
      (b-read in size)
    (if (not status)
        (values 0 n nil)
        (let* ((ordered-bytes (if (eq endian :little) bytes (reverse bytes)))
               (unsigned (list-to-int ordered-bytes)))
          (if (and signed (>= unsigned (ash 1 (- (* size 8) 1))))
              ;; интерпретируем как отрицательное (two's complement)
              (values (- unsigned (ash 1 (* size 8))) n t)
              (values unsigned n t))))))

(defun b-write-integer (int-val out size &key (signed nil) (endian :little))
  "Записывает целое число INT-VAL в бинарный поток OUT.
SIZE — количество байт (1, 2, 4, 8).
SIGNED — если T, интерпретирует как знаковое число.
ENDIAN — :little или :big."
  (let* ((max-val (ash 1 (* size 8)))
         (val (if signed
                  (mod int-val max-val) ; two's complement
                  int-val))
         (bytes (int-to-list val size))
         (ordered-bytes (if (eq endian :little) bytes (reverse bytes))))
    (b-write ordered-bytes out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-binary-integer-io (read-fn-name write-fn-name size
                                    &key signed (endian :little))
  "Генерирует функции для бинарного чтения и записи целого числа."
  `(progn
     (defun ,read-fn-name (in)
       (multiple-value-bind (bytes n status)
           (b-read in ,size)
         (if (not status)
             (values 0 n nil)
             (let* ((ordered-bytes (if (eq ,endian :little) bytes (reverse bytes)))
                    (unsigned (list-to-int ordered-bytes)))
               (if (and ,signed (>= unsigned (ash 1 (- (* ,size 8) 1))))
                   (values (- unsigned (ash 1 (* ,size 8))) n t)
                   (values unsigned n t))))))

     (defun ,write-fn-name (val out)
       (let* ((max-val (ash 1 (* ,size 8)))
              (val (if ,signed (mod val max-val) val))
              (bytes (int-to-list val ,size))
              (ordered-bytes (if (eq ,endian :little) bytes (reverse bytes))))
         (b-write ordered-bytes out)))))

(progn
  (define-binary-integer-io b-read-char   b-write-char    1 :signed t)
  (define-binary-integer-io b-read-uchar  b-write-uchar   1)
  (define-binary-integer-io b-read-short  b-write-short   2 :signed t)
  (define-binary-integer-io b-read-ushort b-write-ushort  2)
  (define-binary-integer-io b-read-int    b-write-int     4 :signed t)
  (define-binary-integer-io b-read-uint   b-write-uint    4)
  (define-binary-integer-io b-read-long   b-write-long    8 :signed t)
  (define-binary-integer-io b-read-ulong  b-write-ulong   8)
  (define-binary-integer-io b-read-long-long  b-write-long-long  16 :signed t)
  (define-binary-integer-io b-read-ulong-long b-write-ulong-long 16)
  )
