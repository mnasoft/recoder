;;;; ./src/binary/test.lisp

(in-package :recoder/binary)

(defmacro ints-test (descr func)
  `(cons ,descr
         (loop :for i :from 1 :to 5 :collect (,func in))))

(with-open-file-b-in
    (in "/home/mna/quicklisp/local-projects/clisp/recoder/src/binary/cpp/output.bin"
     )
  (format t "~{~{~A~^ ~}~%~}"
          (list
           (ints-test "char:" b-read-char)
           (ints-test "unsigned char:" b-read-uchar)
           (ints-test "short:" b-read-short )
           (ints-test "unsigned short:" b-read-ushort )
           (ints-test "int:" b-read-int )
           (ints-test "unsigned int:" b-read-uint )
           (ints-test "long:" b-read-long )
           (ints-test "unsigned long:" b-read-ulong))))

(with-open-file-b-in
    (in "/home/mna/quicklisp/local-projects/clisp/recoder/src/binary/cpp/output.bin"
     )
  (format t "~{~{~S~^ ~}~%~}"
          (list
           (ints-test "float:"  b-read-float)
           (ints-test "double:" b-read-double)
           ))
           (ints-test "double:" b-read-long-double)
  )

(defun read-long-double (stream &optional (size 16)) ; предполагаем 128-битное значение
  "Читает long double из бинарного потока."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    (let ((value 0.0))
      (dotimes (i size)
        (setf value (+ value
                       (* (coerce (aref bytes i) 'double-float)
                          (expt 256 (- i size))))))
      value)))

1.1111111111111111111111111111111111111111110l0 ; => 1.1111111111111111111l0 (111.11111111111111111l0%)
1.1111111111111111111111111111111111111111110d0 ; => 1.1111111111111112d0 (111.11111111111111d0%)
1.1111111111111111111111111111111111111111110e0 ; => 1.1111112 (111.111115%)


(/ (- 140 (* 5 4) (* 5 8)) 5)

           (ints-test "unsigned char:" b-read-uchar)
           (ints-test "short:" b-read-short )
           (ints-test "unsigned short:" b-read-ushort )
           (ints-test "int:" b-read-int )
           (ints-test "unsigned int:" b-read-uint )
           (ints-test "long:" b-read-long )
           (ints-test "unsigned long:" b-read-ulong))))



float: 2.51134e+38 2.94564e+38 4.13818e+37 3.30557e+38 1.4678e+38 
double: 7.21004e+307 1.01332e+308 4.90814e+307 9.64053e+307 1.0482e+308 
long double: 5.29738e+4931 1.05404e+4932 1.99046e+4931 3.57799e+4931 4.52533e+4930 


(with-open-file-b-out
    (out
     "/home/mna/quicklisp/local-projects/clisp/recoder/src/binary/cpp/output-1.bin")
  (loop :for i :from 0 :to 9
        :collect
    (b-write-uchar (- 255 i) out)
))


