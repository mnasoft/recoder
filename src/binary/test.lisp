;;;; ./src/binary/test.lisp

(in-package :recoder/binary)

(with-open-file-b-in
    (in "/home/mna/quicklisp/local-projects/clisp/recoder/src/binary/cpp/signed_chars.bin"
     )
  (loop :for i :from 0 :to 255 :collect
    (b-read-uchar in)
        ))

(with-open-file-b-out
    (out
     "/home/mna/quicklisp/local-projects/clisp/recoder/src/binary/cpp/output-1.bin")
  (loop :for i :from 0 :to 9
        :collect
    (b-write-uchar (- 255 i) out)
))


