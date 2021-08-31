;;;; ./src/tests/suites/trd.lisp

(in-package :recoder/tests)

(progn 
  (defparameter *binary.bin*
    (concatenate 'string
                 (namestring (asdf:system-source-directory :recoder))
                 "trd" "/" "binary.bin")))

(def-suite binary
  :description "Мастер-набор всех тестов проекта recoder/binary."
  :in all)

(in-suite binary)

(def-test write-read-short ()
  "Проверка записи и чтения целых чисел типа short."
  (let* ((up (expt 2 (* 2 8)))
         (shorts (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-short el fl-w))
       shorts)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal shorts 
                      (loop :for i :in shorts
                            :collect
                            (recoder/binary:read-trd-file-short fl-r))))
      (close fl-r))))

(def-test write-read-int ()
  "Проверка записи и чтения целых чисел типа int."
  (let* ((up (expt 2 (* 4 8)))
         (ints (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-int el fl-w))
       ints)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal ints 
                      (loop :for i :in ints
                            :collect
                            (recoder/binary:read-trd-file-int fl-r))))
      (close fl-r))))

(def-test write-read-long ()
  "Проверка записи и чтения целых чисел типа long."
  (let* ((up (expt 2 (* 4 8)))
         (longs (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-long el fl-w))
       longs)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal longs 
                      (loop :for i :in longs
                            :collect
                            (recoder/binary:read-trd-file-long fl-r))))
      (close fl-r))))

(def-test write-read-long-long ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (long-longs (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-long-long el fl-w))
       long-longs)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal long-longs 
                      (loop :for i :in long-longs
                            :collect
                            (recoder/binary:read-trd-file-long-long fl-r))))
      (close fl-r))))

(def-test write-read-float ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (floats (loop :for i :from 0 :to 100 :collect (* 1.0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-float el fl-w))
       floats)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal floats 
                      (loop :for i :in floats
                            :collect
                            (recoder/binary:read-trd-file-float fl-r))))
      (close fl-r))))

(def-test write-read-double ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (doubles (loop :for i :from 0 :to 100 :collect (* 1.0d0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (recoder/binary:open-trd-file-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:write-trd-file-double el fl-w))
       doubles)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-trd-file-read *binary.bin*)))
      (is-true (equal doubles 
                      (loop :for i :in doubles
                            :collect
                            (recoder/binary:read-trd-file-double fl-r))))
      (close fl-r))))
