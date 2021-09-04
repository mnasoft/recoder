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
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-short el fl-w))
       shorts)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal shorts 
                      (loop :for i :in shorts
                            :collect
                            (recoder/binary:b-read-short fl-r))))
      (close fl-r))))

(def-test write-read-int ()
  "Проверка записи и чтения целых чисел типа int."
  (let* ((up (expt 2 (* 4 8)))
         (ints (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-int el fl-w))
       ints)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal ints 
                      (loop :for i :in ints
                            :collect
                            (recoder/binary:b-read-int fl-r))))
      (close fl-r))))

(def-test write-read-long ()
  "Проверка записи и чтения целых чисел типа long."
  (let* ((up (expt 2 (* 4 8)))
         (longs (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-long el fl-w))
       longs)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal longs 
                      (loop :for i :in longs
                            :collect
                            (recoder/binary:b-read-long fl-r))))
      (close fl-r))))

(def-test write-read-long-long ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (long-longs (loop :for i :from 0 :to 100 :collect (random up))))
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-long-long el fl-w))
       long-longs)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal long-longs 
                      (loop :for i :in long-longs
                            :collect
                            (recoder/binary:b-read-long-long fl-r))))
      (close fl-r))))

(def-test write-read-float ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (floats (loop :for i :from 0 :to 100 :collect (* 1.0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-float el fl-w))
       floats)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal floats 
                      (loop :for i :in floats
                            :collect
                            (recoder/binary:b-read-float fl-r))))
      (close fl-r))))

(def-test write-read-double ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (doubles (loop :for i :from 0 :to 100 :collect (* 1.0d0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (recoder/binary:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (recoder/binary:b-write-double el fl-w))
       doubles)
      (close fl-w))
    (let ((fl-r (recoder/binary:open-b-read *binary.bin*)))
      (is-true (equal doubles 
                      (loop :for i :in doubles
                            :collect
                            (recoder/binary:b-read-double fl-r))))
      (close fl-r))))
