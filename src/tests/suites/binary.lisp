;;;; ./src/tests/suites/trd.lisp

(in-package :recoder/tests)

(defun random-in-range (from to)
  "Генерирует случайное целое число от FROM до TO включительно."
  (+ from (random (1+ (- to from)))))

(defun min-range (bytes)
  (* -1 (expt 2 (1- (* 8 bytes)))))

(defun max-range (bytes)
  (1- (expt 2 (1- (* 8 bytes)))))



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
  (let* ((bytes 1)
         (min (min-range bytes))
         (max (max-range bytes))
         (nums (loop :for i :from 0 :to 100 :collect (random-in-range min max))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-char el fl-w))
       nums)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal nums 
                      (loop :for i :in nums
                            :collect
                            (r/bin:b-read-char fl-r))))
      (close fl-r))))

(def-test write-read-short ()
  "Проверка записи и чтения целых чисел типа short."
  (let* ((bytes 2)
         (min (min-range bytes))
         (max (max-range bytes))
         (nums (loop :for i :from 0 :to 100 :collect (random-in-range min max))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-short el fl-w))
       nums)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal nums 
                      (loop :for i :in nums
                            :collect
                            (r/bin:b-read-short fl-r))))
      (close fl-r))))

(def-test write-read-int ()
  "Проверка записи и чтения целых чисел типа int."
  (let* ((bytes 4)
         (min (min-range bytes))
         (max (max-range bytes))
         (nums (loop :for i :from 0 :to 100 :collect (random-in-range min max))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-int el fl-w))
       nums)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal nums 
                      (loop :for i :in nums
                            :collect
                            (r/bin:b-read-int fl-r))))
      (close fl-r))))

(def-test write-read-long ()
  "Проверка записи и чтения целых чисел типа long."
  (let* ((bytes 8)
         (min (min-range bytes))
         (max (max-range bytes))
         (nums (loop :for i :from 0 :to 100 :collect (random-in-range min max))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-long el fl-w))
       nums)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal nums 
                      (loop :for i :in nums
                            :collect
                            (r/bin:b-read-long fl-r))))
      (close fl-r))))

(def-test write-read-long-long ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((bytes 8)
         (min (min-range bytes))
         (max (max-range bytes))
         (nums (loop :for i :from 0 :to 100 :collect (random-in-range min max))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-long-long el fl-w))
       nums)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal nums 
                      (loop :for i :in nums
                            :collect
                            (r/bin:b-read-long-long fl-r))))
      (close fl-r))))

(def-test write-read-float ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (floats (loop :for i :from 0 :to 100 :collect (* 1.0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-float el fl-w))
       floats)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal floats 
                      (loop :for i :in floats
                            :collect
                            (r/bin:b-read-float fl-r))))
      (close fl-r))))
 
(def-test write-read-double ()
  "Проверка записи и чтения целых чисел типа long-long."
  (let* ((up (expt 2 (* 8 8)))
         (doubles (loop :for i :from 0 :to 100 :collect (* 1.0d0 (/ (random up) (+ 10 (random up)))))))
    (let ((fl-w (r/bin:open-b-write *binary.bin*)))
      (mapcar
       #'(lambda (el)
           (r/bin:b-write-double el fl-w))
       doubles)
      (close fl-w))
    (let ((fl-r (r/bin:open-b-read *binary.bin*)))
      (is-true (equal doubles 
                      (loop :for i :in doubles
                            :collect
                            (r/bin:b-read-double fl-r))))
      (close fl-r))))

(def-test write-read-string ()
  (loop :for enc :in '(:KOI8-U #+nil :KOI8-R :KOI8-RU :CP1251 :UTF-16 :UTF-32) :do
    (loop :for str :in (list r/bin:*pangram-uk-1* r/bin:*pangram-uk-2*  r/bin:*pangram-uk-3*) :do
      (is-true (equal
                (progn
                  (let ((buffer-length 900)
                        (path *binary.bin*))
                    (let ((w (r/bin:open-b-write path)))
                      (r/bin:b-write-string str w buffer-length :external-format enc)
                      (close w))
                    (let* ((r (r/bin:open-b-read path))
                           (rez (r/bin:b-read-string r buffer-length :encoding enc)))
                      (close r)
                      rez)))
                str))))
  (loop :for enc :in '(#+nil :KOI8-U :KOI8-R :KOI8-RU :CP1251 :UTF-16 :UTF-32) :do
    (loop :for str :in (list r/bin:*pangram-ru-1* r/bin:*pangram-ru-2*  r/bin:*pangram-ru-3*) :do
      (is-true (equal
                (progn
                  (let ((buffer-length 900)
                        (path *binary.bin*))
                    (let ((w (r/bin:open-b-write path)))
                      (r/bin:b-write-string str w buffer-length :external-format enc)
                      (close w))
                    (let* ((r (r/bin:open-b-read path))
                           (rez (r/bin:b-read-string r buffer-length :encoding enc)))
                      (close r)
                      rez)))
                str)))))
 
