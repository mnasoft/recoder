;;;; tests/package.lisp

(defpackage :recoder/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :recoder/tests)

(defun run-tests ()
  (run! 'txt)
  #+nil  (run! 'all)
  )

(def-suite all
  :description "Мастер-набор всех тестов проекта Recoder.")

(in-suite all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fixture fix-sig-names ()
  (let ((names '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO")))
    (&body)))

(def-fixture a-names ()
  (let ((a-names '("V2" "P02" "T2" "ET300")))
    (&body)))

(def-fixture d-names ()
  (let ((d-names '("FA530" "FK526" "FA526" "FA566")))
      (&body)))

;;;;(run-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun semi-equal (lst-1 lst-2)
  (recoder/split::apply-and
   (mapcar
    #'(lambda (x y)
        (math/core:semi-equal x y))
    lst-1
    lst-2)))

(defun semi-equalp (lst-1 lst-2)
  (recoder/split::apply-and
   (mapcar
    #'(lambda (x y)
        (math/core:semi-equal
         x y :tolerance
         (+ 1.0d-3 (* 1.0d-3
                      (math/core:norma
                       (list (math/core:norma x) (math/core:norma y)))))))
    lst-1
    lst-2)))



(def-fixture fix-open-trd (path-to-file)
  (let ((trd (r/trd:trd-open
              (make-instance 'r/c:<trd> :file-name path-to-file))))
    (&body)))

(progn 
  (defparameter *trd-fname* (mnas-path:asdf-path :recoder "data/trd/2018-11-06_092329.trd"))
  (defparameter *trd* (make-instance 'r/c:<trd> :file-name *trd-fname*))
  (recoder/trd:trd-open *trd*)
  )

(defun excel-column-to-number (col)
  "Преобразует строку столбца Excel (например, 'AF') в номер (например, 32)."
  (loop for char across (string-upcase col)
        for pow = (1- (length col)) then (1- pow)
        summing (* (- (char-code char) 64) (expt 26 pow))))

(defun count-columns-between (start-col end-col)
  "Возвращает количество столбцов между двумя именами, включая оба."
  (let ((start (excel-column-to-number start-col))
        (end (excel-column-to-number end-col)))
    (1+ (- end start))))
