;;;; tests/package.lisp

(defpackage :recoder/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :recoder/tests)

(defun run-tests () (run! 'all))

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
   (mapcar #'math/core:semi-equal lst-1 lst-2)))


(def-fixture fix-open-trd (path-to-file)
  (let ((trd (r/trd:trd-open
              (make-instance 'r/c:<trd> :file-name path-to-file))))
    (&body)))

(progn 
  (defparameter *trd-fname* (mnas-path:asdf-path :recoder "data/trd/2018-11-06_092329.trd"))
  (defparameter *trd* (make-instance 'r/c:<trd> :file-name *trd-fname*))
  (recoder/trd:trd-open *trd*)
  )
