;;;; tests/matrix.lisp

(in-package :recoder/tests)

(progn 
(defparameter *trd-fname*
  (concatenate 'string
             (namestring (asdf:system-source-directory :recoder))
             "trd"
             "/"
             "2018-11-06_092329.trd"))

(defparameter *trd* (make-instance 'recoder:<trd> :trd-file-name *trd-fname*))

(recoder:trd-open *trd*)
)

(def-suite trd-tests
  :description "Мастер-набор всех тестов проекта trd."
  :in all-tests)

(in-suite trd-tests)

(def-fixture fix-open-trd ()
  (let ((trd (recoder:trd-open
              (make-instance 'recoder:<trd> :trd-file-name *trd-fname*))))
      (&body)))

(def-test trd-open-test ()
  "Проверка открытия и закрытия треда."
  (with-fixture fix-open-trd ()
    (is-true trd)
    (is-true (recoder:trd-file-descr trd))
    (is-false (progn (recoder:trd-close trd)
                     (recoder:trd-file-descr trd)))
    (is-true (progn (recoder:trd-open trd)
                     (recoder:trd-file-descr trd)))))

(def-test trd-header-test ()
  "Проверка заголовка треда."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder:trd-file-name trd)))
    (is-true (string= (recoder:trd-id-string trd) "TREND" ))
    (is-true (= (recoder:trd-version trd) 2 ))
    (is-true (= (recoder:trd-utime-start trd) 3750477809))
    (is-true (= (recoder:trd-reserv trd) 415))
    (is-true (= (recoder:trd-analog-number trd) 314))
    (is-true (= (recoder:trd-discret-number trd) 101))
    (is-true (= (recoder:trd-total-records trd) 15706))
    (is-true (= (recoder:trd-delta-time trd) 0.25d0))
    (is-true (= (hash-table-count (recoder:trd-analog-ht trd)) 314))
    (is-true (= (hash-table-count (recoder:trd-discret-ht trd)) 101))))

(def-test trd-header-test ()
  "Проверка извлечения аналоговых сигналов."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder:trd-file-name trd)))))

(recoder:trd-analog-by-utime
 *trd*
 (+ (recoder:trd-utime-start *trd*)
    600)
    (recoder:trd-analog-signal-list *trd* '("V2" "P02" "T2" "ET300")))

(* 0.25 1500)
