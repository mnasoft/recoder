;;;; ./src/tests/suites/trd.lisp

(in-package :recoder/tests)

(progn 
  (defparameter *trd-fname*
    (concatenate 'string
                 (namestring (asdf:system-source-directory :recoder))
                 "trd"
                 "/"
                 "2018-11-06_092329.trd"))

  (defparameter *trd* (make-instance 'recoder/trd:<trd> :file-name *trd-fname*))

  (recoder/trd:trd-open *trd*))

(def-suite trd
  :description "Мастер-набор всех тестов проекта trd."
  :in all)

(in-suite trd)

(def-fixture fix-open-trd ()
  (let ((trd (recoder/trd:trd-open
              (make-instance 'recoder/trd:<trd> :file-name *trd-fname*))))
      (&body)))

(def-test trd-open-test ()
  "Проверка открытия и закрытия треда."
  (with-fixture fix-open-trd ()
    (is-true trd)
    (is-true (recoder/trd:<trd>-file-descr trd))
    (is-false (progn (recoder/trd:trd-close trd)
                     (recoder/trd:<trd>-file-descr trd)))
    (is-true (progn (recoder/trd:trd-open trd)
                     (recoder/trd:<trd>-file-descr trd)))))

(def-test trd-header-test ()
  "Проверка заголовка треда."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder/trd:<trd>-file-name trd)))
    (is-true (string= (recoder/trd:<trd>-id-string trd) "TREND" ))
    (is-true (= (recoder/trd:<trd>-version trd) 2 ))
    (is-true (= (recoder/trd:<trd>-utime-start trd) 3750477809))
    (is-true (= (recoder/trd:<trd>-reserv trd) 415))
    (is-true (= (recoder/trd:<trd>-analog-number trd) 314))
    (is-true (= (recoder/trd:<trd>-discret-number trd) 101))
    (is-true (= (recoder/trd:<trd>-total-records trd) 15706))
    (is-true (= (recoder/trd:<trd>-delta-time trd) 0.25d0))
    (is-true (= (hash-table-count (recoder/trd:<trd>-analog-ht trd)) 314))
    (is-true (= (hash-table-count (recoder/trd:<trd>-discret-ht trd)) 101))))

(def-test trd-analog-test ()
  "Проверка извлечения аналоговых сигналов."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder/trd:<trd>-file-name trd)))
    (is-true (equal (recoder/trd:trd-analog-by-utime
                     trd
                     (+ 600 (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-analog-signal-list
                      trd '("V2" "P02" "T2" "ET300")))
                    '(0.1274128328374151d0 106446.93675135424d0 15.457389181353474d0 424.24963759823d0)))
    (is-true (equal (recoder/trd:trd-analog-by-utime
                     trd
                     (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-analog-signal-list trd '("V2" "P02" "T2" "ET300")))
                    '(0.49477378500038144d0 156145.5710688945d0 34.91111619745175d0 4.736400396734569d0)))))

(def-test trd-discret-test ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder/trd:<trd>-file-name trd)))
    (is-true (equal (recoder/trd:trd-discret-by-utime 
                     trd
                     (+ 600 (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-discret-signal-list
                      trd '("GAS" "OIL")))
                    '(0 1)))
    (is-true (equal (recoder/trd:trd-discret-by-utime 
                     trd
                     (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-discret-signal-list
                      trd '("GAS" "OIL")))
                    '(0 1)))))

