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

(def-test trd-analog-length-byte ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-analog-length-byte trd) 628))))

(def-test trd-discret-length-byte ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-discret-length-byte trd) 13))))

(def-test trd-discret-offset ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-discret-offset trd) 628))))

(def-test trd-start-offset ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-start-offset trd) 28316))))

(def-test trd-record-length ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-record-length trd) 641))))

(def-test trd-utime-end ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-utime-end trd) 3750481735))))

(def-test trd-record->utime()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-record->utime trd 1000) 3750478059))))

(def-test trd-utime->record()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:trd-utime->record trd 3750478059) 1000))))

(def-test time-universal-encode()
    (is-true (= (recoder/trd:time-universal-encode 2021 09 16 12 27 25)  3840773245)))
