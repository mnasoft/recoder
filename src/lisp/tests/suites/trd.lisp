;;;; ./src/tests/suites/trd.lisp

(in-package :recoder/tests)

(progn 
  (defparameter *trd-fname* (mnas-path:asdf-path :recoder "trd/2018-11-06_092329.trd"))
  (defparameter *trd* (make-instance 'r/c:<trd> :file-name *trd-fname*))
  (recoder/trd:trd-open *trd*))

(recoder/get:trd-discret-by-record-t-nil
 *trd*
 15705
 (recoder/slist:d-signals *trd*
                          '("FA530" "FK526" "FA526" "FA566")))

(recoder/get:trd-discret-by-record
 *trd*
 15705
 (recoder/slist:d-signals *trd*
                          '("FA530" "FK526" "FA526" "FA566")))

(def-suite trd
  :description "Мастер-набор всех тестов проекта trd."
  :in all)

(in-suite trd)

(def-fixture fix-open-trd ()
  (let ((trd (r/trd:trd-open
              (make-instance 'r/c:<trd> :file-name *trd-fname*))))
    (&body)))
 
(def-test trd-header-test ()
  "Проверка заголовка треда."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (r/c:<trd>-file-name trd)))
    (is-true (string= (r/c:<trd>-id-string trd) "TREND" ))
    (is-true (= (r/c:<trd>-version trd) 2 ))
    (is-true (= (r/c:<trd>-utime-start trd) 3750477809))
    (is-true (= (r/c:<trd>-reserv trd) 415))
    (is-true (= (r/c:<trd>-a-number trd) 314))
    (is-true (= (r/c:<trd>-d-number trd) 101))
    (is-true (= (r/c:<trd>-records trd) 15706))
    (is-true (= (r/c:<trd>-increment trd) 0.25d0))
    (is-true (= (hash-table-count (r/c:<trd>-analog-ht trd)) 314))
    (is-true (= (hash-table-count (r/c:<trd>-discret-ht trd)) 101))))

(def-test analog-length ()
  "Место (байты) в записи, занимаемое всеми аналоговыми сигналами."
  (with-fixture fix-open-trd ()
    (is-true (= (r/trd:analog-length trd) 628))))

(def-test discret-length ()
  "Место (байты) в записи, занимаемое всеми дискретными сигналами."  
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:discret-length trd) 13))))

(def-test discret-offset () 
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:discret-offset trd) 628))))

(def-test start-offset ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:start-offset trd) 28316))))

(def-test record-length ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:record-length trd) 641))))

(def-test utime-end ()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:utime-end trd) 3750481735))))

(def-test record->utime()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:record->utime trd 1000) 3750478059))))

(def-test utime->record()
  (with-fixture fix-open-trd ()
    (is-true (= (recoder/trd:utime->record trd 3750478059) 1000))))

(def-test time-universal-encode()
    (is-true (= (recoder/trd:time-universal-encode 2021 09 16 12 27 25)  3840773245)))
