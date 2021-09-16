(in-package :recoder/tests)

(def-suite get
  :description "Мастер-набор всех тестов проекта get."
  :in all)

(in-suite get)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+nil

(defpackage #:recoder/get
  (:use #:cl
        #:recoder/binary
        #:recoder/d-signal
        #:recoder/a-signal
        #:recoder/trd
        #:recoder/slist
        #:mnas-string/print)
  (:export *offset*)
  (:export trd-analog-mid-by-snames       +
           trd-analog-stddev-by-snames    +
           trd-analog-mid-by-utime        +
           trd-analog-stddev-by-utime     +
           trd-analog-by-record           +
           trd-analog-by-utime            +
           )
  (:export trd-discret-by-record          +
           trd-discret-by-record-t-nil    +
           trd-discret-by-utime           +
           trd-discret-by-utime-t-nil     +
           )
  (:export trd-analog-discret-by-record   +
           )
  (:export analogs-in-records             +
           analogs-in-utimes              +
           )
  (:export trd-a-ids                      +
           trd-a-units                    +
           ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test trd-analog-mid-by-snames ()
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true
       (semi-equal
        (recoder/get:trd-analog-mid-by-snames
         trd
         (+ 600 (recoder/trd:<trd>-utime-start trd))
         a-names)
        '(0.1269768607832238d0 106507.972838941d0 15.469356614241027d0 424.66468299382007d0))))))

(def-test trd-analog-stddev-by-snames ()
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true
       (semi-equal
        (recoder/get:trd-analog-stddev-by-snames
         trd
         (+ 600 (recoder/trd:<trd>-utime-start trd))
         a-names)
        '(7.06355458741549d-4 40.08217147040062d0 0.04416575081504683d0 3.190519247625745d0))))))

(def-test trd-analog-mid-by-utime ()
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true
       (semi-equal
        (recoder/get:trd-analog-mid-by-utime
         trd
         (+ (recoder/trd:<trd>-utime-start trd) 2000)
         (recoder/slist:a-signals trd a-names))
        '(0.13618677042801555d0 107386.45652813654d0 23.159540340130864d0 489.1862218298474d0))))))

(def-test trd-analog-stddev-by-utime ()
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true
       (semi-equal
        (recoder/get:trd-analog-stddev-by-utime
         trd
         (+ (recoder/trd:<trd>-utime-start trd) 2000)
         (recoder/slist:a-signals trd a-names))
        '(7.53353843676347d-4 47.907358065677975d0 0.029378371881425107d0 1.68445279343399d0))))))


(def-test trd-analog-by-record ()
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true
       (equal
        (recoder/get:trd-analog-by-record trd 1000  (recoder/slist:a-signals trd a-names) )
        '(0.13962005035477226d0 105607.69054703593d0 13.577172503242544d0 16.552986953536276d0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test trd-analog-test ()
  "Проверка извлечения аналоговых сигналов."
  (with-fixture fix-open-trd ()
    (with-fixture a-names ()
      (is-true (equal (recoder/get:trd-analog-by-utime
                       trd
                       (+ 600 (recoder/trd:<trd>-utime-start trd))
                       (recoder/slist:a-signals trd a-names))
                      '(0.1274128328374151d0 106446.93675135424d0 15.457389181353474d0 424.24963759823d0)))
      (is-true (equal (recoder/get:trd-analog-by-utime
                       trd
                       (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                       (recoder/slist:a-signals trd a-names))
                      '(0.49477378500038144d0 156145.5710688945d0 34.91111619745175d0 4.736400396734569d0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test trd-discret-by-record ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (with-fixture d-names ()
      (is-true (equal (recoder/get:trd-discret-by-record trd
                                                         2400 
                                                         (recoder/slist:d-signals trd d-names))
                      '(0 0 0 0)))
      (is-true (equal (recoder/get:trd-discret-by-record trd
                                                         15705
                                                         (recoder/slist:d-signals trd d-names))
                      '(0 0 0 0))))))

(def-test trd-discret-by-record-t-nil ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (with-fixture d-names ()
      (is-true (equal (recoder/get:trd-discret-by-record-t-nil trd
                                                               2400 
                                                               (recoder/slist:d-signals trd d-names))
                      '(nil nil nil nil)))
      (is-true (equal (recoder/get:trd-discret-by-record-t-nil trd
                                                               15705
                                                               (recoder/slist:d-signals trd d-names))
                      '(nil nil nil nil))))))

(def-test trd-discret-by-utime ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (with-fixture d-names ()
    (is-true (equal (recoder/get:trd-discret-by-utime 
                     trd
                     (+ 600 (recoder/trd:<trd>-utime-start trd))
                     (recoder/slist:d-signals trd d-names))
                    '(0 0 0 0)))
    (is-true (equal (recoder/get:trd-discret-by-utime 
                     trd
                     (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                     (recoder/slist:d-signals trd d-names))
                    '(0 0 0 0))))))

(def-test trd-discret-by-utime-t-nil ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (with-fixture d-names ()
      (is-true (equal (recoder/get:trd-discret-by-utime-t-nil
                       trd
                       (+ 600 (recoder/trd:<trd>-utime-start trd))
                       (recoder/slist:d-signals trd d-names))
                      '(nil nil nil nil)))
      (is-true (equal (recoder/get:trd-discret-by-utime-t-nil 
                       trd
                       (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                       (recoder/slist:d-signals trd d-names))
                      '(nil nil nil nil))))))

(def-test trd-analog-discret-by-record ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (is-true
       (equal
        (recoder/get:trd-analog-discret-by-record trd
                                                  1000
                                                  (recoder/slist:a-signals trd names)
                                                  (recoder/slist:d-signals trd names))
        '(0.13962005035477226d0 105607.69054703593d0 13.577172503242544d0 16.552986953536276d0 0 0 0 0))))))

(def-test analogs-in-records ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (is-true (equal (recoder/get:analogs-in-records trd
                                                      0 3
                                                      (recoder/slist:a-signals trd names))
                      '((0.18692301823453117d0 0.18272678721293964d0 0.18272678721293964d0)
                        (107866.02578774701d0 107774.47165636683d0 107774.47165636683d0)
                        (11.613183794918744d0 11.613183794918744d0 11.613183794918744d0)
                        (1.6601815823605708d0 2.3193713282978563d0 2.2705424582284275d0)))))))

(def-test analogs-in-utimes ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (is-true
       (equal
        (recoder/get:analogs-in-utimes trd
                                       (+ 600 (recoder/trd:<trd>-utime-start trd))
                                       (+ 601 (recoder/trd:<trd>-utime-start trd))
                                       (recoder/slist:a-signals trd names))
        '((0.1274128328374151d0 0.1274128328374151d0 0.1274128328374151d0 0.1274128328374151d0)
          (106446.93675135424d0 106446.93675135424d0 106446.93675135424d0 106446.93675135424d0)
          (15.457389181353474d0 15.494621194781413d0 15.494621194781413d0 15.494621194781413d0)
          (424.24963759823d0 423.5904478522927d0 423.00450141145956d0 425.42153047989626d0)))))))


(def-test trd-a-ids ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (is-true (equal (recoder/get:trd-a-ids names trd)
                      '("V2" "P02" "T2" "ET300"))))))

(def-test trd-a-units ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
    (is-true (equal (recoder/get:trd-a-units names trd)
                '("м3/с" "Па" "°C" "°C"))))))


