(in-package :recoder/tests)

(def-suite get
  :description "Мастер-набор всех тестов проекта get."
  :in all)

(in-suite get)

(def-test trd-analog-test ()
  "Проверка извлечения аналоговых сигналов."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder/trd:<trd>-file-name trd)))
    (is-true (equal (recoder/get:trd-analog-by-utime
                     trd
                     (+ 600 (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-analog-signal-list
                      trd '("V2" "P02" "T2" "ET300")))
                    '(0.1274128328374151d0 106446.93675135424d0 15.457389181353474d0 424.24963759823d0)))
    (is-true (equal (recoder/get:trd-analog-by-utime
                     trd
                     (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-analog-signal-list trd '("V2" "P02" "T2" "ET300")))
                    '(0.49477378500038144d0 156145.5710688945d0 34.91111619745175d0 4.736400396734569d0)))))

(def-test trd-discret-test ()
  "Проверка извлечения дисктерных сигналов."
  (with-fixture fix-open-trd ()
    (is-true (probe-file (recoder/trd:<trd>-file-name trd)))
    (is-true (equal (recoder/get:trd-discret-by-utime 
                     trd
                     (+ 600 (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-discret-signal-list
                      trd '("GAS" "OIL")))
                    '(0 1)))
    (is-true (equal (recoder/get:trd-discret-by-utime 
                     trd
                     (+ (floor 15706 4) (recoder/trd:<trd>-utime-start trd))
                     (recoder/trd:trd-discret-signal-list
                      trd '("GAS" "OIL")))
                    '(0 1)))))
