;;;; ./src/tests/suites/org.lisp

(in-package :recoder/tests)

(def-suite org
  :description "Мастер-набор всех тестов проекта recoder/org."
  :in all)

(in-suite org)

(def-test header ()
  "Проверка извлечения заголовка."
    (with-fixture fix-open-trd ()
  (is-true (equal
            (recoder/org:header trd)
             `(("Файл"          ,(r/trd:<trd>-file-name trd))
               ("Версия тренда" ,(r/trd:<trd>-version *trd*)) 
               ("Дата создания тренда" "2018-11-06")
               ("Время создания тренда" "09:23:29")
               ("К-во аналоговых+дискретных сигналов" 415)
               ("Общее число записей в тренде" 15706)
               ("Интервал между записями тренда" 0.25d0)
               ("Количество аналоговых сигналов" 314)
               ("Количество дискретных сигналов" 101))))))

(def-test a-signals ()
  "Проверка извлечения заголовков аналоговых сигналов."
  (is-true (equal (length (recoder/org:a-signals *trd*)) 314))
  (is-true
   (equal
    (elt (recoder/org:a-signals *trd*) 10)
    '(10 "FP620" 0.0d0 600.0d0 "кПа" "Избыточное давление топливного газа во 2")))
  (is-true
   (equal
    (elt (recoder/org:a-signals *trd*) 105)
    '(105 "ET005" 0.0d0 1600.0d0 "°C" "Температура продуктов сгорания на выходе")))
  (is-true
   (equal
    (elt (recoder/org:a-signals *trd*) 313)
    '(313 "SF2" 0.0d0 1000.0d0 "" "Площадь сечения на входе в кс, см2"))))

(def-test d-signals ()
  "Проверка извлечения заголовков дискретных сигналов."
  (is-true
   (equal (length (recoder/org:d-signals *trd*)) 101))
  (is-true
   (equal (elt (recoder/org:d-signals *trd*) 10)
          '(10 "FK530" "Кран №43 топливного газа крановой площад")))
  (is-true
   (equal (elt (recoder/org:d-signals *trd*) 50)
          '(50 "FK537" "Кран №43 топливного газа крановой площад")))
  (is-true
   (equal (elt (recoder/org:d-signals *trd*) 100)
          '(100 "FH011" "Кран подачи ДТ - закрыт"))))
