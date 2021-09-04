(in-package :recoder/tests)

(def-suite interval
  :description "Мастер-набор всех тестов проекта interval."
  :in all)

(in-suite interval)

(def-test trd-interval-to-secods ()
  (with-fixture fix-open-trd ()
    (is-true
      (equal (recoder/interval:trd-interval-to-secods *trd* '(1000 3000))  500.0d0))))

(def-test trd-interval-to-minutes ()
  (with-fixture fix-open-trd ()
    (is-true
     (equal (recoder/interval:trd-interval-to-minutes *trd* '(1000 3000)) 8.333333333333334d0))))

(def-test trd-interval-to-hours ()
  (with-fixture fix-open-trd ()
    (is-true
      (equal (recoder/interval:trd-interval-to-hours *trd* '(1000 3000)) 0.1388888888888889d0))))

