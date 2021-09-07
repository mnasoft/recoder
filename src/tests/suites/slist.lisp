(in-package :recoder/tests)

(def-suite slist
  :description "Мастер-набор всех тестов проекта get."
  :in all)

(in-suite slist)

(def-fixture fix-sig-names ()
  (let ((names '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO")))
      (&body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test a-signals ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (let ((a-list (recoder/slist:a-signals trd names)))
        (is-true (eq 4 (length a-list)))))))

(def-test d-signals ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (let* ((d-list (recoder/slist:d-signals trd names)))
        (is-true (eq 4 (length d-list)))))))

(def-test d-signals ()
  (with-fixture fix-open-trd ()
    (with-fixture fix-sig-names ()
      (let* ((not-list (recoder/slist:not-signals trd names)))
        (is-true (equal '("KAZNA-SCHO") not-list))))))

