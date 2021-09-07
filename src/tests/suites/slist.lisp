(in-package :recoder/tests)

(def-suite slist
  :description "Мастер-набор всех тестов проекта get."
  :in all)

(in-suite slist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test trd-analog-signal-list ()
  (with-fixture fix-open-trd ()
    (let* ((a-names '("V2" "P02" "T2" "ET300" "KAZNA-SCHO"))
           (a-list (recoder/slist:trd-analog-signal-list trd a-names)))
      (is-true (eq (1- (length a-names)) (length a-list))))))
#+nil
(recoder/slist:trd-analog-signal-list *trd* '("V2" "P02" "T2" "ET300" "KAZNA-SCHO"))

(def-test trd-discret-signal-list ()
  (with-fixture fix-open-trd ()
    (let* ((d-names  '("FA530" "FK526" "FA526" "FA566" "KAZNA-SHO"))
           (d-list (recoder/slist:trd-discret-signal-list trd d-names)))
      (is-true (eq (1- (length d-names)) (length d-list))))))

#+nil
(recoder/slist:trd-discret-signal-list *trd* '("FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO"))

(recoder/slist:trd-separate-not-signals
 *trd*
 '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO"))
