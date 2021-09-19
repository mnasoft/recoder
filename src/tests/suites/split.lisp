;;;; ./src/tests/suites/split.lisp

(in-package :recoder/tests)

(def-suite split
  :description "Мастер-набор всех тестов проекта recoder/split."
  :in all)

(in-suite split)

#+nil
(defpackage #:recoder/split
  (:use #:cl #:mnas-string/print #:recoder/trd)
  (:intern apply-and                                   +
	   apply-or                                    +
	   )
  (:export split-on-intervals-of-time-when-flag-is-on
	   split-on-intervals-when-flag-is-on
   	   split-on-intervals-by-condition
	   split-on-utimes-when-flag-is-on))

(def-test apply-and ()
  (is-false
   (recoder/split::apply-and '(nil   t nil)))
  (is-false
   (recoder/split::apply-and '(  t   t nil)))
  (is-true
   (recoder/split::apply-and '(  t   t   t)))
    (is-true
   (recoder/split::apply-and '(  1   2   3))))

(def-test apply-or ()
  (is-true
   (recoder/split::apply-or '(nil   t nil)))
  (is-true
   (recoder/split::apply-or '(  t   t nil)))
  (is-true
   (recoder/split::apply-or '(  t   t   t)))
  (is-true
   (recoder/split::apply-or '(  1   2   3)))
    (is-false
   (recoder/split::apply-or '(nil nil nil)))
  )

#+nil
(recoder/get:trd-discret-by-record *trd* 100 (recoder/slist:d-signals *trd* '("OIL" "GAS")))



