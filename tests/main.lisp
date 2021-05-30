;;;; tests/main.lisp

(in-package #:recoder/tests)

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта recoder.")

(in-suite all-tests)

(defun test-recoder ()
  (run! 'all-tests))
