;;;; tests/package.lisp

(defpackage #:recoder/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :recoder/tests)

(defun run-tests () (run! 'all))

(def-suite all
  :description "Мастер-набор всех тестов проекта Recoder.")

(in-suite all)

;;;;(run-tests)
