;;;; tests/package.lisp

(defpackage #:recoder/tests
  (:use #:cl #:fiveam)
  (:export run!
	   all-tests
	   test-recoder))
