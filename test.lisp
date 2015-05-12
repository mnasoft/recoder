;;;; test.lisp

(in-package #:recoder)

(defun test_01(&optional (in (open-trd-file-read "/home/namatv/MyDoc/git/clisp/trd.files/20150423_190354.trd")))
  (let
      ((analog-signal-list(read-trend-hdr in)))
    (close in)
    analog-signal-list
    ))

;;(defparameter analog-signal-list (test_01 (open-trd-file-read "/home/namatv/MyDoc/git/clisp/trd.files/20150423_190354.trd")))

;;(defparameter analog-signal-list (test_01 (open-trd-file-read "/home/namatv/MyDoc/git/clisp/trd.files/230415_191202.trd")))
