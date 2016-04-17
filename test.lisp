;;;; test.lisp

(in-package #:recoder)

(defun test_01(&optional (in (open-trd-file-read "/home/namatv/MyDoc/git/clisp/trd.files/20150423_190354.trd")))
  (let
      ((analog-signal-list(read-trend-hdr in)))
    (close in)
    analog-signal-list
    ))

(defun test_02(fname)
  (let* ((in (open-trd-file-read fname))
	 (header (read-trend-header in))
	 (analog-number (nth 11 header))
	 (discret-number (nth 12 header))
	 (analog-descriptor-list (read-trend-analog-descriptor-list in analog-number))
	 (discret-descriptor-list (read-trend-discret-descriptor-list in discret-number))
	 (analog-record	 (read-trend-analog-record in analog-number analog-descriptor-list)))
    (close in)
    (list header
	  analog-descriptor-list
	  discret-descriptor-list
	  analog-record)))

;;;;(test_02 "/home/namatv/My/git/Trends/ДМ80№1/230415_191202.trd")

;;(defparameter analog-signal-list (test_01 (open-trd-file-read "/home/namatv/My/git/Trends/ДМ80№1/230415_165454.trd")))

;;(defparameter analog-signal-list (test_01 (open-trd-file-read "/home/namatv/My/git/clisp/trd.files/230415_191202.trd")))
