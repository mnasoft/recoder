;;;; recoder.asd

(defsystem #:recoder
  :description "Describe recoder here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
;  :depends-on (#:ie3fp #:mnas-string #:html-table #:math #:mnas-path #:mnas-file-dialog)
  :depends-on (#:ieee-floats #:mnas-string #:html-table #:math #:mnas-path #:mnas-file-dialog)

  :serial t
  :components ((:file "package")
	       (:file "ascii")
	       (:file "cp866")
	       (:file "cp1251")
	       (:file "binary-read")
	       (:file "binary-write")
	       (:file "defparameters")
	       (:file "classes")
               (:file "a-signal-defmethods")
	       (:file "d-signal-defmethods")
	       (:file "trd-defmethods")
               (:file "recoder")
;;;;	       (:file "test")
	       ))
