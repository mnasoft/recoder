;;;; recoder.asd

(asdf:defsystem #:recoder
  :description "Describe recoder here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007"
  :depends-on (#:ie3fp #:mnas-string #:html-table #:math #:mnas-path)
  :serial t
  :components ((:file "package")
	       (:file "ascii")
	       (:file "cp866")
	       (:file "cp1251")
	       (:file "binary-read")
	       (:file "binary-write")
               (:file "recoder")
;;;;	       (:file "test")
	       ))
