;;;; recoder.asd

(defsystem "recoder"
  :description "Преднзначен для работы с трендами."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  ;;  :depends-on (#:ie3fp #:mnas-string #:html-table #:math #:mnas-path #:mnas-file-dialog)
  :depends-on (
               ;; "ieee-floats"
               "mnas-string"
               "html-table"
               "math"
               "mnas-path"
               "mnas-file-dialog"
               "recoder/binary"
               "recoder/a-signal"
               "recoder/d-signal"
               ) 
  :serial nil
  :in-order-to ((test-op (test-op "recoder/tests")))
  :components ((:file "package")
	       (:module "src"
		:serial t
		:depends-on ("package")
                :components
		((:file "defparameters")
		 (:file "trd-defmethods")
		 (:file "recoder")
		 (:file "example") 
		 ;; (:file "test") 
		 ))))

(defsystem "recoder/docs"
  :description "Зависимости для сборки документации."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder" "mnas-package"))


(defsystem "recoder/binary"
  :description "Преднзначен для работы с трендами. 
Содержит низкоуровневые функции ввода-вывода."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("ieee-floats" )
  ;; "mnas-string" "html-table" "math" "mnas-path" "mnas-file-dialog"
  :serial nil
  :components
  ((:module "src/binary"
    :serial t
    :components
    ((:file "package")
     (:file "ascii")
     (:file "cp866")
     (:file "cp1251")
     (:file "binary-read")
     (:file "binary-write")
     ))))

(defsystem "recoder/a-signal"
  :description "Преднзначен для работы с трендами.
Аналоговый сигнал"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :components
  ((:module "src/a-signal"
    :serial t
    :components
    ((:file "package")
     (:file "a-signal-defmethods")
     ))))


(defsystem "recoder/d-signal"
  :description "Преднзначен для работы с трендами.
Аналоговый сигнал"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial nil
  :components
  ((:module "src/d-signal"
    :serial t
    :components
    ((:file "package")
     (:file "d-signal-defmethods")
     ))))

(defsystem "recoder/tests"
  :description "Тестирование систем, входящих  в проект Recoder"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (:recoder :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :recoder/tests :test-recoder))
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "main")
				     (:file "trd")))))
