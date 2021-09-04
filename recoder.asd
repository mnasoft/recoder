;;;; recoder.asd

(defsystem "recoder"
  :description "Преднзначен для работы с трендами."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/trd"
               "recoder/seq"
               "recoder/dir"
               "recoder/dia"
               "recoder/split") 
  :serial nil
  :in-order-to ((test-op (test-op "recoder/tests")))
  :components  ((:file "package")))

(defsystem "recoder/trd"
  :description "Преднзначен для работы с трендами."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string"
               "html-table"
               "math"
               "mnas-path"
               ;; "mnas-file-dialog"
               "recoder/binary"
               "recoder/a-signal"
               "recoder/d-signal"
               ) 
  :serial nil
  :in-order-to ((test-op (test-op "recoder/tests")))
  :components ((:module "src/trd"
		:serial t
                :components
		((:file "trd")
		 ;; (:file "test") 
		 ))))

(defsystem "recoder/split"
  :description "@b(Описание:) система @b(recoder/split) содержит
  фукции поиска интервалов по в тренде по определенным критериям."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/split"
    :serial nil
    :components
    ((:file "split")))))

(defsystem "recoder/dia"
  :description "@b(Описание:) система @b(recoder/dia) содержит
  диалоговые фукции открытия трендов и каталогов с трендами."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-file-dialog")
  :serial nil
  :components
  ((:module "src/dia"
    :serial nil
    :components
    ((:file "dia")))))

(defsystem "recoder/docs"
  :description "Зависимости для сборки документации."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))

(defsystem "recoder/binary"
  :description "Преднзначен для работы с трендами. 
Содержит низкоуровневые функции ввода-вывода."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("ieee-floats")
  ;; "mnas-string" "html-table" "math" "mnas-path" "mnas-file-dialog"
  :serial nil
  :components
  ((:module "src/binary"
    :serial t
    :components
    ((:file "binary")))))

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
    ((:file "a-signal")))))

(defsystem "recoder/seq"
  :description "Преднзначен для работы с трендами.
Определяет классы и методы для представления тренда в виде последовательности (sequience)."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("recoder/trd" "mnas-org-mode")
  :serial nil
  :components
  ((:module "src/seq"
    :serial nil
    :components
    ((:file "trd-seq")))))

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
    ((:file "d-signal")
     ))))

(defsystem "recoder/dir"
  :description "Преднзначен для работы группами трендов, помещенными в отдельные каталоги."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("recoder/trd" "termo-container" "mnas-org-mode" "math" "mnas-format")
  :serial nil
  :components
  ((:module "src/dir"
    :serial nil
    :components
    ((:file "direcory-trd")
     ))))

(defsystem "recoder/tests"
  :description "Тестирование систем, входящих  в проект Recoder."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder" "fiveam")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-string/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "tests")))
               (:module "src/tests/suites"
                :depends-on ("src/tests")
		:serial nil
                :components ((:file "trd")
                             (:file "binary")))
               (:module "src/tests/run"
                :depends-on ("src/tests/suites")
		:serial nil
                :components ((:file "run")))))
