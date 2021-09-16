;;;; recoder.asd

(defsystem "recoder"
  :description "@b(Описание:) система @b(Recoder) преднзначена для работы с трендами.

 Система состоит из следующих подсистем:
@begin(deflist)
      
@term(recoder/trd) @def(...)

@term(recoder/seq) @def(Содержит функции, позволяющие работать с
отдельным трендом как с последовательностью.)

@term(recoder/org) @def(...)

@term(recoder/dir) @def(...)
@term(recoder/dia) @def(...)

@term(recoder/split) @def(Содержит функции, позволяющие разделить
тренд на группу диапазонов.)

@end(deflist)
"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/trd"
               "recoder/get"
               "recoder/html"
               "recoder/interval"
               
               "recoder/org"
               "recoder/seq"
               "recoder/dir"
               "recoder/dia"
               "recoder/split") 
  :serial nil
  :in-order-to ((test-op (test-op "recoder/tests")))
  :components  ((:file "recoder")))

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

(defsystem "recoder/get"
  :description "@b(Описание:) система @b(recoder/get) содержит функции
  для извлечения информации из тренда."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/slist") ;; "recoder/trd"
  :serial nil
  :components
  ((:module "src/get"
    :serial nil
    :components
    ((:file "get")))))

(defsystem "recoder/html"
  :description "@b(Описание:) система @b(recoder/html) содержит функции
  для извлечения информации из тренда."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/get" "recoder/slist") ;; "recoder/trd"
  :serial nil
  :components
  ((:module "src/html"
    :serial nil
    :components
    ((:file "html")))))

(defsystem "recoder/interval"
  :description "@b(Описание:) система @b(recoder/get) содержит функции
  для извлечения информации из тренда."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/interval"
    :serial nil
    :components
    ((:file "interval")))))

(defsystem "recoder/org"
  :description "@b(Описание:) система @b(recoder/org) содержит функции
  для вывода информации из тренда в ORG-режиме редактора Emacs."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/org"
    :serial nil
    :components
    ((:file "org")))))

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
  :depends-on ("ieee-floats" "babel")
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
  :depends-on ("recoder/slist" "recoder/get" "mnas-org-mode") ;;"recoder/trd"
  :serial nil
  :components
  ((:module "src/seq"
    :serial nil
    :components
    ((:file "seq")))))

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

(defsystem "recoder/slist"
  :description "@b(Описание:) система @b(recoder/slist) служит для получения списка синалов."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/slist"
    :serial nil
    :components
    ((:file "slist")
     ))))


(defsystem "recoder/tests"
  :description "Тестирование систем, входящих  в проект Recoder."
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("recoder" "fiveam" "math/arr-matr")
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-string/tests :run-tests))
  :components ((:module "src/tests"
		:serial nil
                :components ((:file "tests")))
               (:module "src/tests/suites"
                :depends-on ("src/tests")
		:serial nil
                :components ((:file "binary")
                             (:file "trd")
                             (:file "slist")
                             (:file "get")
                             (:file "split")
                             (:file "interval")
                             (:file "org")))
               (:module "src/tests/run"
                :depends-on ("src/tests/suites")
		:serial nil
                :components ((:file "run")))))
