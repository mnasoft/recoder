;;;; recoder.asd

(defsystem "recoder"
  :description "@b(Описание:) система @b(Recoder) преднзначена для работы с трендами."
  ;; :long-description #.(uiop:read-file-string "doc/recoder-long-description.txt")
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-path"
               "recoder/trd"
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
  :components ((:module "src/lisp"
                :components  ((:file "recoder")))))

(defsystem "recoder/trd"
  :description "Преднзначен для работы с трендами."
  :depends-on ("mnas-string/print"
               "mnas-string/parse"
               "cl-ppcre"
               "recoder/a-signal"
               "recoder/d-signal"
               "mnas-bin"
               "trivial-octet-streams"
               ;; "mnas-file-dialog" "html-table" "mnas-path" "math" "mnas-string"
               ) 
  :serial nil
  :in-order-to ((test-op (test-op "recoder/tests")))
  :components ((:module "src/lisp/trd"
		:serial t
                :components
		((:file "trd")
                 (:file "read-obj-pathname")
		 ;; (:file "test") 
		 ))))

(defsystem "recoder/get"
  :description "@b(Описание:) система @b(recoder/get) содержит функции
  для извлечения информации из тренда."
  :depends-on ("recoder/slist" "math") ;; "recoder/trd"
  :serial nil
  :components
  ((:module "src/lisp/get"
    :serial nil
    :components
    ((:file "get")))))

(defsystem "recoder/html"
  :description "@b(Описание:) система @b(recoder/html) содержит функции
  для извлечения информации из тренда."
  :depends-on ("recoder/get" "recoder/slist" "html-table" "mnas-string/print")
  :serial nil
  :components
  ((:module "src/lisp/html"
    :serial nil
    :components
    ((:file "html")))))

(defsystem "recoder/interval"
  :description "@b(Описание:) система @b(recoder/get) содержит функции
  для извлечения информации из тренда."
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/lisp/interval"
    :serial nil
    :components
    ((:file "interval")))))

(defsystem "recoder/org"
  :description "@b(Описание:) система @b(recoder/org) содержит функции
  для вывода информации из тренда в ORG-режиме редактора Emacs."
  :depends-on ("recoder/trd"
               "mnas-string/print")
  :serial nil
  :components
  ((:module "src/lisp/org"
    :serial nil
    :components
    ((:file "org")))))

(defsystem "recoder/split"
  :description "@b(Описание:) система @b(recoder/split) содержит
  фукции поиска интервалов по в тренде по определенным критериям."
  :depends-on ("recoder/get"
               "mnas-string/print")
  :serial nil
  :components
  ((:module "src/lisp/split"
    :serial nil
    :components
    ((:file "split")))))

(defsystem "recoder/dia"
  :description "@b(Описание:) система @b(recoder/dia) содержит
  диалоговые фукции открытия трендов и каталогов с трендами."
  :depends-on ("mnas-file-dialog" "recoder/trd")
  :serial nil
  :components
  ((:module "src/lisp/dia"
    :serial nil
    :components
    ((:file "dia")))))

(defsystem "recoder/docs"
  :description "Зависимости для сборки документации."
  :depends-on ("recoder" "codex" "mnas-package")
  :components ((:module "src/lisp/docs"
		:serial nil
                :components ((:file "docs")))))


(defsystem "recoder/constants"
  :description "@b(Описание:) система @b(recoder/constants) определяет константы."
  :serial nil
  :components
  ((:module "src/lisp/constants"
    :serial t
    :components
    ((:file "constants")))))

(defsystem "recoder/d-signal"
  :description "Преднзначен для работы с трендами.
Аналоговый сигнал"
  :depends-on ("recoder/constants" "recoder/generics" "mnas-bin")
  :serial nil
  :components
  ((:module "src/lisp/d-signal"
    :serial t
    :components
    ((:file "d-signal")
     ))))

(defsystem "recoder/a-signal"
  :description "Преднзначен для работы с трендами.
Аналоговый сигнал"
  :depends-on ("recoder/constants"
               "recoder/generics"
               "mnas-bin"
               "float-features")
  :serial nil
  :components
  ((:module "src/lisp/a-signal"
    :serial t
    :components
    ((:file "a-signal")))))

(defsystem "recoder/seq"
  :description "Преднзначен для работы с трендами.
Определяет классы и методы для представления тренда в виде последовательности (sequience)."
  :depends-on ("recoder/get"
               "mnas-org-mode") ;;"recoder/trd"
  :serial nil
  :components
  ((:module "src/lisp/seq"
    :serial nil
    :components
    ((:file "seq")))))



(defsystem "recoder/dir"
  :description "Преднзначен для работы группами трендов, помещенными в отдельные каталоги."
  :depends-on ("recoder/get" ;; -> "math"; "recoder/slist" -> "recoder/trd"
               "termo-container"
               "mnas-org-mode"
               "mnas-format"
               "mnas-path"
               "html-table"
               )
  :serial nil
  :components
  ((:module "src/lisp/dir"
    :serial nil
    :components
    ((:file "dir")))))

(defsystem "recoder/slist"
  :description "@b(Описание:) система @b(recoder/slist) служит для получения списка синалов."
  :depends-on ("recoder/trd")
  :serial nil
  :components
  ((:module "src/lisp/slist"
    :serial nil
    :components
    ((:file "slist")
     ))))


(defsystem "recoder/tests"
  :description "Тестирование систем, входящих  в проект Recoder."
  :depends-on ("recoder" "fiveam")      ; "math/arr-matr"
  :perform (test-op (o s)
		    (uiop:symbol-call :mnas-string/tests :run-tests))
  :components ((:module "src/lisp/tests"
		:serial nil
                :components ((:file "tests")))
               (:module "src/lisp/tests/suites"
                :depends-on ("src/lisp/tests")
		:serial nil
                :components ((:file "a-signal")
                             (:file "d-signal")
                             (:file "trd")
                             (:file "slist")
                             (:file "get")
                             (:file "split")
                             (:file "interval")
                             (:file "org")))
               (:module "src/lisp/tests/run"
                :depends-on ("src/lisp/tests/suites")
		:serial nil
                :components ((:file "run")))))


(defsystem "recoder/generics"
  :description
  "@b(Описание:) система @b(recoder/generics) определяет обобщенные
 функции."
  :components ((:module "src/lisp/generics"
		:serial nil
                :components ((:file "generics")))))
