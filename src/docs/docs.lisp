(defpackage #:recoder/docs
  (:use #:cl ) 
  (:nicknames "MSTR/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(recoder/docs) содержит функции
  генерирования и публикации документации."))

(in-package :recoder/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:recoder :recoder)
      (:recoder/a-signal :recoder/a-signal)
      (:recoder/d-signal :recoder/d-signal)
      (:recoder/seq :recoder/seq)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:recoder
      :recoder/a-signal
      :recoder/d-signal
      :recoder/seq
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/recoder.
"
  (mnas-package:make-html-path :recoder)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:recoder :recoder/docs)
   "Recoder"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "recoder")
   :output-format of)
  (codex:document :recoder)
  (make-graphs)
  (mnas-package:copy-doc->public-html "recoder")
  (mnas-package:rsync-doc "recoder"))

;;;; (make-all)
