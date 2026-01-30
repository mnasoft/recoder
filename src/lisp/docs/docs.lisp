(defpackage :recoder/docs
  (:use #:cl ) 
  (:nicknames "REC/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(recoder/docs) содержит функции
  генерирования и публикации документации."))

(in-package :recoder/docs)

(defun make-document ()
  (loop
    :for j :from 1
    :for i :in
    '((:recoder          :recoder)
      (:recoder/generics :recoder/generics)
      (:recoder/classes  :recoder/classes)
      (:recoder/trd      :recoder/trd)            
      (:recoder/a-signal nil)
      (:recoder/d-signal nil)
      (:recoder/dia      :recoder/dia)
      (:recoder/dir      :recoder/dir)
      (:recoder/seq      :recoder/seq)
      (:recoder/html     :recoder/html)
      (:recoder/org      nil)
      (:recoder/interval nil)
      (:recoder/split    nil)
      (:recoder/get      nil)
      (:recoder/slist    nil)
      )
    :do
       (progn
         (apply #'mnas-package:document i)
         (format t "~A ~A~%" j i))))

(defun make-graphs ()
  (loop
    :for j :from 1
    :for i :in
    '(:recoder
      :recoder/dia
      :recoder/split
      :recoder/dir
      :recoder/seq
      :recoder/html
      :recoder/org
      :recoder/interval
      :recoder/get
      :recoder/slist
      :recoder/trd
      :recoder/a-signal
      :recoder/d-signal
      :recoder/classes
      )
    :do (progn
          (mnas-package:make-codex-graphs i i)
          (format t "~A ~A~%" j i))))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :recoder)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
