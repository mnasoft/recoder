;;;; [[/home/mna/quicklisp/local-projects/clisp/recoder/recoder.lisp]]

(defpackage :recoder
  (:use #:cl)
  (:nicknames "R")
  (:export trd-open)

  (:documentation
   "@begin(section) @title(Обзор)

 Пакет @b(Recoder) содержит:
 @b(функции:)
@begin(list)
 @item(@b(trd-open) - открытие файла тренда.)
@end(list)

@end(section)
"))

(in-package :recoder)

(defun trd-open (f-name)
  "@b(Описание:) функция @b(trd-open) возвращает объект тренда.

 @b(Переменые:)
@begin(list)
 @item(f-name - полный путь к файлу тренда.)
@end(list)
"
  (when (probe-file f-name)
    (let ((trd (make-instance 'r/trd:<trd> :file-name f-name)))
      (r/trd:trd-open trd)
      trd)))


