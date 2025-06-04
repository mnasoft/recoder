;;;; [[/home/mna/quicklisp/local-projects/clisp/recoder/recoder.lisp]]

(defpackage :recoder
  (:use #:cl)
  (:nicknames "R")
  (:export trd-open)
  (:documentation
   "@begin(section) @title(Обзор)

 Пакет @b(Recoder) предназначен для извлечения информации из трендов.

 @b(Определения:)
@begin(deflist)
@term(Тенд)
 @def(Файл, в котором записаны значения аналоговых и дискретных сигналов
      через равные промежутки времени.)
@term(Сигнал)
 @def(Параметр, фиксируемый и обрабатываемый САУ.)
@term(Аналоговый сигнал)
 @def(Параметр, значения которого могут лежать в определнном диапазоне.)
@term(Дискретный сигнал (флаг))
 @def(Параметр, значения которого могут принимать два значения:
 0 - (нет ложь) или 1 - (да истина).)
@end(deflist)

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
