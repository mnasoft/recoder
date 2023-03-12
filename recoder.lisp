;;;; package.lisp

(defpackage :recoder
  (:use #:cl)
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



;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
