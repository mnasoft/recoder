;;;; ./clisp/recoder/src/dia/dia.lisp

(defpackage #:recoder/dia
  (:use #:cl #:mnas-string/print #:recoder/binary #:recoder/d-signal #:recoder/a-signal)
  (:nicknames "R/DIA")
  (:export get-open-ternds
	   get-open-ternd
	   change-directory-default))

(in-package #:recoder/dia)

(defun get-open-ternd ()
  "@b(Описание:) функция @b(get-open-ternd)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (get-open-ternd)
@end(code)"
  (mnas-file-dialog:get-open-file
   :filetypes '(("Файлы трендов" "*.trd"))
   :title "Выберите файл тренда"))

(defun get-open-ternds ()
  (mnas-file-dialog:get-open-file
   :filetypes '(("Файлы трендов" "*.trd"))
   :title "Выберите файлы трендов" :multiple t))

(defun change-directory-default ()
  (mnas-file-dialog:change-directory-default))
