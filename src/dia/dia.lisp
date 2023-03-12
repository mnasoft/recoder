;;;; ./clisp/recoder/src/dia/dia.lisp

(defpackage :recoder/dia
  (:use #:cl #:mnas-string/print #:recoder/binary #:recoder/d-signal #:recoder/a-signal)
  (:nicknames "R/DIA")
  (:export get-open-ternds
	   get-open-ternd
	   change-directory-default)
  (:export *trd*))

(in-package :recoder/dia)

(defparameter *trd* nil
  "@b(Описание:) переменная @b(*trd*) содержит объект класса @b(recoder/trd:<trd>).
")

(defun get-open-ternd ()
  "@b(Описание:) функция @b(get-open-ternd) возвращает объект класса
  @b(recoder/trd:<trd>).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (get-open-ternd)
@end(code)"
  (setf *trd*
        (r/trd:trd-open
         (make-instance 'recoder/trd:<trd> :file-name
                        (mnas-file-dialog:get-open-file
                         :filetypes '(("Файлы трендов" "*.trd"))
                         :title "Выберите файл тренда")))))

(defun get-open-ternds ()
  "@b(Описание:) функция @b(get-open-ternds) возвращает список имен,
 отобранных в диалоге файлов.

 @b(Пример использования:) @begin[lang=lisp](code)
 (get-open-ternds)
@end(code)

"
  (mnas-file-dialog:get-open-file
   :filetypes '(("Файлы трендов" "*.trd"))
   :title "Выберите файлы трендов" :multiple t))

(defun change-directory-default ()
  
  (mnas-file-dialog:change-directory-default))
