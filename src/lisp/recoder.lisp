;;;; ./recoder.lisp

(defpackage :recoder
  (:use #:cl)
  (:nicknames "R")
  (:export trd-open
           recode
           recode-xls
           )

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
    (let ((trd (make-instance 'r/c:<trd> :file-name f-name)))
      (r/trd:trd-open trd)
      trd)))

(defun recode-xls ()
  (loop :for i :in (mapcar
                    #'pathname
                    (mnas-file-dialog:get-open-file :filetypes '(("XLS" "*.xls")) :multiple t))
        #+nil (directory "*.xls")
        :for n :from 1
        :do
           (format t "~3A ~A~%" n i)
           (recode i)))

(defun recode (path)
  "@b(Описание:) функция @b(path) выполняет перекодирование тренда из
формата xls или txt в формат трендера trd.

 При перекодировании функция основывается на расширении файла.
"
  (let* ((ps1-script-fn 
           (first (directory (concatenate 'string (namestring (mnas-path:posix-arg0-path)) "*.ps1"))))
         (ps1-script
           (when ps1-script-fn
             (probe-file ps1-script-fn))))
    (format t "~A~%" ps1-script)
    (cond
      (ps1-script
       (setf r/trd:*convert-excel-to-txt-ps1* ps1-script))
      ((probe-file r/trd:*convert-excel-to-txt-ps1*))
      (t (error "~A" r/trd:*convert-excel-to-txt-ps1*)))
    (let ((trd (make-instance 'r/c:<trd>)))
      (r/g:read-obj trd path)
      (r/g:write-obj trd (r/trd::fname-xls->trd path)))))


;; sbcl
#+nil (asdf:load-system :recoder)
#+nil (save-lisp-and-die "recode-xls-trd.exe"  :executable t :toplevel #'r:recode-xls)
