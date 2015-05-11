;;;; recoder.lisp

(in-package #:recoder)

;;; "recoder" goes here. Hacks and glory await!

(defun recode-string (bufer &optional (start 0) (len (length bufer)) &key (break-nul T) (code-page *cp1251*))
  "Выполняет преобразование символов, передаваемых в параметре bufer,
имеющих кодировку code-page (*cp1251*|*cp866*), в кодировку utf8."
  (do*
   ( (i start (1+ i))
     (ch (gethash (nth i bufer) code-page) (gethash (nth i bufer) code-page))
     (str-rez ""))
   ( (or (>= i (+ start len))
	 (and break-nul (= 0 (nth i bufer)))) 
    str-rez)
    (setf str-rez (concatenate 'string str-rez ch))))
