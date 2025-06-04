;;;; ./src/slist/slist.lisp

(defpackage :recoder/slist
  (:use #:cl #:recoder/trd)
  (:nicknames "R/SLIST")
  (:export a-signals
           d-signals
           not-signals)
  (:documentation
   "@b(Описание:) пакет @b(recoder/slist) предназначен для формирования
списков сигналов аналоговых и дискретных. Также пакет позволяет
определить, какое имя сигналом не является."))

(in-package :recoder/slist)

(defgeneric a-signals (trd s-names)
  (:documentation 
   "@b(Описание:) обобщенная_функция @b(a-signals) возвращает список аналоговых
сигналов тренда @b(trd), имена которых присутствуют в списке имен
сигналов @b(s-names)."))

(defgeneric d-signals (trd s-names)
  (:documentation 
  "@b(Описание:) обобщенная_функция @b(d-signals) возвращает список дискретных
сигналов тренда @b(trd), имена которых присутствуют в списке имен
сигналов @b(s-names)."))

(defgeneric not-signals (trd s-names)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(not-signals) возвращает список
строк, которые для тренда @b(trd) не являются именами аналоговых или
дискреных сигналов, присутствующих в списке @b(s-names)."))

(defmethod not-signals ((trd <trd>) s-names)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (not-signals
   recoder/trd:*trd*
   '(\"V2\" \"P02\" \"T2\" \"ET300\" \"FA530\" \"FK526\" \"FA526\" \"FA566\" \"KAZNA-SCHO\"))
   ; => (\"KAZNA-SCHO\")
@end(code)
"
  (when (file-descr trd)
    (loop :for s-n :in s-names
          :unless (or (gethash s-n (analog-ht trd))
                      (gethash s-n (discret-ht trd)))
            :collect s-n)))

(defmethod a-signals ((trd <trd>) s-names)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (a-signals
   recoder/trd:*trd*
   '(\"V2\" \"P02\" \"T2\" \"ET300\" \"FA530\" \"FK526\" \"FA526\" \"FA566\" \"KAZNA-SCHO\"))
@end(code)
"
  (when (file-descr trd)
    (loop :for s-n :in s-names
      :when (gethash s-n (analog-ht trd))
        :collect :it)))

(defmethod d-signals ((trd <trd>) s-names)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-signals
   recoder/trd:*trd*
   '(\"V2\" \"P02\" \"T2\" \"ET300\" \"FA530\" \"FK526\" \"FA526\" \"FA566\" \"KAZNA-SCHO\"))
@end(code)"
  (when (file-descr trd)
        (loop :for s-n :in s-names
      :when (gethash s-n (discret-ht trd))
        :collect :it)))
