;;;; ./src/slist/slist.lisp

(defpackage :recoder/slist
  (:use #:cl #:recoder/trd)
  (:nicknames "R/SLIST")
  (:export a-signals
           d-signals
           not-signals)
  (:documentation
   "@b(Описание:) пакет @b(recoder/slist) предназначен для
 формирования списков сигналов аналоговых и дискретных."))

(in-package :recoder/slist)

(defmethod not-signals ((trd <trd>) s-names)
  "@b(Описание:) метод @b(not-signals) возвращает список строк,
которые для тренда @b(trd) не являются именами аналоговых или
дискреных сигналов, присутствующих в списке @b(s-names).

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
  "@b(Описание:) метод @b(a-signals) возвращает список аналоговых
сигналов тренда @b(trd), имена которых присутствуют в списке имен
сигналов @b(s-names).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (a-signals
   recoder/trd:*trd*
   '(\"V2\" \"P02\" \"T2\" \"ET300\" \"FA530\" \"FK526\" \"FA526\" \"FA566\" \"KAZNA-SCHO\"))
 => (#a-s(30 \"V2\"    [0.0d0 25.0d0]      \"м3/с\" \"Объемный расход воздуха\")
     #a-s(89 \"P02\"   [0.0d0 1000000.0d0] \"Па\"   \"Полное давление на входе в КС\")
     #a-s(24 \"T2\"    [-10.0d0 600.0d0]   \"°C\"   \"Т2 - средняя\")
     #a-s(25 \"ET300\" [0.0d0 1600.0d0]    \"°C\"   \"Т3 - средняя\"))
@end(code)
"
  (when (file-descr trd)
    (loop :for s-n :in s-names
      :when (gethash s-n (analog-ht trd))
        :collect :it)))
#+nil
(a-signals
   recoder/trd:*trd*
   '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO"))

(defmethod d-signals ((trd <trd>) s-names)
  "@b(Описание:) метод @b(a-signals) возвращает список дискретных
сигналов тренда @b(trd), имена которых присутствуют в списке имен
сигналов @b(s-names).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-signals
   recoder/trd:*trd*
   '(\"V2\" \"P02\" \"T2\" \"ET300\" \"FA530\" \"FK526\" \"FA526\" \"FA566\" \"KAZNA-SCHO\"))
 => (#d-s(18 \"FA530\" \"Кран грубой регулировки топливного газа \")
     #d-s(47 \"FK526\" \"Кран №42 топливного газа крановой площад\")
     #d-s(55 \"FA526\" \"Кран грубой регулировки топливного газа \")
     #d-s(63 \"FA566\" \"Кран тонкой регулировки топливного газа \"))
@end(code)
"
  (when  (file-descr trd)
        (loop :for s-n :in s-names
      :when (gethash s-n (discret-ht trd))
        :collect :it)))

#+nil
(d-signals
   recoder/trd:*trd*
   '("V2" "P02" "T2" "ET300" "FA530" "FK526" "FA526" "FA566" "KAZNA-SCHO"))
