;;;; ./src/org/org.lisp

(defpackage #:recoder/org
  (:use #:cl #:recoder/trd #:recoder/d-signal #:recoder/a-signal #:mnas-string/print)
  (:export analog-signals
           discret-signals
           header))

(in-package :recoder/org)

(defmethod header ((trd <trd>))
  "@b(Описание:) метод @b(header) возвращает общую информацию
 о тренде в виде пригодном для вставки таблицы в ORG-режиме редактора
 Emacs.

 @b(Пример использования:)
@begin[lang=org](code)
#+begin_src lisp
  (recoder/org:header recoder/trd:*trd*)
#+end_src
@end(code)

@b(Пример использования:) 
@begin[lang=lisp](code)
 (header *trd*)
@end(code)"
  (let ((rez nil))
    (push (list "Файл" (<trd>-file-name trd )) rez)
    (when (<trd>-file-descr trd)
      (progn
	(push (list "Версия тренда" 	                  (<trd>-version        trd)) rez)
	(push (list "Дата создания тренда"                (date (<trd>-utime-start trd) :stream nil)) rez)
	(push (list "Время создания тренда"               (day-time (<trd>-utime-start trd) :stream nil)) rez)
	(push (list "К-во аналоговых+дискретных сигналов" (<trd>-reserv         trd)) rez)
	(push (list "Общее число записей в тренде"        (<trd>-total-records  trd)) rez)
	(push (list "Интервал между записями тренда"      (<trd>-delta-time     trd)) rez)
	(push (list "Количество аналоговых сигналов"      (<trd>-analog-number  trd)) rez)
	(push (list "Количество дискретных сигналов"      (<trd>-discret-number trd)) rez)))
    (nreverse rez)))

(defmethod analog-signals ((trd <trd>))
  "@b(Описание:) метод @b(analog-signals) возврвщает 2d-list список,
 отображающий все аналоговые сигналы в удобном виде для представления
 в ORG-режиме редактора Emacs.

 @b(Пример использования:)
@begin[lang=org](code)
#+begin_src lisp
(recoder/org:header recoder/trd:*trd*)
;;(recoder/org:analog-signals recoder/trd:*trd*)
;;(recoder/org:discret-signals recoder/trd:*trd*)
#+end_src
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (analog-signals *trd*) 
 =>
 ((0 \"FP510\" 0.0d0 1.600000023841858d0 \"МПа\" \"Избыточное давление газа перед суммирующ\")
  (1 \"FP520\" 0.0d0 16.0d0 \"кПа\" \"Перепад давления газа на суммирующем РУ\")
;;..........................................................................
  (313 \"SF2\" 0.0d0 1000.0d0 \"\" \"Площадь сечения на входе в кс, см2\"))
@end(code)
@begin[lang=org](code)
|   0 | FP510    |   0.0d0 | 1.600000023841858d0 | МПа     | Избыточное давление газа перед суммирующ |
|   1 | FP520    |   0.0d0 |              16.0d0 | кПа     | Перепад давления газа на суммирующем РУ  |
.......................................................................................................
| 313 | SF2      |   0.0d0 |            1000.0d0 |         | Площадь сечения на входе в кс, см2       |
@end(code)"
  (loop :for k :being :the hash-key :using (hash-value v) :of (<trd>-analog-ht trd)
	:collect
        (mapcar #'(lambda (f) (funcall f v))
                '(<a-signal>-num <a-signal>-id
                  <a-signal>-min <a-signal>-max
                  <a-signal>-units <a-signal>-description))))

(defmethod discret-signals ((trd <trd>))
  "@b(Описание:) метод @b(discret-signals) возврвщает 2d-list список,
отображающий все дискретные сигналы в удобном виде для представления в org режиме.

 @b(Пример использования:)
@begin[lang=org](code)
#+begin_src lisp
(recoder/org:discret-signals recoder/trd:*trd*)
#+end_src
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (discret-signals *trd*) 
=>
 ((0 \"FH020\" \"Дроссельный кран рабочего насоса ДТ - от\")
  (1 \"FH021\" \"Дроссельный кран рабочего насоса ДТ - за\")
;;.....................................................
  (100 \"FH011\" \"Кран подачи ДТ - закрыт\"))
@end(code)
@begin[lang=org](code)
|   0 | FH020 | Дроссельный кран рабочего насоса ДТ - от |
|   1 | FH021 | Дроссельный кран рабочего насоса ДТ - за |
..........................................................
| 100 | FH011 | Кран подачи ДТ - закрыт                  |
@end(code)"
  (loop :for k :being :the hash-key :using (hash-value v) :of (<trd>-discret-ht trd)
	:collect
                (mapcar #'(lambda (f) (funcall f v))
                '(<d-signal>-num <d-signal>-id <d-signal>-description))))