;;;; ./src/org/org.lisp

(defpackage #:recoder/org
  (:use #:cl
        )
  (:nicknames "R/ORG")
  (:export a-signals
           d-signals
           header))

(in-package :recoder/org)

(defmethod header ((trd r/trd:<trd>))
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
    (push (list "Файл" (r/trd:<trd>-file-name trd )) rez)
    (when (r/trd:<trd>-file-descr trd)
      (progn
	(push (list "Версия тренда" 	                  (r/trd:<trd>-version               trd)) rez)
	(push (list "Дата создания тренда"                (mnas-string/print:date (r/trd:<trd>-utime-start trd) :stream nil)) rez)
	(push (list "Время создания тренда"               (mnas-string/print:day-time (r/trd:<trd>-utime-start trd) :stream nil)) rez)
	(push (list "К-во аналоговых+дискретных сигналов" (r/trd:<trd>-reserv                trd)) rez)
	(push (list "Общее число записей в тренде"        (r/trd:<trd>-records               trd)) rez)
	(push (list "Интервал между записями тренда"      (r/trd:<trd>-increment             trd)) rez)
	(push (list "Количество аналоговых сигналов"      (r/trd:<trd>-a-number trd)) rez)
	(push (list "Количество дискретных сигналов"      (r/trd:<trd>-d-number trd)) rez)))
    (nreverse rez)))

(defmethod a-signals ((trd r/trd:<trd>))
  "@b(Описание:) метод @b(a-signals) возврвщает 2d-list список,
 отображающий все аналоговые сигналы в удобном виде для представления
 в ORG-режиме редактора Emacs.

 @b(Пример использования:)
@begin[lang=org](code)
#+begin_src lisp
(recoder/org:header recoder/trd:*trd*)
;;(recoder/org:a-signals recoder/trd:*trd*)
;;(recoder/org:d-signals recoder/trd:*trd*)
#+end_src
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (a-signals *trd*) 
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
  (loop :for k :being :the hash-key :using (hash-value v) :of (r/trd:<trd>-analog-ht trd)
	:collect
        (mapcar #'(lambda (f) (funcall f v))
                '(r/a-sig:<a-signal>-num r/a-sig:<a-signal>-id
                  r/a-sig:<a-signal>-min r/a-sig:<a-signal>-max
                  r/a-sig:<a-signal>-units r/a-sig:<a-signal>-description))))

(defmethod d-signals ((trd r/trd:<trd>))
  "@b(Описание:) метод @b(d-signals) возврвщает 2d-list список,
отображающий все дискретные сигналы в удобном виде для представления в org режиме.

 @b(Пример использования:)
@begin[lang=org](code)
#+begin_src lisp
(recoder/org:d-signals recoder/trd:*trd*)
#+end_src
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (d-signals *trd*) 
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
  (loop :for k :being :the hash-key :using (hash-value v) :of (r/trd:<trd>-discret-ht trd)
	:collect
                (mapcar #'(lambda (f) (funcall f v))
                '(r/d-sig:<d-signal>-num r/d-sig:<d-signal>-id r/d-sig:<d-signal>-description))))
