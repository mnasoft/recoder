;;;; ./src/slist/slist.lisp

(defpackage #:recoder/slist
  (:use #:cl #:recoder/trd) 
  (:export trd-analog-signal-list
           trd-discret-signal-list
           trd-separate-not-signals))

(in-package #:recoder/slist)

(defmethod trd-separate-not-signals ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(trd-separate-not-signals) возвращает список
строк, которые для тренда trd не являются именами аналоговых или
дискреных сигналов."
  (when (<trd>-file-descr trd)
    (loop :for s-n :in signal-names
          :unless (or (gethash s-n (<trd>-analog-ht trd))
                      (gethash s-n (<trd>-discret-ht trd)))
            :collect s-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-analog-signal-list ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(trd-analog-signal-list) возвращает список
аналоговых сигналов тренда <trd>, которые соответствуют списку
обозначений сигналов из списка signal-names"
  (when (<trd>-file-descr trd)
    (loop :for s-n :in signal-names
      :when (gethash s-n (<trd>-analog-ht trd))
        :collect :it)))

(defmethod trd-discret-signal-list ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(trd-discret-signal-list) возвращает список
 дискретных сигналов тренда trd, которые соответствуют списку
 обозначений сигналов из списка строк @b(signal-names)."
  (when  (<trd>-file-descr trd)
        (loop :for s-n :in signal-names
      :when (gethash s-n (<trd>-discret-ht trd))
        :collect :it)))

#+nil
    (mapcar #'(lambda(el)
		(gethash el (<trd>-discret-ht trd)))
	    signal-names)
