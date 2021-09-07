;;;; ./src/slist/slist.lisp

(defpackage #:recoder/slist
  (:use #:cl #:recoder/trd) 
  (:export a-signals
           d-signals
           not-signals)
  (:documentation
   "@b(Описание:) пакет @b(recoder/slist) предназначен для
 формирования списков сигналов аналоговых и дискретных.
"))

(in-package #:recoder/slist)

(defmethod not-signals ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(not-signals) возвращает список строк,
 которые для тренда trd не являются именами аналоговых или дискреных
 сигналов."
  (when (<trd>-file-descr trd)
    (loop :for s-n :in signal-names
          :unless (or (gethash s-n (<trd>-analog-ht trd))
                      (gethash s-n (<trd>-discret-ht trd)))
            :collect s-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod a-signals ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(a-signals) возвращает список аналоговых
 сигналов тренда <trd>, которые соответствуют списку обозначений
 сигналов из списка signal-names"
  (when (<trd>-file-descr trd)
    (loop :for s-n :in signal-names
      :when (gethash s-n (<trd>-analog-ht trd))
        :collect :it)))

(defmethod d-signals ((trd <trd>) signal-names)
  "@b(Описание:) метод @b(d-signals) возвращает список
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
