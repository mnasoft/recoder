;;;; ./recoder/src/constants/constants.lisp

(defpackage #:recoder/constants
  (:use #:cl)
  (:nicknames "R/C")
  (:export +signal-id-wid+              
           +signal-description-wid+     
           +signal-units-wid+           
           )
  (:export +ushort-max+)
  (:documentation
   "@b(Описание:) пакет @b(recoder/constants) recoder/constants константы."))

(in-package :recoder/constants)

(defconstant +ushort-max+ (- (expt 2 16) 1)
  "Количество градаций аналогового сигнала от 0 до +ushort-max+ при
 записи тренда")

  
(defconstant +signal-id-wid+ 10
  "Длина строки обозначения сигнала, char[10]")

(defconstant +signal-description-wid+ 40
  "Длина строки описания сигнала, char[40] ")

(defconstant +signal-units-wid+ 8
  "Длина строки размерности аналогового сигнала, char[8]")
