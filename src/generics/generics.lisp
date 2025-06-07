;;;; ./src/defgeneric/defgeneric.lisp

(defpackage #:recoder/generics
  (:use #:cl)
  (:nicknames "R/G")
  (:export read-obj write-obj)
  (:documentation
   "@b(Описание:) пакет @b(recoder/generics) определяет обобщенные функции."))

(in-package #:recoder/generics)

(defgeneric read-obj (d-signal stream)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(read-obj) считывание данных о
сигнале @b(d-signal) из потока."))

(defgeneric write-obj (d-signal stream)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(read-obj) считывание данных о
сигнале @b(d-signal) из потока."))
