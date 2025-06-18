;;;; ./src/defgeneric/defgeneric.lisp

(defpackage #:recoder/generics
  (:use #:cl)
  (:nicknames "R/G")
  (:export read-obj write-obj)
  (:documentation
   "@b(Описание:) пакет @b(recoder/generics) определяет обобщенные функции."))

(in-package #:recoder/generics)

(defgeneric read-obj (obj stream)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(read-obj) чтение данных об объекте
@b(obj) из потока @b(stream)."))

(defgeneric write-obj (obj stream)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(write-obj) записи данных объекта
@b(obj) в поток @b(stream)."))
