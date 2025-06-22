;;;; ./src/defgeneric/defgeneric.lisp

(defpackage #:recoder/generics
  (:use #:cl)
  (:nicknames "R/G")
  (:export read-obj write-obj)
  (:documentation
   "@b(Описание:) пакет @b(recoder/generics) определяет обобщенные функции."))

(in-package #:recoder/generics)

(defgeneric read-obj (obj stream_or_pathname)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(read-obj) чтение данных об объекте
@b(obj) из потока или файл @b(stream_or_pathname)."))

(defgeneric write-obj (obj stream_or_pathname)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(write-obj) записи данных объекта
@b(obj) в поток или файл @b(stream_or_pathname)."))
