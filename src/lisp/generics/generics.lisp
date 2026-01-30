;;;; ./src/defgeneric/defgeneric.lisp

(defpackage #:recoder/generics
  (:use #:cl)
  (:nicknames "R/G")
  (:export read-obj write-obj)
  (:export a-signal-list
           d-signal-list
           )
  (:export decode-value
           encode-value
           )
  (:export signal-value
           )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric a-signal-list (trd)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(a-signal-list) возвращает список
аналоговых сигналов объекта @b(trd)."))

(defgeneric d-signal-list (trd)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(d-signal-list) возвращает список
аналоговых сигналов объекта @b(trd)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric decode-value (encoded-value a-signal)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(decode-value) выполняет
раскодирование аналогового сигнала, представленного закодированным
значением @b(encoded-value) по правилу, основанному на характеристиках
дескриптора @b(a-signal)."))

(defgeneric encode-value (decoded-value a-signal)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(encode-value) выполняет
кодирование аналогового сигнала, представленного раскодированным
значением @b(decoded-value) по правилу, основанному на характеристиках
дескриптора @b(a-signal)."))

(defgeneric signal-value (trd record x-signal)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(signal-value) возвращает значение
сигнала @b(x-signal), распложенного в записи или на момент времени
@b(record) из тренда @b(trd) дескриптора @b(a-signal)."))
