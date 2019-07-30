;;;; defparameters.lisp

(in-package #:recoder)

(defparameter *head-id-wid*              5 "Строка идентификации файла тренда, char[5]")

(defparameter *head-version-wid*         1 "Версия данных тренда, char[1]")

(defparameter *head-date-wid*            3 "День Месяц Год-2000 char[3]")

(defparameter *head-time-wid*            3 "Час Минута Секунда char[3]")

(defparameter *head-wid*                30 "Общая длина заголовка, char[30]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defparameter *signal-id-wid*           10 "Длина строки обозначения сигнала, char[10]")

(defparameter *signal-description-wid*  40 "Длина строки описания сигнала, char[40] ")

(defparameter *signal-units-wid*         8 "Длина строки размерности аналогового сигнала, char[8]")

(defparameter *signal-LowLimit-wid*      8 "Ширина поля для нижней  границы диапазона аналогового сигнала, double = char[8]")

(defparameter *signal-HighLimit-wid*     8 "Ширина поля для верхней границы диапазона аналогового сигнала, double = char[8]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *analog-wid* (+ *signal-id-wid* *signal-description-wid* *signal-units-wid* *signal-LowLimit-wid* *signal-HighLimit-wid*)
  "Длина заголовка одной записи аналогового сигнала")
 
(defparameter *discret-wid* (+ *signal-id-wid* *signal-description-wid* )
  "Длина заголовка одной записи дискретного сигнала")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ushort-max* (- (expt 2 16) 1) "Количество градаций аналогового сигнала от 0 до *ushort-max* при записи тренда")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*mid-value-number-offset*)
(defparameter *mid-value-number-offset*  10 "Количество записей тренда отсчитываемое влево и вправо от текущей записи для определения среднего значения и стандартнорго отклонения" )
