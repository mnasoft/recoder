;;;; package.lisp

(defpackage #:recoder/binary
  (:use #:cl) ;; #:mnas-string
  (:export *cp1251*
	   *cp866*
           *ascii-sym*
	   )
  (:export list-to-int
           open-trd-file-read
           open-trd-file-write
           )
  (:export read-trd-file-long
	   read-trd-file-long-long
	   read-trd-file-float
	   read-trd-file-int
	   read-trd-file-double
           ;; read-trd-file-quad
	   read-trd-file
	   read-trd-file-short
	   )
  (:intern int-to-list
           )
  (:export write-trd-file-int
	   write-trd-file-long
	   write-trd-file-float
	   write-trd-file
	   write-trd-file-long-long
	   write-trd-file-double
           ;; write-trd-file-quad
	   write-trd-file-short
	   ))

;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(in-package #:recoder/binary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ascii-sym*
  '("NUL" 	"SOH" 	"STX" 	"ETX" 	"EOT" 	"ENQ" 	"ACK" 	"BEL" 	"BS" 	"HT" 	"LF" 	"VT" 	"FF" 	"CR" 	"SO" 	"SI"
    "DLE" 	"DC1" 	"DC2" 	"DC3" 	"DC4" 	"NAK" 	"SYN" 	"ETB" 	"CAN" 	"EM" 	"SUB" 	"ESC" 	"FS" 	"GS" 	"RS" 	"US"
    " "    	"!" 	"\"" 	"#" 	"$" 	"%" 	"&" 	"'" 	"(" 	")" 	"*" 	"+" 	"," 	"-" 	"." 	"/"
    "0" 	"1" 	"2" 	"3" 	"4" 	"5" 	"6" 	"7" 	"8" 	"9" 	":" 	";" 	"<" 	"=" 	">" 	"?"
    "@" 	"A" 	"B" 	"C" 	"D" 	"E" 	"F" 	"G" 	"H" 	"I" 	"J" 	"K" 	"L" 	"M" 	"N" 	"O"
    "P" 	"Q" 	"R" 	"S" 	"T" 	"U" 	"V" 	"W" 	"X" 	"Y" 	"Z" 	"[" 	"\\" 	"]" 	"^" 	"_"
    "`" 	"a" 	"b" 	"c" 	"d" 	"e" 	"f" 	"g" 	"h" 	"i" 	"j" 	"k" 	"l" 	"m" 	"n" 	"o"
    "p" 	"q" 	"r" 	"s" 	"t" 	"u" 	"v" 	"w" 	"x" 	"y" 	"z" 	"{" 	"|" 	"}" 	"~" 	"DEL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter
    *cp866-sym*
  '("А" 	"Б"	"В"	"Г"	"Д"	"Е"	"Ж"	"З"	"И"	"Й"	"К"	"Л"	"М"	"Н"	"О"	"П"
    "Р" 	"С"	"Т"	"У"	"Ф"	"Х"	"Ц"	"Ч"	"Ш"	"Щ"	"Ъ"	"Ы"	"Ь"	"Э"	"Ю"	"Я"
    "а" 	"б" 	"в" 	"г" 	"д" 	"е" 	"ж" 	"з" 	"и" 	"й" 	"к" 	"л" 	"м" 	"н" 	"о" 	"п"
    "░" 	"▒" 	"▓" 	"│" 	"┤" 	"╡" 	"╢" 	"╖" 	"╕" 	"╣" 	"║" 	"╗" 	"╝" 	"╜" 	"╛" 	"┐"
    "└" 	"┴" 	"┬" 	"├" 	"─" 	"┼" 	"╞" 	"╟" 	"╚" 	"╔" 	"╩" 	"╦" 	"╠" 	"═" 	"╬" 	"╧"
    "╨" 	"╤" 	"╥" 	"╙" 	"╘" 	"╒" 	"╓" 	"╫" 	"╪" 	"┘"	"┌"	"█" 	"▄" 	"▌" 	"▐" 	"▀"
    "р" 	"с" 	"т" 	"у" 	"ф" 	"х" 	"ц" 	"ч" 	"ш" 	"щ" 	"ъ" 	"ы" 	"ь" 	"э" 	"ю" 	"я"
    "Ё" 	"ё" 	"Є" 	"є" 	"Ї" 	"ї" 	"Ў" 	"ў" 	"°" 	"∙" 	"·" 	"√" 	"№" 	"¤" 	"■" 	" "))

(defparameter *cp866* (make-hash-table))

(let ((i 0))
  (mapc
   #'(lambda (el)
       (setf (gethash i *cp866*) el)
       (setf i (1+ i)))
   *ascii-sym*))

(let ((i #x80))
  (mapc
   #'(lambda (el)
       (setf (gethash i *cp866*) el)
       (setf i (1+ i)))
   *cp866-sym*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter
    *cp1251-sym*
  '("Ђ"  	"Ѓ" 	"‚" 	"ѓ" 	"„" 	"…" 	"†" 	"‡" 	"€" 	"‰" 	"Љ" 	"‹" 	"Њ" 	"Ќ" 	"Ћ" 	"Џ"
    "ђ" 	"‘" 	"’" 	"“" 	"”" 	"•" 	"–" 	"—"     " "  	"™" 	"љ" 	"›" 	"њ" 	"ќ" 	"ћ" 	"џ"
    "Ў" 	"ў" 	"Ј" 	"¤" 	"Ґ" 	"¦" 	"§" 	"Ё" 	"©" 	"Є" 	"«" 	"¬" 	" "     "­" 	"®" 	"Ї"
    "°" 	"±" 	"І" 	"і" 	"ґ" 	"µ" 	"¶" 	"·" 	"ё" 	"№" 	"є" 	"»" 	"ј" 	"Ѕ" 	"ѕ" 	"ї"
    "А" 	"Б"	"В"	"Г"	"Д"	"Е"	"Ж"	"З"	"И"	"Й"	"К"	"Л"	"М"	"Н"	"О"	"П"
    "Р" 	"С"	"Т"	"У"	"Ф"	"Х"	"Ц"	"Ч"	"Ш"	"Щ"	"Ъ"	"Ы"	"Ь"	"Э"	"Ю"	"Я"
    "а" 	"б" 	"в" 	"г" 	"д" 	"е" 	"ж" 	"з" 	"и" 	"й" 	"к" 	"л" 	"м" 	"н" 	"о" 	"п"
    "р" 	"с" 	"т" 	"у" 	"ф" 	"х" 	"ц" 	"ч" 	"ш" 	"щ" 	"ъ" 	"ы" 	"ь" 	"э" 	"ю" 	"я"))

(defparameter *cp1251* (make-hash-table))

(let ((i 0))
  (mapc
   #'(lambda (el)
       (setf (gethash i *cp1251*) el)
       (setf i (1+ i)))
   *ascii-sym*))

(let ((i #x80))
  (mapc
   #'(lambda (el)
       (setf (gethash i *cp1251*) el)
       (setf i (1+ i)))
   *cp1251-sym*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-int (list-of-int)
  "@b(Описание:) list-to-int выполняет преобразование списка целых чисел 
находящихся в диапазоне 0 - 255 в целое число"
  (do ((i 0 (+ i 8))
       (lst list-of-int (cdr lst))
       (rez 0))
      ((null lst) rez)
    (setf (ldb (byte 8 i) rez) (car lst))))

(defun open-trd-file-read (path)
  "@b(Описание:) open-trd-file-read выполняет открытие файла тренда."
  (open path :element-type 'unsigned-byte))

(defun read-trd-file (in byte-number)
  "Выполняет чтение bite-number из потока in"
  (let ((lst nil)
	(bt nil))
    (dotimes (i byte-number)
      (setf bt (read-byte in nil 'eof))
      (if (eq bt 'eof)
	  (return-from read-trd-file  (values (nreverse lst) i nil))
	  (push bt lst)))
    (values (nreverse lst) byte-number t)))

(defun read-trd-file-short(in &optional (len 2))
  "Выполняет чтение short из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-int(in &optional (len 4))
  "Выполняет чтение int из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-long(in &optional (len 4))
  "Выполняет чтение long из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun read-trd-file-long-long(in &optional (len 8))
  "Выполняет чтение long-long из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (values   (list-to-int rez) n file-stastus)))

(defun read-trd-file-float(in &optional (len 4))
  "Выполняет чтение float из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
;;;;	(values (ie3fp:decode-ieee-float (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float32 (list-to-int rez)) n file-stastus)	
	(values 0 n file-stastus))))

(defun read-trd-file-double(in &optional (len 8))
  "Выполняет чтение doudle из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
        #+nil
	(values (ie3fp:decode-ieee-double (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float64 (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))
#+nil
(defun read-trd-file-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
  (multiple-value-bind (rez n file-stastus)
      (read-trd-file in len)
    (if file-stastus
	(values (ie3fp:decode-ieee-quad (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun int-to-list(int-val len)
  "Выполняет преобразование целого числа в список целых 
чисел, находящихся в диапазоне от 0 до 255"
  (let ((bbb nil))
    (dotimes (i len (reverse bbb))
      (push (ldb (byte 8  (* 8 i)) int-val) bbb))))

(export 'open-trd-file-write )

(defun open-trd-file-write (path)
  "Выполняет открытие файла тренда"
  (open path :element-type 'unsigned-byte :direction :io :if-exists :overwrite))

(export 'write-trd-file )

(defun write-trd-file (byte-list out &optional (byte-number (length  byte-list)))
  "Выполняет запись  bite-number элементов списка byte-list в поток out"
  (dotimes (i byte-number)
    (write-byte (pop byte-list ) out)))

(defun write-trd-file-short(int-val out &optional (len 2))
  "Выполняет запись short в поток out"
  (write-trd-file (int-to-list int-val len) out len))

(defun write-trd-file-int(int-val out &optional (len 4))
  "Выполняет запись int в поток out"
  (write-trd-file (int-to-list int-val len) out len))

(defun write-trd-file-long (int-val out &optional (len 4))
  "Выполняет запись long в поток out"
  (write-trd-file (int-to-list int-val len) out len))

(defun write-trd-file-long-long (int-val out &optional (len 8))
  "Выполняет чтение long-long из потока in"
  (write-trd-file (int-to-list int-val len) out len))

(defun write-trd-file-float (val out &optional (len 4))
  "Выполняет чтение float из потока in"
  (write-trd-file
   (int-to-list
;;;;    (ie3fp:encode-ieee-float (coerce val 'float))
    (ieee-floats:encode-float32 (coerce val 'float))    
    len)
   out
   len))

(defun write-trd-file-double (val out &optional (len 8))
  "Выполняет чтение doudle из потока in"
  (write-trd-file
   (int-to-list
;;;;(ie3fp:encode-ieee-double (coerce val 'double-float))
    (ieee-floats:encode-float64 (coerce val 'double-float))
    len)
   out
   len))

;(defun write-trd-file-quad (val out &optional (len 16))
;  "Выполняет чтение quad из потока in"
;  (write-trd-file
;   (int-to-list
;    (ie3fp:encode-ieee-quad
;     (coerce val 'long-float))
;    len)
;   out
;   len))
