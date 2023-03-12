;;;; package.lisp

(defpackage :recoder/binary
  (:use #:cl) ;; #:mnas-string
  (:nicknames "R/BIN")
  (:export *cp1251*
	   *cp866*
           *ascii-sym*
	   )
  (:export open-b-read
           open-b-write
           )
  (:export b-read
           b-read-short
           b-read-int
           b-read-long
	   b-read-long-long
	   b-read-float
	   b-read-double
           #+nil b-read-quad
           b-read-string
	   )
  (:export b-write
           b-write-short
           b-write-int
           b-write-long
           b-write-long-long
	   b-write-float
	   b-write-double
           #+nil b-write-quad
           b-write-string
	   )
  (:export decode-string
           list-to-int
           int-to-list
           )
  (:export *pangram-ru-1* *pangram-ru-2* *pangram-ru-3*
           *pangram-uk-1* *pangram-uk-2* *pangram-uk-3*))

(in-package :recoder/binary)

(defparameter *pangram-ru-1* "Съешь же ещё этих мягких французских булок да выпей чаю.")
(defparameter *pangram-ru-2* "Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства.")
(defparameter *pangram-ru-3* "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!")

(defparameter *pangram-uk-1* "Чуєш їх, доцю, га? Кумедна ж ти, прощайся без ґольфів!")
(defparameter *pangram-uk-2* "Жебракують філософи при ґанку церкви в Гадячі, ще й шатро їхнє п'яне знаємо.")
(defparameter *pangram-uk-3* "Гей, хлопці, не вспію - на ґанку ваша файна їжа знищується бурундучком.")

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
  "@b(Описание:) функция @b(list-to-int) выполняет преобразование
списка целых чисел находящихся в диапазоне 0 - 255 в целое число.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-to-int '(2 8 0 0)) => 2050
 (list-to-int '(1 0)) => 1
 (list-to-int '(0 1)) => 256
@end(code)"
  (do ((i 0 (+ i 8))
       (lst list-of-int (cdr lst))
       (rez 0))
      ((null lst) rez)
    (setf (ldb (byte 8 i) rez) (car lst))))

(defun int-to-list (int-val len)
  "Выполняет преобразование целого числа в список целых 
чисел, находящихся в диапазоне от 0 до 255.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (int-to-list 2050 4) => (2 8 0 0)
@end(code)
"
  (let ((bbb nil))
    (dotimes (i len (reverse bbb))
      (push (ldb (byte 8  (* 8 i)) int-val) bbb))))

(defun open-b-read (path)
  "@b(Описание:) функция @b(open-b-read) выполняет открытие файла для
 бинарного чтения.

 Пример использования см. в описании к функции @b(open-b-write)."
  (open path :element-type 'unsigned-byte))

(defun b-read (in byte-number)
  "@b(Описание:) функция @b(b-read) выполняет чтение @b(byte-number)
 количества байт из бинарного потока @b(in)."
  (let ((lst nil)
	(bt nil))
    (dotimes (i byte-number)
      (setf bt (read-byte in nil 'eof))
      (if (eq bt 'eof)
	  (return-from b-read  (values (nreverse lst) i nil))
	  (push bt lst)))
    (values (nreverse lst) byte-number t)))

(defun b-read-short (in &optional (len 2))
  "@b(Описание:) функция @b(b-read-short) выполняет чтение
короткого (2 байта) беззнакового целого из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun b-read-int (in &optional (len 4))
  "@b(Описание:) функция @b(b-read-int) выполняет чтение беззнакового
 целого (4 байта) числа из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun b-read-long (in &optional (len 4))
  "@b(Описание:) функция @b(b-read-long) выполняет чтение длинного (4
 байта) беззнакового целого числа из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (list-to-int rez) n file-stastus)
	(values 0 n file-stastus))))

(defun b-read-long-long (in &optional (len 8))
  "@b(Описание:) функция @b(b-read-long-long) выполняет чтение очень
 длинного (8 байт) беззнакового целого числа из потока in, окрытого в
 двоичном режиме."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (values   (list-to-int rez) n file-stastus)))

(defun b-read-float (in &optional (len 4))
  "@b(Описание:) функция @b(b-read-float) выполняет чтение
 короткого (4 байта) числа с плавающей точкой из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
        #+nil
	(values (ie3fp:decode-ieee-float (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float32 (list-to-int rez)) n file-stastus)	
	(values 0 n file-stastus))))

(defun b-read-double (in &optional (len 8))
  "@b(Описание:) функция @b(b-read-double) выполняет чтение
 длинного (8 байт) числа с плавающей точкой из бинарного потока @b(in)."
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
        #+nil
	(values (ie3fp:decode-ieee-double (list-to-int rez)) n file-stastus)
	(values (ieee-floats:decode-float64 (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

#+nil
(defun b-read-quad (in &optional (len 16))
  "Выполняет чтение quad из потока in"
  (multiple-value-bind (rez n file-stastus)
      (b-read in len)
    (if file-stastus
	(values (ie3fp:decode-ieee-quad (list-to-int rez)) n file-stastus)
	(values 0 n file-stastus))))

(defun b-read-string (in byte-number &key (encoding :cp1251))
  "@b(Описание:) метод @b(b-read-string) возвращает строку,
считываемую из бинарного потока @b(in). При этом из потока считывается
количество байт @b(byte-number). Начальные и конечные нуль-символы из
строки исключаются. Символы декодируются из кодировки @b(encoding).

 @b(Пример использования:)
@begin[lang=lisp](code)
(progn
  (let ((buffer-length 900)
        (path
          (merge-pathnames
           #P\"trd/bin-01.bin\" 
           (asdf:system-source-directory :recoder/binary))))
    (let ((w (open-b-write path)))
      (b-write-string w *pangram-uk-1* buffer-length :external-format :KOI8-U)
      (close w))
    (let* ((r (open-b-read path))
           (rez (b-read-string r buffer-length :encoding :KOI8-U)))
      (close r)
      rez)))
@end(code)
"
  (string-trim `,(format nil "~A" #\Nul)
               (babel:octets-to-string
                (coerce (b-read in byte-number)
                        '(vector (unsigned-byte 8)))
                :encoding encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-b-write (path)
    "@b(Описание:) функция @b(open-b-read) выполняет открытие файла для
 бинарной записи.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((val 2050)
       (rez nil)
       (fname (merge-pathnames
                #P\"trd/binary.bin1\"
                (asdf:system-source-directory
                 (asdf:find-system \"recoder/binary\")))))
   (let ((out (open-b-write fname)))
     (b-write-short val out)
     (close out))
   (let ((in (open-b-read fname)))
     (setf rez (b-read-short in))
     (close in)
     rez))
 @end(code)
"
  (open path :element-type 'unsigned-byte :direction :output :if-exists :supersede))

(defun b-write (byte-list out &optional (byte-number (length  byte-list)))
  "@b(Описание:) функция @b(b-write) выполняет запись @b(bite-number)
 элементов списка byte-list в бинарный поток вывода @b(out)."
  (dotimes (i byte-number)
    (write-byte (pop byte-list ) out)))

(defun b-write-short (int-val out &optional (len 2))
  "@b(Описание:) функция @b(b-write-short) выполняет запись
короткого (2 байта) беззнакового целого в бинарный поток @b(out)."
  (b-write (int-to-list int-val len) out len))

(defun b-write-int (int-val out &optional (len 4))
  "@b(Описание:) функция @b(b-write-short) выполняет запись
короткого (4 байта) беззнакового целого в бинарный поток @b(out)."
  (b-write (int-to-list int-val len) out len))

(defun b-write-long (int-val out &optional (len 4))
  "@b(Описание:) функция @b(b-write-long) выполняет запись
длинного (4 байта) беззнакового целого в бинарный поток @b(out)."
  (b-write (int-to-list int-val len) out len))

(defun b-write-long-long (int-val out &optional (len 8))
  "@b(Описание:) функция @b(b-write-long-long) выполняет запись очень
длинного (8 байта) беззнакового целого в бинарный поток @b(out)."
  (b-write (int-to-list int-val len) out len))

(defun b-write-float (val out &optional (len 4))
  "@b(Описание:) функция @b(b-write-float) выполняет запись
короткого (4 байта) числа с плавающей точкой в бинарный поток @b(out)."
  (b-write
   (int-to-list #+nil (ie3fp:encode-ieee-float (coerce val 'float))
                (ieee-floats:encode-float32 (coerce val 'float))    
                len)
   out
   len))

(defun b-write-double (val out &optional (len 8))
  "@b(Описание:) функция @b(b-write-float) выполняет запись
длиного (8 байт) числа с плавающей точкой в бинарный поток @b(out)."
  (b-write
   (int-to-list #+nil (ie3fp:encode-ieee-double (coerce val 'double-float))
                (ieee-floats:encode-float64 (coerce val 'double-float))
                len)
   out
   len))

#+nil
(defun b-write-quad (val out &optional (len 16))
  "Выполняет чтение quad из потока in"
  (b-write
   (int-to-list (ie3fp:encode-ieee-quad (coerce val 'long-float)) len) out len))

;;; TODO - реализация пока применима только для однобайтных кодировок.
(defun b-write-string (out string byte-number &key (external-format :cp1251))
  "@b(Описание:) метод @b(b-write-string) выводит в поток @b(out)
строку @b(string). 

 @b(Переменые:)
@begin(list)
 @item(out - двоичныый поток вывода;)
 @item(string - строка, подлежащая выводу в поток;)
 @item(external-format - кодировка, в которой строка будет сохранена.
       Список кодировок доступный на данный момент можно получить при
       помощи вызова следующей функции:
       (babel:list-character-encodings);)
 @item(byte-number - количество байт до которых усекается строка.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((w (open-b-write 
          (merge-pathnames #P\"trd/bin-01.bin\" 
                           (asdf:system-source-directory :recoder)))))
  (b-write-string w *pangram-uk-1* :external-format :KOI8-U :byte-number 200)
  (close w))
@end(code)
"
  (let ((s-buffer (make-string byte-number :initial-element #\Nul)))
    (loop :for i :from 0 :below byte-number
          :for j :from 0 :below (length string) :do
      (setf (char s-buffer i) (char string j)))
    (b-write
     (babel-streams:with-output-to-sequence (o :return-as 'list :external-format external-format)
       (format o "~A" s-buffer))
     out)))

