;;;; cp866.lisp

(in-package #:recoder/binary)

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

(export '*cp866*)

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