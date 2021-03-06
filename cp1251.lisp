;;;; cp1251.lisp

(in-package #:recoder)

;;; "recoder" goes here. Hacks and glory await!

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
  (mapc #'(lambda (el)
	    (setf (gethash i *cp1251*) el)
	    (setf i (1+ i)))  *ascii-sym*))

(let ((i #x80))
  (mapc #'(lambda (el)
	    (setf (gethash i *cp1251*) el)
	    (setf i (1+ i)))  *cp1251-sym*))
