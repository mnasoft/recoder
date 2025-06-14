;;;; package.lisp

(defpackage :recoder/binary
  (:use #:cl) 
  (:nicknames "R/BIN")
  (:export *cp1251*
	   *cp866*
           *ascii-sym*
	   )
  (:export open-b-read
           open-b-write
           )
  (:export with-open-file-b-out
           with-open-file-b-in)
  (:export b-read
           )
  (:export b-read-char           b-read-uchar
           b-read-short          b-read-ushort
           b-read-int            b-read-uint
           b-read-long           b-read-ulong
           b-read-long-long      b-read-ulong-long 
           )
  (:export b-read-float
           b-read-double
           #+nil b-read-quad
           )
  (:export b-read-string)
  (:export b-write)
  (:export b-write-char      b-write-uchar
           b-write-short     b-write-ushort
           b-write-int       b-write-uint
           b-write-long      b-write-ulong
           b-write-long-long b-write-ulong-long 
           )
  (:export b-write-float
           b-write-double
           #+nil b-write-quad
           )
  (:export b-write-string
	   )
  (:export decode-string
           list-to-int
           int-to-list
           )
  (:export *pangram-ru-1* *pangram-ru-2* *pangram-ru-3*
           *pangram-uk-1* *pangram-uk-2* *pangram-uk-3*)
  )

(in-package :recoder/binary)
