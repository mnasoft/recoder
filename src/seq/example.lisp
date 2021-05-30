;;;; test.lisp

(in-package #:recoder/seq)

(defparameter *trd-fname* (concatenate 'string
				       (namestring (asdf:system-source-directory :recoder)) "trd" "/" "2018-11-06_092329.trd")
  "Для примеров.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *t-seq* (make-instance '<trd-seq> :trd-file-name *trd-fname*
                                                :s-sig '("V2" "ET300"))
  "Для примеров.")

(trd-open *t-seq*)

(<trd-seq>-a-sig *t-seq*)
(<trd-seq>-d-sig *t-seq*)
(<trd-seq>-s-sig *t-seq*)

(length *t-seq*)  ; => 15706 (14 bits, #x3D5A)

(find-if
 #'(lambda (el)
     (> (sig "ET300" el *t-seq*) 800.0 ))
 *t-seq*)

(position-if
 #'(lambda (el)
     (> (sig "ET300" el *t-seq*) 800.0 ))
 *t-seq*)

(position-if #'(lambda (el) (> el 700.0))
	     *t-seq*
	     :key #'(lambda (el) (sig "ET300" el *t-seq*))
	     :from-end t
	     :start 5000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *pulsation-template* '("EN1" "EN2" "EN3"  "EB060" "EB120" "EB130" "EB090" "T04" "Na"))

(extract-signals "~/org/troynich/20200907_090415.trd" *pulsation-template*)
(extract-signals "~/org/troynich/20200907_133300.trd" *pulsation-template*)

(extract-signals
 "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200806_151019.trd"
 *pulsation-template*)

(extract-signals
 "~/quicklisp/local-projects/ZM/PM/pm-237/trd-CPiPES/2020-per/20200814_132922.trd"
 *pulsation-template*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *trd-seq*
  (make-instance '<trd-seq>
                 :trd-file-name
                 "~/quicklisp/local-projects/clisp/recoder/trd/2018-11-06_092329.trd"
                 ))

(trd-open *trd-seq*)

(elt *trd-seq* 0)

(setf (<trd-seq>-s-sig  *trd-seq*)
      '("V2" "T2" "ET300"
        "ET001" "ET002" "ET003" "ET004" "ET005"
        "ET006" "ET007" "ET008" "ET009" "ET010"
        "ET011" "ET012" "ET013" "ET014" "ET015"
        "ET016" "ET017" "ET018" "ET019" "ET020"
        "ET021" "ET022" "ET023" "ET024" "ET025"
         ))

(<trd-seq>-a-sig *trd-seq*)

(elt *trd-seq* 0)


(position-if
 #'(lambda (el) (> (sig "ET300" el *trd-seq*) 800.0 ))
 *trd-seq*)

(elt *trd-seq* 5102)


     (> (sig "ET300" el *t-seq*) 800.0 ))
