;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@cl:with-package[name="cl-user"](
@cl:doc(method print-object (x trd) stream)
)

@cl:with-package[name="recoder"](



)

@end(section)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid-values-by-snames (dir-name utime snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-mid-values-by-snames (find-trd-by-utime-dirname utime dir-name :extension extension)
			    utime snames :n-before n-before :n-after n-after))

(defun stddev-values-by-snames ( dir-name udate snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-stddev-values-by-snames   (find-trd-by-utime-dirname utime dir-name :extension extension)
				 utime snames :n-before n-before :n-after n-after))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (find-trd-by-utime-dirname (time-universal-encode 2017 02 20 16 27 15) "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" :extension "trd")

;;;; (find-trd-by-utime-dirname (time-universal-encode 2017 01 23 16 27 15) "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" :extension "trd")

;;;; (mid-values-by-snames "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" utime snames )

;;;; (find-trd-by-utime-dirname (time-universal-encode 2017 03 09 13 39 30) "d:/home/_namatv/_WorkPlan/2017/80/ОТ_ЖАКИ.102.025-2017/TRD_ДМ80-10-100/ЦПиПЭС/TRD/Переходы" :extension "trd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *trd* (make-instance 'trd :trd-file-name "D:/home/_namatv/_WorkPlan/2018/80/ЖГУ/Испытания/2018-11-06_092329.trd"))

(trd-open *trd*)
  
"ET001""ET002" "ET003" "ET004" "ET005" "ET006" "ET007" "ET008" "ET009" "ET010" "ET011" "ET012" "ET013" "ET014" "ET015" "ET016" "ET017" "ET018" "ET019" "ET020" "ET021" "ET022" "ET023" "ET024" "ET025"

"FG010" "FP030" "PP010"

"OIL" "GAS"
