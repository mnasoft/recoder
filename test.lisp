;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid-values-by-snames (dir-name utime snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-mid-values-by-snames (get-trd-by-utime-dirname utime dir-name :extension extension)
			    utime snames :n-before n-before :n-after n-after))

(defun stddev-values-by-snames ( dir-name udate snames &key (extension "trd") (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
  (trd-stddev-values-by-snames   (get-trd-by-utime-dirname utime dir-name :extension extension)
				 utime snames :n-before n-before :n-after n-after))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (get-trd-by-utime-dirname (time-universal-encode 2017 02 20 16 27 15) "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" :extension "trd")

;;;; (get-trd-by-utime-dirname (time-universal-encode 2017 01 23 16 27 15) "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" :extension "trd")

;;;; (mid-values-by-snames "d:/PRG/msys32/home/namatv/develop/TRD/DM80L№1-100-10/CPIPES/" utime snames )

;;;; (get-trd-by-utime-dirname (time-universal-encode 2017 03 09 13 39 30) "d:/home/_namatv/_WorkPlan/2017/80/ОТ_ЖАКИ.102.025-2017/TRD_ДМ80-10-100/ЦПиПЭС/TRD/Переходы" :extension "trd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(get-open-ternd)
(defparameter *trd* (make-instance 'trd :trd-file-name (get-open-ternd)))
(trd-open *trd*)
(make-instance 'trd :trd-file-name "D:/home/_namatv/_WorkPlan/2018/80/ЖГУ/Испытания/2018-11-06_092329.trd")
(trd-open *trd*)

