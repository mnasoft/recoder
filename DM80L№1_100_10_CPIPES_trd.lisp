;;;; DM80L№1_100_10_CPIPES_trd.lisp


(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cpipes-signal-name*
  '("GQ010" "EN1" "EN2" 
    "T04" "dT04plus" "dT04minus"
    "T01"
    "P02" 
    "FT020" "FP210" "FP220" "FA020" "FQ110"

    "FT010" "FP230" "FA010" "FQ010"
    "PT240" "PT230" "PT250" 
				  
    "EB100" "EB110" "EB120"
    "FP450" "FP460" "FP470" 
    )
  "Имена сигналов, записываемых ЦПиПЭС")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"Перед выполнением нижеследующего необходимо выполнить содержимое файла DM80L№1_100_10_trd-signal-oboznachenie.lisp"

(make-html-trd "~/develop/TRD/DM80L№1-100-10/CPIPES/20170123_085545.trd"
	       "~/rez-cpipes-dt-20170123_085545.html"
	       *cpipes-signal-name* *dt-time*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-html-trd "~/develop/TRD/DM80L№1-100-10/CPIPES/20170126_114520.trd"
	       "~/rez-cpipes-dt-20170126_114520.html"
	       *cpipes-signal-name* *gt-time*)
