;;;; test.lisp

(in-package #:recoder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-analog-ht *t*) )

;;;; (maphash #'(lambda (k v) (format t "~S~%" v)) (trd-discret-ht *t*) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (trd-get-analog-signal-number-list *t* 21 '("EN1" "T04mid"))

;;;; (a-signal-value (gethash "EN1" (trd-analog-ht *t*)) (* *ushort-max* 0.0125))

;;;; (trd-total-records *t*)

;;;; (file-position (trd-file-descr *t*))

;;;; (trd-start-offset *t*)

;;;; (trd-record-length *t*)

;;;; (trd-total-records-number *t*)

;;;; (trd-record-length *t*)

;;;; (file-length (trd-file-descr *t*))

;;;; (trd-total-records-number *t*)

;;;; (trd-start-offset *t*)

;;;; (trd-total-records *t*)
