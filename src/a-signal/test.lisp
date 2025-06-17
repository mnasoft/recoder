(in-package :recoder/a-signal)

(defparameter *a* (make-instance 'r/a-sig:<a-signal>
                                   :id "GA-GTD"
                                   :description "Массовый расход воздуха на входе в ГТД"
                                   :units "кг/с"
                                   :min 0.0d0
                                   :max 150.0))

(with-open-file (out "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :output
                     :if-exists :supersede)
  (r/bin:b-write-double 45.23d0 out)
  (r/bin:b-write-float  12.78d0 out)
  (r/g:write-obj *a* out))

(with-open-file (in "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :input)
  (file-length in))

(defparameter *a1* (make-instance 'r/a-sig:<a-signal>))

(with-open-file (in "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :input)
  (format t "~A~%" (r/bin:b-read-double in))
  (format t "~A~%" (r/bin:b-read-float  in))
  (r/g:read-obj *a1* in))
