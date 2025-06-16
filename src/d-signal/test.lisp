(in-package :recoder/d-signal)

(defparameter *d*
  (make-instance '<d-signal>
                 :id "FH10"
                 :description "Клапан FH10 положение и нечто 12345678901234567890"))
(defparameter *d1* (make-instance '<d-signal>))

(with-open-file (out "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :output
                     :if-exists :supersede)
  (r/g:write-obj *d* out))

(with-open-file (in "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :input)
  (file-length in))

(with-open-file (in "/home/mna/123321.bin"
                     :element-type 'unsigned-byte
                     :direction :input)
  (r/g:read-obj *d1* in))
