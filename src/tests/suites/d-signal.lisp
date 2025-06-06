(in-package :recoder/tests)

(def-suite d-signal
  :description "Мастер-набор всех тестов проекта recoder/d-signal."
  :in all)

(in-suite d-signal)

(defparameter *d-signal*
  (let ((num 218)
        (id "PH010")
        (description "Положение клапан PH01"))
    (make-instance
     'r/d-sig:<d-signal> :num num :id id :description description)))

(def-fixture fix-d-signal (num id description)
  (let ((d-signal (make-instance 'r/d-sig:<d-signal>
                                 :num num
                                 :id id
                                 :description description
                                 )))
    (&body)))

(def-test d-signal-slots ()
  (with-fixture fix-d-signal (218 "PH01" "Положение клапан PH01")
    (is-true (= num (r/d-sig:<d-signal>-num d-signal)))
    (is-true (string= id (r/d-sig:<d-signal>-id d-signal)))
    (is-true (string= description (r/d-sig:<d-signal>-description d-signal)))
    (is-true (stringp (format nil "~%~A" d-signal)))))

(def-test d-signal-slots-change ()
  (with-fixture fix-d-signal (218 "PH01" "Положение клапан PH01")
    (progn
      (setf (r/d-sig:<d-signal>-num d-signal) 42)
      (setf (r/d-sig:<d-signal>-id d-signal) "FH03")
      (setf (r/d-sig:<d-signal>-description d-signal) "Положение клапан FH03")
    
    (is-true (= 42 (r/d-sig:<d-signal>-num d-signal)))
    (is-true (string= "FH03" (r/d-sig:<d-signal>-id d-signal)))
    (is-true (string= "Положение клапан FH03" (r/d-sig:<d-signal>-description d-signal)))
    (is-true (stringp (format nil "~%~A" d-signal))))))
