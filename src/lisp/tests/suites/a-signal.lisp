(in-package :recoder/tests)

(def-suite a-signal
  :description "Мастер-набор всех тестов проекта recoder/a-signal."
  :in all)

(in-suite a-signal)

(defparameter *a-signal*
  (let ((num 115)
        (id "V2")
        (min 0.0)
        (max 25.0)
        (units "м3/с")
        (description "Объёмный расход через КС"))
    (make-instance
     'r/a-sig:<a-signal> :num num :id id :min min
     :max max :units units :description description)))

(def-fixture fix-a-signal (num id min max units description)
  (let ((a-signal (make-instance 'r/a-sig:<a-signal>
                                 :num num
                                 :id id
                                 :min min
                                 :max max
                                 :units units
                                 :description description
                                 )))
    (&body)))
#+nil
(defparameter *a* (make-instance 
                                  ))

;; encode-value
#+nil
(eval (cons 'and
       (loop :for i :from 0 :to *ushort-max*
             :collect (= i (encode-value (decode-value i *a*) *a*)))))

(def-test a-signal-slots ()
  (with-fixture fix-a-signal (115 "V2" -1.0  9.0 "м3/с" "Объёмный расход через КС")
    (is-true
     (= num (r/a-sig:<a-signal>-num a-signal)))
    (is-true (string= id (r/a-sig:<a-signal>-id a-signal)))
    (is-true (math/core:semi-equal min (r/a-sig:<a-signal>-min a-signal)))
    (is-true (math/core:semi-equal max (r/a-sig:<a-signal>-max a-signal)))
    (is-false (math/core:semi-equal min (r/a-sig:<a-signal>-max a-signal)))
    (is-true (string= units (r/a-sig:<a-signal>-units a-signal)))
    (is-true (string= description (r/a-sig:<a-signal>-description a-signal)))
    (is-true (stringp (format nil "~%~A" a-signal)))))

(def-test a-signal-slots-change ()
  (with-fixture fix-a-signal (115 "V2" -1.0  9.0 "м3/с" "Объёмный расход через КС")
    (progn
      (setf (r/a-sig:<a-signal>-num a-signal) 52)
      (setf (r/a-sig:<a-signal>-id a-signal) "T03")
      (setf (r/a-sig:<a-signal>-min a-signal) 0.0)
      (setf (r/a-sig:<a-signal>-max a-signal) 10.0)
      (setf (r/a-sig:<a-signal>-units a-signal) "C")
      (setf (r/a-sig:<a-signal>-description a-signal) "Температура за КС"))
    
    (is-true (= 52 (r/a-sig:<a-signal>-num a-signal)))
    (is-true (string= "T03" (r/a-sig:<a-signal>-id a-signal)))
    (is-true (math/core:semi-equal 0.0 (r/a-sig:<a-signal>-min a-signal)))
    (is-true (math/core:semi-equal 10.0 (r/a-sig:<a-signal>-max a-signal)))
    (is-true (string= "C" (r/a-sig:<a-signal>-units a-signal)))
    (is-true (string= "Температура за КС" (r/a-sig:<a-signal>-description a-signal)))
    (is-true (stringp (format nil "~%~A" a-signal)))))

(def-test a-signal-value ()
  (with-fixture fix-a-signal (115 "V2" 0.0  25.0 "м3/с" "Объёмный расход через КС")
    (is-true (= 0 (r/a-sig:encode-value 0.0 a-signal)))
    (is-true (= 0 (r/a-sig:encode-value -0.1 a-signal)))
    (is-true (= (1- (* 256 256)) (r/a-sig:encode-value 25.0 a-signal)))
    (is-true (= (1- (* 256 256)) (r/a-sig:encode-value 25.1 a-signal)))
    (is-true (= (round (/ (1- (* 256 256)) 2))
                (r/a-sig:encode-value 12.5 a-signal)))
    (is-true (= (round (* (1- (* 256 256)) 1/10))
                (r/a-sig:encode-value 2.5 a-signal)))
    (is-true (math/core:semi-equal
              (round (* (1- (* 256 256)) 9/10))
              (r/a-sig:encode-value 22.5 a-signal)
              :tolerance 1))
    (loop :for i :from 0 :to 100 :do
      (let ((j (random r/a-sig::*ushort-max*)))
               (is-true
                (= j
                   (r/a-sig:encode-value
                    (r/a-sig:decode-value j a-signal)
                    a-signal)))))))
