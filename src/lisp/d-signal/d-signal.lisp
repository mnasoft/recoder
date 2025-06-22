;;;; d-signal-defmethods.lisp

(defpackage #:recoder/d-signal
  (:use #:cl)
  (:nicknames "R/D-SIG")
  )

(in-package :recoder/d-signal)

(defmethod print-object ((x r/c:<d-signal>) stream)
  (print-unreadable-object (x stream)
    (format stream "~3D ~10S ~S"
            (r/c:<d-signal>-num x)
            (r/c:<d-signal>-id x)
            (r/c:<d-signal>-description x))))

(defmethod r/g:read-obj ((d-signal r/c:<d-signal>) in)
  (setf (r/c:<d-signal>-id d-signal)
        (m-bin:b-read-string in r/const:+signal-id-wid+))
  (setf (r/c:<d-signal>-description d-signal)
        (m-bin:b-read-string in r/const:+signal-description-wid+))
  d-signal)

(defmethod r/g:write-obj ((d-signal r/c:<d-signal>) out)
  (m-bin:b-write-string (r/c:<d-signal>-id d-signal) out
                        r/const:+signal-id-wid+)
  (m-bin:b-write-string (r/c:<d-signal>-description d-signal) out
                        r/const:+signal-description-wid+)
  d-signal)

