;;;; package.lisp

(defpackage #:recoder/d-signal
  (:use #:cl)
  ;; #:mnas-string #:recoder/binary
  (:export <d-signal>
	   d-signal-description
	   d-signal-id
           d-signal-num)
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
