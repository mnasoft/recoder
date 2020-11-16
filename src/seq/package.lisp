;; ./src/seq/package.lisp

(defpackage #:recoder/seq
  (:use #:cl
        #:mnas-string
        #:recoder/binary
        #:recoder/d-signal
        #:recoder/a-signal)
  (:export <trd-seq>
           <trd-seq>-a-sig
           <trd-seq>-d-sig
           <trd-seq>-s-sig
           trd-open
           *csv-stream*
           ))
