;;;; ./src/trd/read-obj-pathname.lisp

(in-package :recoder/trd)

(defun a-singal-name-unit-description (string)
  (multiple-value-bind (whole m1)
      (ppcre:scan-to-strings
       "^([^:]*):?([^,]*),?(.*)$" string)
    (loop :for i :across m1
          :collect (string-trim " " i))))

(defun a-singal-name-unit-description (string)
  (let ((m1 (nth-value 1 (ppcre:scan-to-strings
                          "^([^:]*):?([^,]*),?(.*)$" string))))
    (loop :for i :across m1
          :collect (string-trim " " i))))

(defun read-double-from-sring (str)
  (cond ((string= (string-trim " " str) "") 0.0d0)
        (t (coerce (mnas-string/parse:read-number str 0.0d0)
                   'double-float))))

(defun signal-detect-range (a-signal value)
  (when (< value (r/a-sig:<a-signal>-min a-signal))
    (setf (r/a-sig:<a-signal>-min a-signal) value))
  (when (> value (r/a-sig:<a-signal>-max a-signal))
    (setf (r/a-sig:<a-signal>-max a-signal) value))
  a-signal)


(defun read-record-line (line a-signal-number)
  (loop :for str :in (cdddr (ppcre:split #\Tab line))
        :for i :from 0 :below a-signal-number
        :collect (read-double-from-sring str)))

(defun read-signal-list (file-name)
  "Чтение сигналов тренда"
  (with-open-file (in file-name :external-format :utf-16le )
    (let* ((names-256 (ppcre:split #\Tab (read-line in)))
           (cols (position-if
                  #'(lambda (el)
                      (or (string= el "") (string= el "")))
                  names-256)))
      (loop :for name :in (cdddr names-256)
            :for n :from 0 :below (- cols 3)
            :collect
            (let ((a-signal (make-instance 'r/a-sig:<a-signal>))
                  (name-3 (a-singal-name-unit-description name)))
              (setf  (r/a-sig:<a-signal>-num a-signal) n)
              (setf  (r/a-sig:<a-signal>-id  a-signal) (first name-3))
              (setf  (r/a-sig:<a-signal>-description  a-signal) (second name-3))
              (setf  (r/a-sig:<a-signal>-units  a-signal) (third name-3))
              a-signal)))))

(defun signal-min-max-detect (file-name)
  "Определение минимального и максимального значения сигналов тренда.
Возвращает список сигналов."
  (let* ((a-signals (read-signal-list file-name))
         (a-signal-number (length a-signals )))
    (with-open-file (in file-name :external-format :utf-16le)
      (read-line in) ;; Пропускаем первую строку заголовков
      (loop :for line = (read-line in nil nil)
            :while line
            :do
               (map 'nil
                    #'signal-detect-range
                    a-signals
                    (read-record-line line a-signal-number))))
    (mapcar #'r/a-sig:correct-range a-signals)
    a-signals))

(defun signal-encode-to-stream (file-name trd out)
  (with-open-file (in file-name :external-format :utf-16le)
    (read-line in) ;; Пропускаем первую строку заголовков
    (let ((a-signals (a-signal-list trd))
          (i 0)
          )
      (loop :for line = (read-line in nil nil)
            :while (and line
                        (< 0 (length
                              (string-trim '(#\Space #\Tab #\Return) line))))
            :do
               (incf i)
               (mapcar
                #'(lambda (value a-signal)
                    (recoder/binary:b-write-ushort
                     (r/a-sig:encode-value value a-signal)
                     out))
                (read-record-line line (<trd>-a-number trd))
                a-signals))
      out)))





(defmethod r/g:read-obj ((trd <trd>) (file-path pathname))
  (let ((a-signals (signal-min-max-detect file-path)))
    (setf (<trd>-a-number trd) (length a-signals))
    (setf (<trd>-id-string trd) "TREND")
    (setf (<trd>-version   trd) 2)
    (setf (<trd>-d-number   trd) 0)
    (setf (<trd>-discret-ht trd)
          (make-hash-table :test #'equal :size (<trd>-d-number trd)))
    (setf (<trd>-reserv trd) (<trd>-a-number trd))
    (setf (<trd>-records trd) 0)
    (block analog-ht
      (setf (<trd>-analog-ht trd)
            (make-hash-table :test #'equal :size (<trd>-a-number trd)))
      (loop :for i :from 0
            :for a-signal :in a-signals
            :do (setf (r/a-sig:<a-signal>-num a-signal) i)
                (setf (gethash (r/a-sig:<a-signal>-id a-signal) (<trd>-analog-ht trd))
                      a-signal)))
    (block encode-a-signal-to-stream
      (with-open-stream (out (trivial-octet-streams:make-octet-output-stream))
        (signal-encode-to-stream file-path trd out)
        (let ((buffer (trivial-octet-streams:get-output-stream-octets out)))
          (setf (<trd>-records trd) (/ (length buffer) (<trd>-a-number trd) 2))
          (setf (<trd>-oc-i-sream trd)
                (trivial-octet-streams:make-octet-input-stream buffer))
          buffer
          )))))
