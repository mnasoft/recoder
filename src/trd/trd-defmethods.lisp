;;;; defmethods.lisp

(in-package #:recoder/trd)

(export '(<trd> trd-file-name trd-file-descr trd-id-string trd-version
          trd-utime-start trd-reserv trd-total-records trd-delta-time
          trd-analog-number trd-discret-number trd-analog-ht trd-discret-ht))

(defclass <trd> ()
  ((trd-file-name         :accessor trd-file-name      :initarg :trd-file-name      :initform nil :documentation "Имя файла в файловой системе")
   (trd-file-descr        :accessor trd-file-descr                                  :initform nil :documentation "Файл тренда")
   (trd-id-string         :accessor trd-id-string                                   :initform nil :documentation "Строка идентифицирующая то, что это файл тренда")
   (trd-version           :accessor trd-version                                     :initform nil :documentation "Версия тренда")
   (trd-utime-start       :accessor trd-utime-start                                 :initform nil :documentation "Дата и время начала создания тренда в универсальном формате")
   (trd-reserv            :accessor trd-reserv                                      :initform nil :documentation "Количество аналоговых сигналов + Количество дискретных сигналов")
   (trd-total-records     :accessor trd-total-records                               :initform nil :documentation "Общее число записей в тренде")
   (trd-delta-time        :accessor trd-delta-time                                  :initform nil :documentation "Интервал между записями тренда")
   (trd-analog-number     :accessor trd-analog-number                               :initform nil :documentation "Количество аналоговых сигналов")
   (trd-discret-number    :accessor trd-discret-number                              :initform nil :documentation "Количество дискретных сигналов")
   (trd-analog-ht         :accessor trd-analog-ht                                   :initform nil :documentation "Хеш-таблица аналоговых сигналов")
   (trd-discret-ht        :accessor trd-discret-ht                                  :initform nil :documentation "Хеш-таблица дискретных сигналов"))
  (:documentation "@b(Описание:) класс @b(<trd>) служит для предоставления
интерфейса к файлу-тренду, содержащему записи аналоговых и дискретных сигналов.

Файл-тренд состоит из:
@begin(enum)
@item(Записи заголовка тренда;)
@item(Записей дескрипторов (описаний) аналоговых сигналов, см. <a-signal>;)
@item(Записей дескрипторов (описаний) дискретных сигналов, см. <d-signal>;)
@item(Записей аналоговых и дискретных сигналов, состоящий из последовательно записанных списка аналоговых сигналов и упакованного списка дискретных сигналов;

4.1 Каждый аналоговый сигнал кодируется целочисленным значением длиной 2 байта, 
его вычисляется по формуле: rez=analog-LowLimit+(i*(analog-HighLimit-analog-LowLimit)/65535)

4.2 Каждый дискретный сигнал кодируется одним битом информации 0|1)
@end(enum)


Заголовок тренда имеет следующую структуру:
@begin(table)
@begin(row) @cell(Поле)             @cell(Дина поля, байт)  @cell(Примечание) @end(row)
@begin(row) @cell(id)               @cell(5)                @cell(Строка идентификации) @end(row)
@begin(row) @cell(version)          @cell(1)                @cell(Версия данных трендера) @end(row)
@begin(row) @cell(date-day)         @cell(1)                @cell(Число месяца) @end(row)
@begin(row) @cell(date-month)       @cell(1)                @cell(Порядковый номер месяца) @end(row)
@begin(row) @cell(date-year)        @cell(1)                @cell(Год-2000) @end(row)
@begin(row) @cell(time-hour)        @cell(1)                @cell(Час) @end(row)
@begin(row) @cell(time-minute)      @cell(1)                @cell(Минута) @end(row)
@begin(row) @cell(time-second)      @cell(1)                @cell(Секунда) @end(row)
@begin(row) @cell(reserv)           @cell(2)                @cell(Резерв - содержит сумму аналоговых и дискретных сигналов до 2^16=65536 шт) @end(row)
@begin(row) @cell(total-records)    @cell(4)                @cell(Количество записей, содержащееся в тренде до 2^32=4294967296 шт) @end(row)
@begin(row) @cell(delta-time)       @cell(8)                @cell(Интервал времени между записями, с) @end(row)
@begin(row) @cell(analog-number)    @cell(2)                @cell(Количество аналоговых сигналов до 2^16=65536 шт) @end(row)
@begin(row) @cell(discret-number)   @cell(2)                @cell(Количество дискретных сигналов до 2^16=65536 шт) @end(row)
@end(table)

При записи в тренд на восемь дискретных сигналов отводится один байт.

Сигналы упаковываются побайтно слева-направо."))


(defmethod print-object ((trd <trd>) stream)
  (format stream "Path= ~S~%" (trd-file-name trd) )
  (when (trd-file-descr trd)
    (format stream "id=~S version=~A " (trd-id-string trd) (trd-version trd))
    (format stream "[ ")
    (mnas-string/print:day-time (trd-utime-start trd) :stream stream)
    (format stream " ; ")
    (mnas-string/print:day-time (trd-utime-end trd) :stream stream)
    (format stream " ]")
    (format stream "~%Reserv         = ~A~%Total-records  = ~A~%Delta-time     = ~A~%Analog-number  = ~A~%Discret-number = ~A"
	    (trd-reserv trd) (trd-total-records trd) (trd-delta-time trd) (trd-analog-number trd) (trd-discret-number trd))
    (format stream "~%==================================================
Перечень аналоговых сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-analog-ht trd) )
    (format stream "~%==================================================
Перечень дискретных сигналов
==================================================~%")
    (maphash #'(lambda (k v) (format stream "~S ~S~%" k v)) (trd-discret-ht trd) )))

(export 'trd-open )

(defmethod trd-open ((trd <trd>))
"@b(Описание:) trd-open выполняет открытие файла тренда включая:
@begin(list)
 @item(чтение заголовка;)
 @item(разбор аналоговых сигналов;)
@item(разбор дискретных сигналов.)
@end(list)
"
  (trd-read-header trd)
  (trd-read-analog-ht trd )
  (trd-read-discret-ht trd)
  trd)

(defmethod trd-read-header((trd <trd>))
  "Выполняет открытие файла тренда и чтение заголовка тренда"
  (when (null (trd-file-descr trd))
    (setf (trd-file-descr trd) (open-trd-file-read (trd-file-name trd)))
    (let ((in (trd-file-descr trd)) (bufer nil) (date-day nil) (date-month nil) (date-year nil) (time-hour nil) (time-minute nil) (time-second nil))
      (setf (trd-id-string trd)      (recode-string (read-trd-file in *head-id-wid*))
	    (trd-version trd)        (car (read-trd-file in *head-version-wid*))
	    bufer                  (read-trd-file in *head-date-wid*)
	    date-day               (first bufer)
	    date-month             (second bufer)
	    date-year              (+ 2000 (third bufer))
	    bufer                  (read-trd-file in *head-time-wid*)
	    time-hour              (first bufer)
	    time-minute            (second bufer)
	    time-second            (third bufer)
	    (trd-utime-start trd)      (encode-universal-time time-second time-minute time-hour date-day date-month date-year)
	    (trd-reserv trd)         (read-trd-file-short in)
	    (trd-total-records trd)  (read-trd-file-long in)
	    (trd-delta-time trd)     (read-trd-file-double in)
	    (trd-analog-number trd)  (read-trd-file-short in)
	    (trd-discret-number trd) (read-trd-file-short in))
      (setf (trd-total-records trd)
	    (/ (- (file-length (trd-file-descr trd)) (trd-start-offset trd))
	       (trd-record-length trd)))))
  trd)

(defmethod trd-read-analog-ht((trd <trd>))
  "Выполняет разбор аналоговых сигналов"
  (when (null (trd-analog-ht trd))
    (setf (trd-analog-ht trd)  (make-hash-table :test #'equal :size (trd-analog-number trd)))
    (file-position (trd-file-descr trd) *head-wid*)
    (let ((in (trd-file-descr trd)) (analog-id nil) (analog-description nil) (analog-units  nil) (analog-min nil) (analog-max nil))
      (dotimes (i (trd-analog-number trd) 'done)
	(setf analog-id          (recode-string (read-trd-file in *signal-id-wid*))
	      analog-description (recode-string (read-trd-file in *signal-description-wid*))
	      analog-units       (recode-string (read-trd-file in *signal-units-wid*))
	      analog-min         (read-trd-file-double in)
	      analog-max         (read-trd-file-double in)
	      (gethash analog-id (trd-analog-ht trd)) (make-instance '<a-signal>
								   :num i
								   :id  analog-id
								   :description analog-description
								   :units analog-units
								   :min analog-min
								   :max analog-max))))))

(defmethod trd-read-discret-ht((trd <trd>))
  "Выполняет разбор дискретных сигналов"
  (when (null (trd-discret-ht trd))
    (setf (trd-discret-ht trd) (make-hash-table :test #'equal :size (trd-discret-number trd)))
    (file-position (trd-file-descr trd) (+ *head-wid* (* (trd-analog-number trd) *analog-wid*)))
    (let ((in (trd-file-descr trd)) (discret-id nil) (discret-description nil))
      (dotimes (i (trd-discret-number trd) 'done)
	(setf discret-id          (recode-string (read-trd-file in *signal-id-wid*))
	      discret-description (recode-string (read-trd-file in *signal-description-wid*))
	      (gethash discret-id (trd-discret-ht trd)) (make-instance '<d-signal>
								     :num i
								     :id discret-id
								     :description discret-description))))))

(export 'trd-close )

(defmethod trd-close ((trd <trd>))
"@b(Описание:) метод @b(trd-close) выполняет закрытие файла тренда"

  (when (trd-file-descr trd)
    (close (trd-file-descr trd))
    (setf (trd-file-descr trd) nil)))

(export 'trd-start-offset )

(defmethod trd-start-offset ((trd <trd>))
  "@b(Описание:) метод @b(trd-start-offset) возвращает смещение, 
выраженное в байтах, первой (нулевой) записи тренда."
  (+ *head-wid*
     (* (trd-analog-number trd) *analog-wid*)
     (* (trd-discret-number trd) *discret-wid*)))

(export 'trd-analog-length-byte )

(defmethod trd-analog-length-byte ((trd <trd>))
  "@b(Описание:) метод @b(trd-analog-length-byte) возвращает 
длину занимаемую аналоговыми сигналами одной записи тренда.
"
  (* (trd-analog-number trd) 2))

(export 'trd-discret-length-byte )

(defmethod trd-discret-length-byte ((trd <trd>))
  "@b(Описание:) метод @b(trd-discret-length-byte) возвращает
количество байт необходимое для записи дискретных сигналов
одной записи.
"
  (ceiling (/ (trd-discret-number trd) 8)))

(export 'trd-record-length )

(defmethod trd-record-length ((trd <trd>))
  "@b(Описание:) метод @b(trd-record-length) возвращает
длину одной записи тренда."
  (+  (trd-analog-length-byte trd)  (trd-discret-length-byte trd)))

(export 'trd-discret-offset )

(defmethod trd-discret-offset ((trd <trd>))
  "@b(Описание:) метод @b(trd-discret-offset) возвращает 
смещение в байтах от начала записи до начала записи дискретных сигналов."
  (+ (* (trd-analog-number trd) 2)))

(export 'trd-utime-end )

(defmethod trd-utime-end ((trd <trd>))
"@b(Описание:) метод @b(trd-utime-end) возвращает время окончания тренда.
Время возвращается в универсальном формате (universal-time)"
  (+ (trd-utime-start trd)
     (floor (* (trd-total-records trd) (trd-delta-time trd)))))

(export 'trd-analog-signal-list )

(defmethod trd-analog-signal-list ((trd <trd>) signal-string-list)
  "@b(Описание:) метод @b(trd-analog-signal-list) возвращает список
аналоговых сигналов тренда <trd>, которые соответствуют списку
обозначений сигналов из списка signal-string-list"
  (when (trd-file-descr trd)
    (mapcar #'(lambda(el)
		(gethash el (trd-analog-ht trd)))
	    signal-string-list)))

(export 'trd-discret-signal-list )

(defmethod trd-discret-signal-list ((trd <trd>) signal-string-list)
"Возвращает список дискретных сигналов тренда trd, 
которые соответствуют списку обозначений сигналов из списка signal-string-list"
  (when  (trd-file-descr trd)
    (mapcar #'(lambda(el)
		(gethash el (trd-discret-ht trd)))
	    signal-string-list)))

(export 'trd-analog-by-rec-number )

(defmethod trd-analog-by-rec-number ((trd <trd>) rec-number signal-list)
  "@b(Описание:) метод @b(trd-analog-by-rec-number) возвращает список
значений тренда @b(trd)  для записи под номером rec-number,
соответствующий сигналам signal-list"
  (when (and (trd-file-descr trd) (< -1 rec-number (trd-total-records trd)))
    (file-position (trd-file-descr trd) 
		   (+ (trd-start-offset trd) (* rec-number (trd-record-length trd))))
    (let* ((v-sh (make-array (trd-analog-number trd) :element-type 'integer)))
      (dotimes (i (trd-analog-number trd) 'done)
	(setf (svref v-sh i)
	      (read-trd-file-short (trd-file-descr trd))))
      (mapcar #'(lambda(el) (<a-signal>-value el (svref v-sh (a-signal-num el))))
	      signal-list))))

(export '(trd-record-number-by-utime))

(defmethod trd-record-number-by-utime ( (trd <trd>) utime)
  "@b(Описание:) метод @b(trd-record-number-by-utime)
 Возвращает номер записи по универсальному времени"
  (floor (- utime (trd-utime-start trd)) (trd-delta-time trd)))

(export '(trd-utime-by-record-number))

(defmethod trd-utime-by-record-number ((trd <trd>) record-number)
  "@b(Описание:) метод @b(trd-record-number-by-utime)
 Возвращает номер записи по универсальному времени"
   (+ (trd-utime-start trd) (floor record-number (/ 1 (trd-delta-time trd)))))

(export 'trd-discret-by-rec-number )

(defmethod trd-discret-by-rec-number ( (trd <trd>) rec-number d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (when (and (trd-file-descr trd) (< -1 rec-number (trd-total-records trd)))
    (file-position (trd-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (read-trd-file (trd-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signal-list))))

(export 'trd-discret-by-rec-number-t-nil )

(defmethod trd-discret-by-rec-number-t-nil ( (trd <trd>) rec-number d-signal-list)
"@b(Описание:) метод @b(trd-discret-by-rec-number-t-nil) возвращает 
список значений тренда trd для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (when (and (trd-file-descr trd) (< -1 rec-number (trd-total-records trd)))
    (file-position (trd-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (read-trd-file (trd-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (logbitp (<d-signal>-num  el ) s-int))
	      d-signal-list))))

(export 'trd-discret-by-utime)

(defmethod trd-discret-by-utime ( (trd <trd>) utime d-signal-list)
  "trd-discret-by-utime"
  (trd-discret-by-rec-number trd (trd-record-number-by-utime trd utime) d-signal-list))

(export 'trd-discret-by-utime-t-nil)

(defmethod trd-discret-by-utime-t-nil ( (trd <trd>) utime d-signal-list)
  "trd-discret-by-utime-t-nil"
  (trd-discret-by-rec-number-t-nil trd (trd-record-number-by-utime trd utime) d-signal-list))

(export 'trd-analog-by-utime )

(defmethod trd-analog-by-utime ( (trd <trd>) utime signal-list)
"@b(Описание:) метод @b(trd-analog-by-utime) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (trd-analog-by-rec-number trd
			    (trd-record-number-by-utime trd utime)
			    signal-list))

(export 'trd-analog-mid-by-utime )

(defmethod trd-analog-mid-by-utime ((trd <trd>) utime signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
"@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (trd-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (math/core:transpose rez))
		     (push (trd-analog-by-rec-number trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:average-value rezult))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(analogs-in-records))

(defmethod analogs-in-records ((trd <trd>) start-record end-record signal-list)
  "@b(Описание:) метод @b(analogs-in-records) возвращает список
значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), начиная с записи @b(start-record) включительно 
до записи @b(end-record) исключительно."
  (when  (trd-file-descr trd)
    (math/core:transpose
	     (loop :for i :from start-record :below end-record
		   :collect (trd-analog-by-rec-number trd i signal-list)))))

(export '(analogs-in-utimes))

(defmethod analogs-in-utimes ((trd <trd>) start-utime end-utime signal-list)
  "@b(Описание:) метод @b(trd-analog-mid-by-utime)  возвращает список
осредненных значений аналоговых сигналов, содержащися в списке @b(signal-list),
тренда @b(trd), соответствующих моменту времени @b(utime)."
  (when  (trd-file-descr trd)
    (analogs-in-records trd
			(trd-record-number-by-utime trd start-utime)
			(trd-record-number-by-utime trd end-utime)
			signal-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'trd-analog-mid-by-snames )

(defmethod trd-analog-mid-by-snames ( (trd <trd>) utime snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
"Возвращает список средних значений параметров, 
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr trd)
    (trd-analog-mid-by-utime trd utime (trd-analog-signal-list trd snames) :n-before n-before :n-after n-after)))

(export 'trd-analog-stddev-by-utime )

(defmethod trd-analog-stddev-by-utime ( (trd <trd>) utime signal-list &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
"Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов signal-list;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr trd)
    (let* ((rez nil)
	   (n-start (- (trd-record-number-by-utime trd utime) n-before))
	   (rezult (dotimes (i (+ n-before n-after 1) (transpose rez))
		     (push (trd-analog-by-rec-number trd (+ n-start i) signal-list) rez))))
      (mapcar #'math/stat:standard-deviation rezult))))

(export 'trd-analog-stddev-by-snames )

(defmethod trd-analog-stddev-by-snames ( (trd <trd>) utime snames &key (n-before *mid-value-number-offset*) (n-after *mid-value-number-offset*))
"Возвращает список стандартных отклонений для параметров,
записанных в тренде trd в момент времени utime для списка сигналов, определяемых их именами snames;
Осреднение происходит в интервале записей от  n-before до n-after"
  (when  (trd-file-descr trd)
    (trd-analog-stddev-by-utime trd utime (trd-analog-signal-list trd snames) :n-before n-before :n-after n-after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Singal separation to analog|discret|unexpected ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'trd-separate-signals )

(defmethod trd-separate-signals ((trd <trd>) singnal-str-list)
"Выполняет проверку того, что имена сигналов, заданные в переменной singnal-str-list,
присутствуют для данного тренда в перечне аналоговых сигналов или дискретных сигналов
или отсутствуют в обоих перечнях.
   Возвращает список элементами которого являются:
- список аналоговых сигналов;
- список дискретных сигналов;
- список строк с именами не соответствующими 
  ни аналоговым ни дискретным сигналам."
  (let ((a-rez nil)
	(d-rez nil)
	(error-rez nil)
	(error-fl t))
    (mapcar #'(lambda (el)
		(setf error-fl t)
		(multiple-value-bind (v r) (gethash  el (trd-analog-ht trd ) )
		  (when r
		    (push v a-rez)
		    (setf error-fl nil)))
		(multiple-value-bind (v r) (gethash  el (trd-discret-ht trd ) )
		  (when r (push v d-rez)
			(setf error-fl nil)))
		(when error-fl (push el error-rez)))
	    singnal-str-list)
    (list (nreverse a-rez) (nreverse d-rez) (nreverse error-rez))))

(export 'trd-separate-a-signals )

(defmethod trd-separate-a-signals ((trd <trd>) singnal-str-list)
"Выделяет из переменной singnal-str-list аналоговые сигналы"
  (first (trd-separate-signals trd singnal-str-list)))

(export 'trd-separate-d-signals )

(defmethod trd-separate-d-signals ((trd <trd>) singnal-str-list)
"Выделяет из переменной singnal-str-list дискретные сигналы"
  (second (trd-separate-signals trd singnal-str-list)))

(export 'trd-separate-not-signals )

(defmethod trd-separate-not-signals ((trd <trd>) singnal-str-list)
"Выделяет из переменной singnal-str-list неожиданные сигналы."
  (second (trd-separate-signals trd singnal-str-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Splitting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(split-on-intervals-when-flag-is-on))

(defmethod split-on-intervals-when-flag-is-on ((trd <trd>) d-signal-str )
"@b(Описание:) метод @b(split-on-intervals-when-flag-is-on) для 
тренда @b(trd) выполняет поиск диапазонов, для которых значение 
дискретного сигнала с именем d-signal-str имеет значение 1.

 Начало и конец диапазона выражено в порядковы номерах записи с начала тренда.
todo: доработать, чтоб возвращался последний диапазон при поднятом флаге в конце"
  (let* (
	 (flag (gethash d-signal-str (trd-discret-ht trd)))
	 (flag-lst (list flag))
	 (total-rec (trd-total-records trd))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1)
	 (rez nil)
	 )
    (dotimes (i (trd-total-records trd) (nreverse rez-lst))
      (setf rez (first(trd-discret-by-rec-number-t-nil trd i flag-lst)))
      (if rez
	  (setf n-start (min i n-start)
		n-end   (max i n-end))
	  (when (< -1 n-end)
	    (push (list n-start n-end) rez-lst)
	    (setf n-start total-rec
		  n-end -1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(split-on-utimes-when-flag-is-on))

(defmethod split-on-utimes-when-flag-is-on ((trd <trd>) d-signal-str )
  "@b(Описание:) метод @b(split-on-intervals-when-flag-is-on) для 
тренда @b(trd) выполняет поиск диапазонов, для которых значение 
дискретного сигнала с именем d-signal-str имеет значение 1.

 Начало и конец диапазона выражено в порядковы номерах записи с начала тренда.
todo: доработать, чтоб возвращался последний диапазон при поднятом флаге в конце"
  (mapcar
   #'(lambda (el)
       (list (trd-utime-by-record-number trd (first el))
	     (trd-utime-by-record-number trd (second el))))
   (split-on-intervals-when-flag-is-on trd  d-signal-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(split-on-intervals-of-time-when-flag-is-on))

(defmethod split-on-intervals-of-time-when-flag-is-on ((trd <trd>) d-signal-str)
"Для тренда trd выполняет поиск диапазонов, для которых
значение сигнала d-signal-str принимало значение 1. 
И возвращает длительность этих диапазонов"
  (let ((intervals (trd-flag-on-intervals trd d-signal-str)))
    (values
     (mapcar #'(lambda (el) (* -1 (trd-delta-time trd) (apply #'- el))) intervals)
     intervals)))

(export 'split-on-intervals-by-condition )

(defmethod split-on-intervals-by-condition ((trd <trd>) start-signal-str-lst end-signal-str-lst)
"Выполняет деление тренда на диапазоны.

Возвращает список.

Каждый элемент, возвращаемого списка, состоит из двух номеров записей - начальной и конечной.

Параметры:
@begin(list)
 @item(@cl:param(trd)                      - объект типа <trd> [тренд];)
 @item(@cl:param(start-signal-str-lst)   - список имен [строк] дискретных сингалов тренда;)
 @item(@cl:param(end-signal-str-lst)     - список имен [строк] дискретных сингалов тренда.)
@end(list)

Логика деления на диапазоны следующая зависит от того, имеются-ли элементы в end-signal-str-lst.

Если элементов в end-signal-str-lst нет:
@begin(list)
 @item(возвращаются диапазоны для, которых все сигналы, соответствующие 
       списку start-signal-str-lst установлены [равны единице].)
@end(list)

Если элементы в end-signal-str-lst есть:
возвращаются диапазоны:
@begin(list)
 @item(в первой записи которых все сигналы, 
       соответствующие списку start-signal-str-lst установлены [равны единице];)
 @item(в следующей после последней записи которых все сигналы, 
       соответствующие списку end-signal-str-lst установлены [равны единице].)
@end(list)
"
  (let* ((start-flag-lst (mapcar #'(lambda(el) (gethash el (trd-discret-ht trd))) start-signal-str-lst))
	 (end-flag-lst   (mapcar #'(lambda(el) (gethash el (trd-discret-ht trd))) end-signal-str-lst))
	 (fl-start nil)
	 (fl-end   nil)
	 (total-rec (trd-total-records trd))
	 (rez-lst nil)
	 (n-start total-rec)
	 (n-end -1))
    (dotimes (i (trd-total-records trd) (nreverse rez-lst))
      (setf fl-start (or fl-start (apply-and (trd-discret-by-rec-number-t-nil trd i start-flag-lst))))
      (if fl-start
	  (progn
	    (setf fl-end nil
		  n-start (min i n-start)
		  n-end   (max i n-end))))
      (if end-flag-lst
	  (setf fl-end   (or fl-end   (apply-and (trd-discret-by-rec-number-t-nil trd i end-flag-lst))))
	  (setf fl-end   (or fl-end   (not (apply-and (trd-discret-by-rec-number-t-nil trd i start-flag-lst))))))
      (if (and fl-start fl-end (< -1 n-end))
	  (progn
	    (push (list n-start n-end) rez-lst)
	    (setf 	 fl-start nil
			 fl-end   nil
			 n-start total-rec
			 n-end -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interval-to-time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'trd-interval-to-secods )

(defmethod trd-interval-to-secods ((trd <trd>) interval)
"Преобразует диапазон времени, заданный в записях, в секунды"
  (* (trd-delta-time trd) (apply #'- (reverse interval))))

(export 'trd-interval-to-minutes )

(defmethod trd-interval-to-minutes ((trd <trd>) interval)
"Преобразует диапазон времени, заданный в записях, в минуты"
  (* 1/60 (trd-interval-to-secods trd interval)))

(export 'trd-interval-to-hours )

(defmethod trd-interval-to-hours ((trd <trd>) interval)
"Преобразует диапазон времени, заданный в записях, часы"
  (* 1/60 1/60 (trd-interval-to-secods trd interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-record-number-to-udate ((trd <trd>) rec-number)
  "trd-record-number-to-udate"
  (+ (trd-utime-start trd) (round (* rec-number (trd-delta-time  trd)))))

(export 'trd-record-number-to-udate)

(defmethod trd-record-number-by-udate ((trd <trd>) udate)
  "trd-record-number-by-udate"
  (round
   (/
    (- udate (trd-utime-start trd))
    (trd-delta-time trd))))

(export 'trd-record-number-by-udate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'trd-analog-ht->org )

(defmethod trd-analog-ht->org ((trd <trd>) )
"@b(Описание:) метод @b(trd-analog-ht->org) возврвщает 2d-list список,
отображающий все аналоговые сигналы в удобном виде для представления в org режиме.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (trd-analog-ht->org *trd*) 
 =>
 ((0 \"FP510\" 0.0d0 1.600000023841858d0 \"МПа\" \"Избыточное давление газа перед суммирующ\")
  (1 \"FP520\" 0.0d0 16.0d0 \"кПа\" \"Перепад давления газа на суммирующем РУ\")
;;..........................................................................
  (313 \"SF2\" 0.0d0 1000.0d0 \"\" \"Площадь сечения на входе в кс, см2\"))
@end(code)
@begin[lang=org](code)
|   0 | FP510    |   0.0d0 | 1.600000023841858d0 | МПа     | Избыточное давление газа перед суммирующ |
|   1 | FP520    |   0.0d0 |              16.0d0 | кПа     | Перепад давления газа на суммирующем РУ  |
.......................................................................................................
| 313 | SF2      |   0.0d0 |            1000.0d0 |         | Площадь сечения на входе в кс, см2       |
@end(code)

"
  (loop :for k :being :the hash-key :using (hash-value v) :of (trd-analog-ht trd)
	:collect  (list (a-signal-num v) (<a-signal>-id v) (<a-signal>-min v) (<a-signal>-max v) (<a-signal>-units v) (<a-signal>-description v))))

(export 'trd-discret-ht->org )

(defmethod trd-discret-ht->org ((trd <trd>) )
"@b(Описание:) метод @b(trd-discret-ht->org) возврвщает 2d-list список,
отображающий все дискретные сигналы в удобном виде для представления в org режиме.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (trd-discret-ht->org *trd*) =>
 ((0 \"FH020\" \"Дроссельный кран рабочего насоса ДТ - от\")
  (1 \"FH021\" \"Дроссельный кран рабочего насоса ДТ - за\")
;;.....................................................
  (100 \"FH011\" \"Кран подачи ДТ - закрыт\"))
@end(code)
@begin[lang=org](code)
|   0 | FH020 | Дроссельный кран рабочего насоса ДТ - от |
|   1 | FH021 | Дроссельный кран рабочего насоса ДТ - за |
..........................................................
| 100 | FH011 | Кран подачи ДТ - закрыт                  |
@end(code)
"
  (loop :for k :being :the hash-key :using (hash-value v) :of (trd-discret-ht trd)
	:collect  (list (<d-signal>-num v) (<d-signal>-id v)  (<d-signal>-description v))))

(export 'trd-header->org )

(defmethod trd-header->org ((trd <trd>))
"trd-header->org"
  (let ((rez nil))
    (push (list "Файл" (trd-file-name trd )) rez)
    (when (trd-file-descr trd)
      (progn
	(push (list "Версия тренда" 	                  (trd-version trd) ) rez)
	(push (list "Дата создания тренда"                (mnas-string/print:date (trd-utime-start trd) :stream nil)) rez)
	(push (list "Время создания тренда"               (mnas-string/print:day-time (trd-utime-start trd) :stream nil)) rez)
	(push (list "К-во аналоговых+дискретных сигналов" (trd-reserv         trd) ) rez)
	(push (list "Общее число записей в тренде"        (trd-total-records  trd) ) rez)
	(push (list "Интервал между записями тренда"      (trd-delta-time     trd) ) rez)
	(push (list "Количество аналоговых сигналов"      (trd-analog-number  trd) ) rez)
	(push (list "Количество дискретных сигналов"      (trd-discret-number trd) ) rez)))
    (nreverse rez)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod trd-discret-by-rec-number ((trd <trd>) rec-number d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (when (and (trd-file-descr trd) (< -1 rec-number (trd-total-records trd)))
    (file-position (trd-file-descr trd) 
		   (+ (trd-start-offset trd)
		      (* rec-number (trd-record-length trd))
		      (trd-discret-offset trd) ))
    (let ((s-int (list-to-int (read-trd-file (trd-file-descr trd) (trd-discret-length-byte trd)))))
      (mapcar #'(lambda (el)
		  (if (logbitp (<d-signal>-num  el ) s-int) 1 0))
	      d-signal-list))))

(export '(trd-analog-discret-by-rec-number))

(defmethod trd-analog-discret-by-rec-number ((trd <trd>) rec-number a-signal-list d-signal-list)
  "@b(Описание:) метод @b(trd-discret-by-rec-number)
возвращает список значений тренда <trd> для записи под номером rec-number,
соответствующий сигналам d-signal-list."
  (append (trd-analog-by-rec-number  trd rec-number a-signal-list)
	  (trd-discret-by-rec-number trd rec-number d-signal-list)))
