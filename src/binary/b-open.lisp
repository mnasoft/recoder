(in-package :recoder/binary)

(defmacro with-open-file-b-in ((stream filespec &key
                                                  (element-type ''(unsigned-byte 8)))
                                    &body body)
  "Открывает файл в бинарном режиме на чтение и выполняет тело с указанным потоком.
   По умолчанию element-type — (unsigned-byte 8)."
  `(with-open-file (,stream ,filespec
                            :direction :input
                            :element-type ,element-type
                            :if-does-not-exist :error)
     ,@body))


(defmacro with-open-file-b-out ((stream filespec &key
                                                   (element-type ''(unsigned-byte 8))
                                                   (if-exists :supersede))
                                &body body)
  "Макрос для открытия файла в бинарном режиме на запись.
   Аргументы:
   - stream: имя переменной потока
   - filespec: путь к файлу
   - element-type: тип элементов (по умолчанию (unsigned-byte 8))
   - if-exists: поведение при существующем файле (по умолчанию :supersede)

   Пример:
   (with-open-file-binary-out (out \"file.bin\")
     (write-byte 255 out))"
  `(with-open-file (,stream ,filespec
                            :direction :output
                            :element-type ,element-type
                            :if-exists ,if-exists
                            :if-does-not-exist :create)
     ,@body))

(defun open-b-read (path)
  "@b(Описание:) функция @b(open-b-read) выполняет открытие файла для
 бинарного чтения.

 Пример использования см. в описании к функции @b(open-b-write)."
  (open path :element-type 'unsigned-byte))

(defun open-b-write (path)
    "@b(Описание:) функция @b(open-b-read) выполняет открытие файла для
 бинарной записи.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((val 2050)
       (rez nil)
       (fname (merge-pathnames
                #P\"trd/binary.bin1\"
                (asdf:system-source-directory
                 (asdf:find-system \"recoder/binary\")))))
   (let ((out (open-b-write fname)))
     (b-write-short val out)
     (close out))
   (let ((in (open-b-read fname)))
     (setf rez (b-read-short in))
     (close in)
     rez))
 @end(code)
"
  (open path :element-type 'unsigned-byte :direction :output :if-exists :supersede))
