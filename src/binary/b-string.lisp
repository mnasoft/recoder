;;;; ./src/binary/b-string.lisp

(in-package :recoder/binary)

(defun b-read-string (in byte-number &key (encoding :cp1251))
  "@b(Описание:) метод @b(b-read-string) возвращает строку,
считываемую из бинарного потока @b(in). При этом из потока считывается
количество байт @b(byte-number). Начальные и конечные нуль-символы из
строки исключаются. Символы декодируются из кодировки @b(encoding).

 @b(Пример использования:)
@begin[lang=lisp](code)
(progn
  (let ((buffer-length 900)
        (path
          (merge-pathnames
           #P\"trd/bin-01.bin\" 
           (asdf:system-source-directory :recoder/binary))))
    (let ((w (open-b-write path)))
      (b-write-string w *pangram-uk-1* buffer-length :external-format :KOI8-U)
      (close w))
    (let* ((r (open-b-read path))
           (rez (b-read-string r buffer-length :encoding :KOI8-U)))
      (close r)
      rez)))
@end(code)
"
  (string-trim `,(format nil "~A" #\Nul)
               (babel:octets-to-string
                (coerce (b-read in byte-number)
                        '(vector (unsigned-byte 8)))
                :encoding encoding)))

;;; TODO - реализация пока применима только для однобайтных кодировок.
(defun b-write-string (string out byte-number
                       &key (external-format :cp1251))
  "@b(Описание:) метод @b(b-write-string) выводит в поток @b(out)
строку @b(string). 

 @b(Переменые:)
@begin(list)
 @item(out - двоичныый поток вывода;)
 @item(string - строка, подлежащая выводу в поток;)
 @item(external-format - кодировка, в которой строка будет сохранена.
       Список кодировок доступный на данный момент можно получить при
       помощи вызова следующей функции:
       (babel:list-character-encodings);)
 @item(byte-number - количество байт до которых усекается строка.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((w (open-b-write 
          (merge-pathnames #P\"trd/bin-01.bin\" 
                           (asdf:system-source-directory :recoder)))))
  (b-write-string w *pangram-uk-1* :external-format :KOI8-U :byte-number 200)
  (close w))
@end(code)
"
  (let ((s-buffer (make-string byte-number :initial-element #\Nul)))
    (loop :for i :from 0 :below byte-number
          :for j :from 0 :below (length string) :do
            (setf (char s-buffer i) (char string j)))
    (b-write
     (babel-streams:with-output-to-sequence (o :return-as 'list :external-format external-format)
       (format o "~A" s-buffer))
     out)))
