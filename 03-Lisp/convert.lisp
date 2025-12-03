(defun split (str delim)
  "Split STR by DELIM into a list of substrings."
  (let ((result '())
        (start 0))
    (loop for i from 0 to (length str)
          do (if (or (= i (length str)) (char= (aref str i) delim))
                 (let ((substr (subseq str start i)))
                   (push substr result)
                   (setf start (1+ i)))))
    (reverse result)))

(defun read-csv (path)
  "Read CSV file and return list of rows (each row is a list of fields)."
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect (split line #\,))))

(defun csv->xml (csv-data)
  "Convert parsed CSV data (list of rows) into XML string."
  (let* ((header (first csv-data))
         (rows   (rest csv-data)))
    (with-output-to-string (out)
      (format out "<rows>~%")
      (dolist (row rows)
        (format out "  <row>~%")
        (loop for field in row
              for name in header
              do (format out "    <~a>~a</~a>~%" name field name))
        (format out "  </row>~%"))
      (format out "</rows>"))))

(defun write-xml (path content)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(defun convert-csv-to-xml (csv-file xml-file)
  (let* ((csv-data (read-csv csv-file))
         (xml      (csv->xml csv-data)))
    (write-xml xml-file xml)
    (format t "✓ Done! Created ~a~%" xml-file)))
