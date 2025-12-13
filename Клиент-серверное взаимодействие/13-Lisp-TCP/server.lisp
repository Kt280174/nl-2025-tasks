;;;; server.lisp


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :usocket))  

(defparameter *log-file* "server.log")

(defun log-line (fmt &rest args)
  (with-open-file (out *log-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (apply #'format out fmt args)
    (terpri out)))

(defun split-by-space (line)
  "Tách line thành list từ (tokens) bằng khoảng trắng."
  (let ((result '())
        (start 0)
        (len (length line)))
    (labels ((skip-spaces (pos)
               (loop while (and (< pos len)
                                (char= (aref line pos) #\Space))
                     do (incf pos))
               pos))
      (loop
        (setf start (skip-spaces start))
        (when (>= start len)
          (return (nreverse result)))
        (let ((end (or (position #\Space line :start start)
                       len)))
          (push (subseq line start end) result)
          (setf start end))))))

(defun parse-number (s)
  (handler-case
      (parse-integer s)
    (error () nil)))

(defun handle-sum (args)
  (let ((nums (mapcar #'parse-number args)))
    (if (some #'null nums)
        "ERROR: SUM expects only integers"
        (write-to-string (reduce #'+ nums)))))

(defun handle-reverse (args)
  (let ((s (map 'string #'identity (apply #'concatenate 'string
                                          (mapcar (lambda (x)
                                                    (concatenate 'string x " "))
                                                  args)))))
    ;; remove trailing space
    (subseq (reverse s) 0 (length s))))

(defun handle-upper (args)
  (let ((s (apply #'concatenate 'string
                  (mapcar (lambda (x)
                            (concatenate 'string x " "))
                          args))))
    (string-upcase (string-right-trim " " s))))

(defun handle-lower (args)
  (let ((s (apply #'concatenate 'string
                  (mapcar (lambda (x)
                            (concatenate 'string x " "))
                          args))))
    (string-downcase (string-right-trim " " s))))

(defun process-command (line)
  (let* ((tokens (split-by-space line)))
    (if (null tokens)
        "ERROR: empty command"
        (let* ((cmd (string-upcase (first tokens)))
               (args (rest tokens)))
          (cond
            ((string= cmd "SUM")     (handle-sum args))
            ((string= cmd "REVERSE") (handle-reverse args))
            ((string= cmd "UPPER")   (handle-upper args))
            ((string= cmd "LOWER")   (handle-lower args))
            ((string= cmd "QUIT")    "QUIT")
            (t (format nil "ERROR: unknown command ~A" cmd)))))))

(defun handle-client (stream)
  (unwind-protect
       (loop for line = (ignore-errors (read-line stream nil nil))
             while line do
               (log-line "Received: ~A" line)
               (let ((resp (process-command line)))
                 (when (string= resp "QUIT")
                   (format stream "Goodbye!~%")
                   (finish-output stream)
                   (log-line "Client requested QUIT")
                   (return))
                 (format stream "~A~%" resp)
                 (finish-output stream)
                 (log-line "Response: ~A" resp)))
    (ignore-errors (close stream))))

(defun start-server (&optional (port 5000))
  (log-line "Server starting on port ~A" port)
  (let ((server-socket (usocket:socket-listen "127.0.0.1" port
                                             :reuse-address t)))
    (unwind-protect
         (loop
           (log-line "Waiting for connection...")
           (let* ((client-socket (usocket:socket-accept server-socket))
                  (stream (usocket:socket-stream client-socket)))
             (log-line "Client connected.")
             (handle-client stream)
             (log-line "Client disconnected.")))
      (usocket:socket-close server-socket)
      (log-line "Server stopped."))))
