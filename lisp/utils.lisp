;;;; -----------------------------------------------------------------
;;;; utility functions
;;;; -----------------------------------------------------------------

(declaim (ftype (function (t t) t) binary-search))

(defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))
    (do () ((< high low) nil)
      (let ((middle (floor (/ (+ low high) 2))))
        (cond ((> (aref array middle) value)
               (setf high (1- middle)))
              ((< (aref array middle) value)
               (setf low (1+ middle)))
              (t (return middle)))))))

(defun ends-with (s1 s2)
  (string= s1 s2 :start1 (- (length s1) (length s2))))

(defun split (s)
  (split-sequence:split-sequence #\, s))

(defun my-integer (n)
  (char-int n))

(defun average (s)
  (float (/ (reduce #'+ s) (length s))))

(defmacro silent (&body body)
  `(progn ,@body 'done))

(defun print-file (f o)
  (with-open-file (s f :direction :output :if-exists :supersede)
    (print o s)))

(defun read-file (f)
  (with-open-file (s f)
    (read s)))

(defun hash-keys (hash)
  (loop for key being the hash-keys in hash collect key))

(defun list->vector (s)
  (let ((sz (length s)))
  (let ((a (make-array sz)))
    (dotimes (n sz)
    (setf (svref a n) (car s)
	  s (cdr s)))
    a)))

