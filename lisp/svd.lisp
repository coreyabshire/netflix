
(defun svd (mn p epochs lrate init)
  (let ((m (array-dimension mn 0))
	(n (array-dimension mn 1)))
    (let ((mp (make-array (list m p)))
	  (np (make-array (list n p))))
      (dotimes (k p)
	(dotimes (i m) (setf (aref mp i k) init))
	(dotimes (j n) (setf (aref np j k) init)))
      (dotimes (k p)
	(dotimes (e epochs)
	  (dotimes (i m)
	    (dotimes (j n)
	      (let ((err (* lrate (- (aref mn i j) (predict mp np i j k))))
		    (iv (aref mp i k)))
		(incf (aref mp i k) (* err (aref np j k)))
		(incf (aref np j k) (* err iv)))))))
      (list mp np))))

(defun predict (mp np i j p)
  (loop for k from 0 upto p sum (* (aref mp i k) (aref np j k))))

(defun combine (mpnp)
  (let ((mp (car mpnp)) (np (cadr mpnp)))
    (let ((m (array-dimension mp 0))
	  (n (array-dimension np 0))
	  (p (array-dimension mp 1)))
      (let ((a (make-array (list m n))))
	(dotimes (i m a)
	  (dotimes (j n)
	    (dotimes (k p)
	      (incf (aref a i j) (* (aref mp i k) (aref np j k))))))))))

(defun rmse (a b)
  (let ((s 0.0) (c 0) (m (array-dimension a 0)) (n (array-dimension a 1)))
    (dotimes (i m (sqrt (/ s c)))
      (dotimes (j n)
	(incf s (expt (- (aref a i j) (aref b i j)) 2))
	(incf c)))))

(defparameter *test-mp*
  (let ((a (make-array '(3 5))))
    (dotimes (i (array-dimension a 0) a)
      (dotimes (k (array-dimension a 1))
	(setf (aref a i k) (random 4))))))

(defparameter *test-np*
  (let ((a (make-array '(10 5))))
    (dotimes (j (array-dimension a 0) a)
      (dotimes (k (array-dimension a 1))
	(setf (aref a j k) (random 4))))))
      
(defparameter *test-mn*
  (let ((mp *test-mp*) (np *test-np*))
    (let ((m (array-dimension mp 0))
	  (n (array-dimension np 0))
	  (p (array-dimension mp 1)))
      (let ((a (make-array (list m n))))
	(dotimes (i m a)
	  (dotimes (j n)
	    (dotimes (k p)
	      (incf (aref a i j) (* (aref mp i k) (aref np j k))))))))))
	       

(defvar *test-p*)

