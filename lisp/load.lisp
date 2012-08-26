;;;; -----------------------------------------------------------------
;;;; Netflix Prize Lisp Code - Corey Abshire - Copyright 2008.
;;;; -----------------------------------------------------------------



;;;; -----------------------------------------------------------------
;;;; configuration section
;;;; -----------------------------------------------------------------

(defconstant +num-movies+ 17770)

(defconstant +num-customers+ 480189)

(defconstant +num-factors+ 10)

(defconstant +num-ratings+ 5)

(defparameter *project-dir* "/Users/Corey/Netflix")

(defparameter *download-dir* "/Users/Corey/Netflix/download")

(defun project-filename (filename)
  (format nil "~a/~a" *project-dir* filename))

(defun download-filename (filename)
  (format nil "~a/~a" *download-dir* filename))

(declaim (type (simple-array single-float) *movie-values*)
	 (type (simple-array single-float) *customer-values*)
	 (type (simple-array simple-vector) *ratings*)
	 (type (simple-array simple-vector) *residuals*)
	 (type (simple-array single-float) *movie-averages*)
	 (type (simple-array single-float) *customer-average-offsets*)
	 (inline predict))

(declaim (optimize (speed 3) (safety 0) (space 0)))

;;;; -----------------------------------------------------------------
;;;; movie meta data functions
;;;; -----------------------------------------------------------------

(defstruct movie id year title avg)

(defvar *movies* (make-hash-table :test 'equal :size +num-movies+))

(defvar *movie-list* ())

(defvar *movie-ids* (loop for movie-id from 1 to +num-movies+ collect movie-id))

(defun title (id)
  (movie-title (gethash id *movies*)))

(defun movie-rating-set (m r)
  (aref *ratings* (- m 1) (- r 1)))

(defun movie-average (m)
  (let ((sum 0) (count 0))
    (loop for r from 1 to +num-ratings+ do
	 (let ((n (length (movie-rating-set m r))))
	   (incf sum (* r n))
	   (incf count n)))
    (float (/ sum count))))

(defun load-movie-averages ()
  (dolist (m *movie-list*)
    (setf (movie-avg m) (movie-average (movie-id m)))))

(defun get-movie (id)
  (gethash id *movies*))

(defun load-movies ()
  (setf *movie-list* ()
	*movie-ids* ()
	*movies* (make-hash-table :test 'equal :size +num-movies+))
  (with-open-file (s (format nil "~a/movie_titles.txt" *download-dir*))
    (let ((line)
	  (fields)
	  (movie))
      (dotimes (n +num-movies+)
	(setq line (read-line s nil nil))
	(setq fields (split-sequence:split-sequence #\, line))
	(setq movie (make-movie :id (parse-integer (first fields))
				:year (if (not (string= "NULL" (second fields)))
					  (parse-integer (second fields)))
				:title (third fields)))
	(setf (gethash (movie-id movie) *movies*) movie)
	(push movie *movie-list*)
	(push (movie-id movie) *movie-ids*)))))




;;;; -----------------------------------------------------------------
;;;; customer analysis subsystem
;;;; -----------------------------------------------------------------

(defun make-initial-customers-hash-table ()
  (make-hash-table :size 500000 :test 'equal))

(defvar *customers* (make-initial-customers-hash-table))

(defun clear-customers ()
  (setf *customers* (make-initial-customers-hash-table)))




;;;; -----------------------------------------------------------------
;;;; training data set functions
;;;; -----------------------------------------------------------------

(defvar *ratings*)

(defun training-set-filename (id)
  (format nil "~a/training_set/mv_~7,'0d.txt" *download-dir* id))

(defun training-set (id)
  (let ((filename (training-set-filename id)))
    (with-open-file (s filename)
      (read-line s nil nil)
      (do ((line (read-line s nil nil) (read-line s nil nil))
	   (count 0 (+ count 1))
	   (ratings nil (cons (make-rating (split line)) ratings)))
	  ((null line) ratings)))))

(defun save-ratings ()
  (with-open-file (out "ratings.dat" :if-exists :supersede :direction :output)
    (dotimes (m +num-movies+)
      (dotimes (r 5)
	(print (aref *ratings* m r) out)))))

(defun load-ratings ()
  (setf *ratings* (make-array (list +num-movies+ +num-ratings+) :element-type '(simple-array fixnum)))
  (with-open-file (in "ratings.dat")
    (dotimes (m +num-movies+)
      (dotimes (r +num-ratings+)
	(setf (aref *ratings* m r) (read in)))))
  'done)

(defun load-training-set (n)
  (setf *ratings*
	(do ((id 1 (+ 1 id))
	     (ratings (make-array (list n 5))))
	    ((> id n) ratings)
	  (let ((r1 (sort-ratings (training-set id))))
	    (dotimes (rating 5)
	      (setf (aref ratings (- id 1) rating) (apply #'vector (aref r1 rating))))
	    (format t "~7d ~a~%" id (title id)))))
  nil)

(defun get-rating (m c)
  (loop for r from 1 to 5 do
     (if (binary-search c (movie-rating-set m r))
	 (return r))))

(defun cheap-train (iterations)
  (dotimes (n iterations)
    (dolist (probe *probe*)
      (let ((r (probe-rec-rating probe))
	    (p (probe-rec-prediction probe)))
	(setf (probe-rec-prediction probe) (/ (+ r p) 2.0))))))

(defun sort-all-rating-sets ()
  (loop for m from 1 to +num-movies+ do
       (loop for r from 1 to 5 do
	    (setf (aref *ratings* (- m 1) (- r 1)) (sort (movie-rating-set m r) #'<)))
       (format t "~7d ~a~%" m (title m))))

(defmacro doratings ((movie-id rating customer-id) &body body)
  `(dotimes (d1 (array-dimension *ratings* 0))
     (dotimes (d2 (array-dimension *ratings* 1))
       (let ((c1 (aref *ratings* d1 d2)))
	 (dotimes (d3 (length c1))
	   (let ((,movie-id (+ d1 1))
		 (,rating (+ d2 1))
		 (,customer-id (svref c1 d3)))
	     ,@body))))))




;;;; -----------------------------------------------------------------
;;;; movie characteristic / customer preference factoring subsystem
;;;; -----------------------------------------------------------------

(defvar *customer-values*
  (make-array (list +num-customers+ +num-factors+)
	      :element-type 'single-float
	      :initial-element 1.0))

(defvar *movie-values*
  (make-array (list +num-movies+ +num-factors+)
	      :element-type 'single-float
	      :initial-element 1.0))

(defvar *customer-average-offsets*
  (make-array +num-customers+
	      :element-type 'single-float
	      :initial-element 1.0))

(defvar *movie-averages*
  (make-array +num-movies+
	      :element-type 'single-float
	      :initial-element 1.0))

(declaim (type fixnum *current-customer* *current-movie* *current-rating*))

(defvar *current-movie* 0)
(defvar *current-customer* 0)
(defvar *current-rating* 0)

(defun show-current ()
  (let ((m *current-movie*)
	(c *current-customer*)
	(r *current-rating*))
    (format t "random sample: ~a ~a~%" m c)
    (show-prediction-values m c)
    (format t "actual rating: ~a~%" r)))
  

(defun pick-random ()
  (let ((m (random +num-movies+))
	(r (random 5)))
    (let ((rs (aref *ratings* m r)))
      (let ((c (aref rs (random (length rs)))))
	(setf *current-movie* m
	      *current-customer* c
	      *current-rating* (+ r 1))
	(show-current)))))

(defun reset-values (v)
  (declare (single-float v))
  (reset-movie-values v)
  (reset-customer-values v))

(defun reset-movie-values (v)
  (declare (single-float v))
  (dotimes (m +num-movies+)
    (dotimes (f +num-factors+)
      (setf (aref *movie-values* m f) v))))

(defun reset-customer-values (v)
  (declare (single-float v))
  (dotimes (c +num-customers+)
    (dotimes (f +num-factors+)
      (setf (aref *customer-values* c f) v))))

(defun show-prediction-values (m c)
  (declare (fixnum m c))
  (let ((p (predict-baseline m c)))
    (declare (single-float p))
    (format t "baseline: ~a~%" p)
    (loop for f from 0 to (- +num-factors+ 1) do
       (let ((mv (aref *movie-values* m f))
	     (cv (aref *customer-values* c f)))
	 (let ((pv (* mv cv)))
	   (declare (single-float mv cv pv))
	   (incf p pv)
	   (format t "~2,7a x ~2,7a = ~2,7a : ~2,7a~%" mv cv pv p))))))

(proclaim '(ftype (function () single-float) sample-math))

(defun sample-math ()
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array single-float) *customer-values*))
  (the single-float (* (the single-float (aref *customer-values* 1 0))
		       (the single-float (aref *customer-values* 3 4)))))



(proclaim '(ftype (function () single-float) sample-math))
(declaim (inline sample-math))
(defun sample-math ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x (aref *customer-values* 1 0))
	(y 2.0))
    (declare (single-float x y))
    (the single-float (* x y))))
	   
(proclaim '(ftype (function () (and single-float (not null))) simplest-float-coercion))
(declaim (inline simplest-float-coercion))
(defun simplest-float-coercion ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (coerce (the single-float (aref *customer-values* 1 0)) 'single-float))

(proclaim '(type (simple-array single-float) *some-array*))
(defvar *some-array* (make-array '(10 10) :element-type 'single-float :adjustable nil))
(proclaim '(ftype (function () single-float) access-some-array))
(defun access-some-array ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (aref *some-array* 5 5))

(proclaim '(type (simple-array single-float) *some-array*))
(defvar *some-array* (make-array 10 :element-type 'single-float :adjustable nil))
(proclaim '(ftype (function () single-float) access-some-array))
(declaim (inline access-some-array))
(declaim (inline aref))
(proclaim '(ftype (function () single-float) some-function))
(declaim (inline some-function))
(defun some-function ()
  5.0)
(defun access-some-array ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x 0.0))
    (+ x (some-function))))



(defun init-movie-values-to-average ()
  (dolist (movie *movie-list*)
    (let ((v (/ (movie-avg movie) +num-factors+))
	  (m (- (movie-id movie) 1)))
      (declare (single-float v))
      (dotimes (f +num-factors+)
	(setf (aref *movie-values* m f) v)))))

(defmacro r (&body b)
  `(time (silent ,@b)))

(defun count-ratings-by-customer ()
  (clear-customers)
  (loop for movie-id from 1 to +num-customers+ do
       (let ((count 0))
	 (format t "~a (~a): " (title movie-id) movie-id)
	 (dolist (rating (training-set movie-id))
	   (let ((customer-id (nth 0 rating))
		 (rating (nth 1 rating)))
	     (if (not (gethash customer-id *customers*))
		 (setf (gethash customer-id *customers*) (vector 0 0 0 0 0)))
	     (incf (svref (gethash customer-id *customers*) (- rating 1)))
	     (incf count)))
	 (format t "~a ~%" count))))

(defun train (f &optional (max-movie-id +num-movies+))
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (fixnum max-movie-id))
  (dotimes (mi max-movie-id)
    (dotimes (ri +num-ratings+)
      (let ((r (+ ri 1))
	    (rating-set (aref *ratings* mi ri)))
	(let ((num-customers (length rating-set)))
	  (dotimes (ci num-customers)
	    (let ((c (aref rating-set ci)))
	      (declare (fixnum c))
	      (let ((p (predict mi c f)))
		(declare (single-float p))
		(let ((err (* 0.001 (- r (predict mi c f))))
		      (mv (aref *movie-values* mi f))
		      (cv (aref *customer-values* c f)))
		  (incf (aref *movie-values* mi f) (* err cv))
		  (incf (aref *customer-values* c f) (* err mv))
		  (if (and (eq mi *current-movie*)
			   (eq c *current-customer*))
		      (format t "~a ~a ~a ~a ~a ~a ~a ~a~%" mv cv
			      err (* 0.001 (- r (predict mi c f)))
			      p (predict mi c f)
			      (aref *movie-values* mi f)
			      (aref *customer-values* c f))))))))))))


(defun iterate-ratings (&optional (max-movie-id +num-movies+))
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (fixnum max-movie-id))
  (let ((count 0) (sum 0.0))
    (declare (fixnum count) (single-float sum))
    (dotimes (mi max-movie-id)
      (dotimes (ri +num-ratings+)
	(let ((r (+ ri 1))
	      (rating-set (aref *ratings* mi ri)))
	  (let ((num-customers (length rating-set)) (f 0))
	    (dotimes (ci num-customers)
	      (let ((err (* 0.001 (- r (predict mi ci 0))))
		    (mv (aref *movie-values* mi f))
		    (cv (aref *customer-values* ci f)))
		(incf (aref *movie-values* mi f) (* err cv))
		(incf (aref *customer-values* ci f) (* err mv))
		(incf count)
		(values count sum)))))))))

(defun init-movie-averages ()
  (dotimes (m +num-movies+)
    (setf (aref *movie-averages* m) (movie-avg (get-movie (+ 1 m))))))

(defun init-customer-average-offsets (&optional (max-movie-id +num-movies+))
  (let ((counts (make-array +num-customers+ :element-type 'fixnum :initial-element 0))
	(sums (make-array +num-customers+ :element-type 'single-float :initial-element 0.0)))
    (dotimes (mi max-movie-id)
      (let ((m-avg (movie-avg (get-movie (+ mi 1)))))
	(declare (single-float m-avg))
	(dotimes (ri +num-ratings+)
	  (let ((r (+ ri 1))
		(rating-set (aref *ratings* mi ri)))
	    (let ((num-customers (length rating-set)))
	      (dotimes (ci num-customers)
		(let ((c (svref rating-set ci)))
		  (incf (aref counts c))
		  (incf (aref sums c) (- r m-avg)))))))))
    (dotimes (c +num-customers+)
      (let ((count (aref counts c))
	    (sum (aref sums c)))
	(setf (aref *customer-average-offsets* c) (/ sum count))))))

(declaim (ftype (function (fixnum fixnum &optional fixnum) single-float) predict)
	 (inline predict))

(defun predict (m c &optional (max-f (- +num-factors+ 1)))
  (declare (optimize (speed 3) (safety 0)) (inline))
  (declare (fixnum m c max-f))
  (let ((s (predict-baseline m c)))
    (declare (single-float s))
    (dotimes (f (+ max-f 1))
      (incf s (* (aref *movie-values* m f)
		 (aref *customer-values* c f))))
    s))

(declaim (ftype (function (fixnum fixnum) single-float) predict-baseline)
	 (inline predict-baseline))

(defun predict-baseline (m c)
  (declare (optimize (speed 3) (safety 0)) (inline))
  (declare (fixnum m c))
  (the single-float (+ (the single-float (aref *movie-averages* m))
		       (the single-float (aref *customer-average-offsets* c)))))

(defun training-set-average (id)
  (average (mapcar #'second (training-set id))))

(defun sort-ratings (ratings)
  (let ((v (make-array 5 :initial-element nil)))
    (dolist (r ratings)
      (push (car r) (svref v (- (nth 1 r) 1))))
    v))

(defun make-rating (f)
  (list (parse-integer (nth 0 f))
	(if (not (string= "NULL" (nth 1 f))) (parse-integer (nth 1 f)))
	(nth 2 f)))

(defun customer-value (c f)
  (svref (gethash c *customer-values*) f))

(defun set-customer-value (c f v)
  (setf (svref (gethash c *customer-values*) f) v))

(defun movie-value (m f)
  (svref (gethash m *movie-values*) f))

(defun set-movie-value (m f v)
  (setf (svref (gethash m *movie-values*) f) v))




;;;; -----------------------------------------------------------------
;;;; probe stuff
;;;; -----------------------------------------------------------------

(defparameter *probe-file* (format nil "~a/probe.txt" *download-dir*))

(defstruct probe-rec movie-id customer-id rating prediction)

(defvar *probe* ())

(defun load-probe-file ()
  (setf *probe* ())
  (with-open-file (in *probe-file*)
    (let ((m nil))
      (loop for line = (read-line in nil) while line do
	   (if (ends-with line ":")
	       (setq m (parse-integer line :junk-allowed t))
	       (let ((c (customer-id-index (parse-integer line))))
		 (let ((r (get-rating m c))
		       (p (predict-baseline (- m 1) c)))
		   (push (make-probe-rec :movie-id m
					 :customer-id c
					 :rating r
					 :prediction p) *probe*)))))))
  (setf *probe* (reverse *probe*)))

(defun load-probe-ratings ()
  (dolist (probe *probe*)
    (let ((m (probe-rec-movie-id probe))
	  (c (probe-rec-customer-id probe)))
    (setf (probe-rec-rating probe) (get-rating m c)))))

(defun convert-probe-to-customer-index ()
  (dolist (probe *probe* 'done)
    (setf (probe-rec-customer-id probe) (customer-id-index (probe-rec-customer-id probe)))))

(defun initialize-probe-predictions-with-movie-averages ()
  (dolist (probe *probe* 'done)
    (let ((m (probe-rec-movie-id probe)))
      (setf (probe-rec-prediction probe) (movie-avg (get-movie m))))))

(defun initialize-probe-predictions-baseline ()
  (dolist (probe *probe* 'done)
    (let ((m (probe-rec-movie-id probe))
	  (c (probe-rec-customer-id probe)))
      (setf (probe-rec-prediction probe) (predict-baseline m c)))))

(defun probe-rmse ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((sum-squares 0.0) (count 0))
    (declare (single-float sum-squares) (fixnum count))
  (dolist (probe *probe*)
    (let ((r (probe-rec-rating probe))
	  (p (probe-rec-prediction probe)))
      (declare (fixnum r) (single-float p))
      (let ((delta (- r p)))
	(declare (single-float delta))
	(incf sum-squares (expt delta 2))
	(incf count))))
  (sqrt (/ sum-squares count))))

(defun update-probe-predictions (f)
  (dolist (probe *probe*)
    (let ((m (probe-rec-movie-id probe))
	  (c (probe-rec-customer-id probe)))
      (let ((p (predict m c f)))
	(setf (probe-rec-prediction probe) p)))))

(defun round-probe-predictions ()
  (dolist (probe *probe*)
    (let ((m (probe-rec-movie-id probe))
	  (c (probe-rec-customer-id probe)))
      (let ((p (min 5 (max 1 (round (predict m c))))))
	(setf (probe-rec-prediction probe) p)))))

(defun cycle (f)
  (train f)
  (update-probe-predictions)
  (probe-rmse))



;;;; -----------------------------------------------------------------
;;;; reporting subsystem
;;;; -----------------------------------------------------------------

(defstruct movie-row id title f1 avg delta)

(defvar *movie-report* ())

(defun load-movie-report ()
  (setf *movie-report* ())
  (dolist (movie *movie-list*)
    (let ((id (movie-id movie)))
      (let ((f1 (svref (gethash id *movie-values*) 0))
	    (avg (movie-avg movie)))
      (push (make-movie-row :id id :title (title id) :f1 f1 :avg avg :delta (- f1 avg)) *movie-report*))))
  (setf *movie-report* (sort *movie-report* (lambda (a b) (< (movie-row-delta a) (movie-row-delta b))))))

(defun print-movie-report ()
  (let ((rownum 1))
    (dolist (row *movie-report*)
      (if (or (<= rownum 10)
	      (>= rownum 17761))
	  (format t "~7d: ~a (f1: ~a, avg: ~a, delta: ~a)~%"
		  rownum
		  (movie-row-title row)
		  (movie-row-f1 row)
		  (movie-row-avg row)
		  (movie-row-delta row)))
      (incf rownum))))




;;;; -----------------------------------------------------------------
;;;; binary training set quick load subsystem (obsolete)
;;;; -----------------------------------------------------------------

(defun read-quick-train-file ()
  (let ((buf (make-array (expt 2 10))))
    (with-open-file (s "/Users/Corey/Netflix/bin/ratings.dat")
      (read-sequence buf s))
    buf))

(defun read-int (v n)
  (let ((i 0))
    (dotimes (j 4)
      (setf i (dpb (char-int (svref v (+ n j))) (byte 8 (* j 8)) i))
      (format t "~a, ~a, ~a~%" (char-int (svref v (+ n j))) i j))
    i))

(defun rcust (n)
  (ldb (byte 19 0) n))

(defun rdate (n)
  (ldb (byte 12 19) n))



;;;; -----------------------------------------------------------------
;;;; customer index assignment
;;;;
;;;; I'm having some issues with the training runs taking too long
;;;; and I think its due to all the hash lookups for the movie and
;;;; customer factors. I think I can decrease the time required to
;;;; complete a run by converting from hash tables to vectors.
;;;;
;;;; -----------------------------------------------------------------

(defvar *original-customer-ids-sorted*
  (make-array +num-customers+ :element-type 'fixnum))

(defun load-customer-ids-from-customer-values-hash ()
  (setf *original-customer-ids-sorted*
	(sort (list->vector (hash-keys *customer-values*)) #'<)))

(defun load-customer-ids ()
  (setf *original-customer-ids-sorted* (read-file "customer-ids.txt")))

(defvar *original-customer-id-to-index-hash*
  (make-hash-table :test 'equal :size +num-customers+))

(defun init-original-customer-id-to-index-hash (i +num-customers+)
    (setf (gethash (svref *original-customer-ids-sorted* i) *original-customer-id-to-index-hash*) i))

(defun customer-id-index (id)
  (gethash id *original-customer-id-to-index-hash*))

(defun customer-index-id (index)
  (svref *original-customer-ids-sorted* index))

(defun convert-training-set ()
  (dotimes (m +num-movies+)
    (dotimes (r +num-ratings+)
      (map-into (aref *ratings* m r) #'customer-id-index (aref *ratings* m r)))))



(defun startup ()
  (load-movies)
  (load-ratings)
  (load-probe-file)
  (load-customer-ids)
  (init-original-customer-id-to-index-hash))

; gb ds, sm reg, yorkie poo, b.a.b. ws penguin
; game: 
; solar jack (spin) - secret santa shop fodria (public school)
; checkers
; box of legos (i.j. boulder, crystal skull)
; treehouse


(defvar *test-movie-values* (make-array '(10 5)))

(defvar *test-customer-values* (make-array '(20 5)))

(defun init-test-values (array min max)
  (dotimes (m (array-dimension array 0))
    (dotimes (f (array-dimension array 1))
      (setf (aref array m f) (random-min-max min max))))
  array)

(defun random-min-max (min max)
  (+ min (random (- (+ max 1) min))))

(defun matrix-multiply (mv cv)
  (let ((nm (array-dimension mv 0))
	(nc (array-dimension cv 0))
	(nf (min (array-dimension mv 1) (array-dimension cv 1))))
    (let ((a (make-array (list nc nm) :initial-element 0)))
      (dotimes (c nc)
	(dotimes (m nm)
	  (dotimes (f nf)
	    (incf (aref a c m) (* (aref mv m f) (aref cv c f))))))
      a)))

(defun matrix-average (mv cv)
  (let ((nm (array-dimension mv 0))
	(nc (array-dimension cv 0))
	(nf (min (array-dimension mv 1) (array-dimension cv 1))))
    (let ((a (make-array (list nc nm) :initial-element 0)))
      (dotimes (c nc)
	(dotimes (m nm)
	  (dotimes (f nf)
	    (incf (aref a c m) (* (aref mv m f) (aref cv c f))))))
      a)))

(defun test-rate (m c)
  (let ((r 0))
    (dotimes (f (array-dimension *test-movie-values* 1))
      (incf r (* (aref *test-movie-values* m f) (aref *test-customer-values* c f))))
    r))

(defun test-predict (m c &optional (max-f (array-dimension *test-movie-factors* 1)))
  (let ((p 0))
    (dotimes (f max-f)
      (incf p (* (aref *test-movie-factors* m f) (aref *test-customer-factors* c f))))
    p))

(defun test-results ()
  (let ((a (matrix-multiply *test-movie-factors* *test-customer-factors*)))
    (dotimes (c (array-dimension a 0))
      (dotimes (m (array-dimension a 1))
	(setf (aref a c m) (round (aref a c m)))))
    (list *test-movie-values* *test-movie-factors*
	  *test-customer-values* *test-customer-factors*
	  *test-multiply* a)))

(defun test-train (f)
  (dotimes (m (array-dimension *test-movie-values* 0))
    (dotimes (c (array-dimension *test-customer-values* 0))
      (if (aref *test-ratings-partial* c m)
	  (let ((err (* 0.001 (- (test-rate m c) (test-predict m c f)))))
	    (let ((temp (aref *test-movie-factors* m f)))
	      (incf (aref *test-movie-factors* m f) (* err (aref *test-customer-factors* c f)))
	      (incf (aref *test-customer-factors* c f) (* err temp)))))))
  (list *test-movie-factors* *test-customer-factors*)
  (test-probe f))

(defun test-train-full ()
  (init-test-factors)
  (dotimes (epoch 1)
    (dotimes (f (array-dimension *test-movie-values* 1))
      (test-train f)))
  (test-results)
  (test-probe))

(defvar *test-probe*)

(defun init-test-probe ()
  (setf *test-probe* (make-array (array-dimensions *test-multiply*) :initial-element nil))
  (dotimes (i (array-dimension *test-multiply* 0))
    (dotimes (j (array-dimension *test-multiply* 1))
      (if (not (aref *test-ratings-partial* i j))
	  (setf (aref *test-probe* i j)
		(if (eq 1 (random 2))
		    (aref *test-multiply* i j)
		    nil)))))
  *test-probe*)

(defun test-probe (&optional (max-f (array-dimension *test-movie-factors* 1)))
  (let ((sum 0) (count 0))
    (dotimes (c (array-dimension *test-probe* 0))
      (dotimes (m (array-dimension *test-probe* 1))
	(let ((r (aref *test-probe* c m)))
	  (when r
	    (incf sum (expt (- r (test-predict m c max-f)) 2))
	    (incf count)))))
    (float (sqrt (/ sum count)))))
  
(defun test-probe-2 (&optional (max-f (array-dimension *test-movie-factors* 1)))
  (let ((sum 0) (count 0))
    (dotimes (c (array-dimension *test-probe* 0))
      (dotimes (m (array-dimension *test-probe* 1))
	(let ((r (aref *test-probe* c m)))
	  (when r
	    (incf sum (expt (- r (aref *test-multiply* c m)) 2))
	    (incf count)))))
    (float (sqrt (/ sum count)))))
  

(defvar *test-multiply*)

(defun init-test-multiply ()
  (setf *test-multiply* (matrix-average *test-movie-values* *test-customer-values*)))

(defvar *test-ratings*)

(defvar *test-ratings-partial*)

(defun init-test-ratings-partial ()
  (setf *test-ratings-partial* (make-array (array-dimensions *test-multiply*)))
  (dotimes (i (array-dimension *test-multiply* 0))
    (dotimes (j (array-dimension *test-multiply* 1))
      (setf (aref *test-ratings-partial* i j)
	    (if (< 5 (random 10))
		(aref *test-multiply* i j)
		nil))))
  *test-ratings-partial*)

(defvar *test-movie-factors*)
(defvar *test-customer-factors*)

(defun init-test-factors ()
  (setf *test-movie-factors* (make-array (array-dimensions *test-movie-values*) :initial-element 1.0)
	*test-customer-factors* (make-array (array-dimensions *test-customer-values*) :initial-element 1.0))
  (list *test-movie-factors* *test-customer-factors*))


(defun init-all-test-values ()
  (init-test-values *test-movie-values* 0 2)
  (init-test-values *test-customer-values* -2 2)
  (init-test-multiply)
  (list *test-movie-values*
	*test-customer-values*
	*test-multiply*))

(defun rate (c m)
  (case (aref *test-movie-values* m 0)
    (0 0)
    (1 4)
    (2 5)))

(defun copy-matrix (a b)
  (dotimes (i (array-dimension a 0))
    (dotimes (j (array-dimension a 1))
      (setf (aref b i j) (aref a i j)))))

(defun copy-factors ()
  (copy-matrix *test-movie-values* *test-movie-factors*)
  (copy-matrix *test-customer-values* *test-customer-factors*))

(defun movie-rating-sets (m)
  (loop for r from 0 to 4 collect
       (let ((rs (movie-rating-set m r)))
	 (if (vectorp rs) rs (vector rs)))))

(defstruct kb mf cf)

(defun customer-ratings-titles (c)
  (let ((ratings ()))
    (dotimes (m (array-dimension *ratings* 0))
      (dotimes (r (array-dimension *ratings* 1))
	(if (binary-search c (aref *ratings* m r))
	    (push (list (+ m 1) (title (+ m 1)) (+ r 1)) ratings))))
    (reverse ratings)))

(defun customer-ratings (c)
  (let ((ratings ()))
    (dotimes (m (array-dimension *ratings* 0))
      (dotimes (r (array-dimension *ratings* 1))
	(if (binary-search c (aref *ratings* m r))
	    (push (list (+ m 1) (+ r 1)) ratings))))
    (reverse ratings)))

(defun group-ratings (m)
  (mapcar #'length (movie-rating-sets m)))

(defun vector-intersect (a b)
  (let ((d (make-array 0 :adjustable t :fill-pointer t)))
    (do ((i 0 (if (<= (aref a i) (aref b j)) (+ i 1) i))
	 (j 0 (if (<= (aref b j) (aref a i)) (+ j 1) j)))
	((or (>= i (length a)) (>= j (length b))) d)
      (if (= (aref a i) (aref b j))
	  (vector-push-extend (aref a i) d)))))

(defun find-movies-by-title (title)
  (loop for id from 1 to +num-movies+
       when (search (string-downcase title) (string-downcase (title id)))
       collect (get-movie id)))

; (#S(MOVIE :ID 3938 :YEAR 2004 :TITLE "Shrek 2" :AVG 4.1445427))
; customer 4

(defun movie-customers (m)
  (let ((a (make-array 0 :adjustable t :fill-pointer t)))
    (dotimes (r 5)
      (let ((rs (movie-rating-set m r)))
	(dotimes (ci (length rs))
	  (vector-push-extend (svref rs ci) a))))
    (sort a #'<)))

(defun stddev (a)
  (let ((sum-squares (reduce #'+ (mapcar (lambda (x) (* x x)) a)))
	(sum (reduce #'+ a))
	(n (length a)))
    (sqrt (- (/ sum-squares n) (/ sum (* n n))))))

(defun predict (m c d)
  (declare (ignore m c d))
  3.0)
	 

(defun create-submission-file ()
  (with-open-file (in (download-filename "qualifying.txt"))
    (with-open-file (out (project-filename "out/submission.txt")
			 :direction :output :if-exists :supersede)
      (do ((line (read-line in nil nil) (read-line in nil nil))
	   (m))
	  ((not line))
	(cond ((ends-with line ":")
	       (setq m (subseq line 0 (- (length line) 1)))
	       (format out "~a~%" line))
	      (t (let ((fields (split line)))
		   (let ((c (parse-integer (first fields)))
			 (d (second fields)))
		     (format out "~,4f~%" (predict m c d))))))))))

(defun global-average ()
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (do ((m 1 (+ 1 m))
       (sum 0)
       (count 0))
      ((> m 17770) (float (/ (float sum) (float count))))
    (declare (fixnum m sum count))
  (let ((filename (format nil "~a/training_set/mv_~7,'0d.txt" *download-dir* m)))
    (with-open-file (s filename)
      (read-line s nil nil)
      (do ((line (read-line s nil nil) (read-line s nil nil)))
	  ((null line) nil)
	(let ((fields (split line)))
	  (let ((r (parse-integer (second fields))))
	    (declare (fixnum r))
	    (incf count)
	    (incf sum r))))
      (format t "~a ~a ~a ~a~%" m count sum (float (/ (float sum) (float count))))))))

(defun global-average-alt ()
  (with-open-file (in "/Users/Corey/Netflix/out/test.txt")
    (do ((line (read-line in nil nil) (read-line in nil nil))
	 (count 0 (+ 1 count))
	 (sum 0))
	((null line)
	 (float (/ sum count)))
      (if (not (ends-with line ":"))
	  (let ((fields (split line)))
	    (let ((r (parse-integer (second fields))))
	      (format t "~a~%" line)
	      (incf sum r)))))))

(defun iterate-all-ratings ()
  (with-open-file (out "/Users/Corey/Netflix/out/test.txt" :direction :output :if-exists :supersede)
    (dolist (file (directory (make-pathname :name :wild :type :wild :defaults "/Users/Corey/Netflix/download/training_set/")))
      (with-open-file (in file)
	(do ((line (read-line in nil nil) (read-line in nil nil))) ((null line))
	  (format out "~a~%" line))))))

(defun write-u4 (out v)
  (write-byte (ldb (byte 8 24) v) out)
  (write-byte (ldb (byte 8 16) v) out)
  (write-byte (ldb (byte 8 8) v) out)
  (write-byte (ldb (byte 8 0) v) out))

(defun read-u4 (in)
  (let ((v 0))
    (setf (ldb (byte 8 24) v) (read-byte in))
    (setf (ldb (byte 8 16) v) (read-byte in))
    (setf (ldb (byte 8 8) v) (read-byte in))
    (setf (ldb (byte 8 0) v) (read-byte in))
    v))

(defun test-write ()
  (with-open-file (out "test.dat"
		       :direction :output
		       :if-exists :supersede
		       :element-type 'unsigned-byte)
    (write-u4 out 1234567890)
    (write-u4 out 42)
    (write-u4 out 11)
    (write-u4 out 42.38290)
    (write-u4 out 30)
    (write-u4 out 8)
    (write-u4 out 5))
  nil)

(defun test-read ()
  (with-open-file (in "test.dat"
		      :element-type 'unsigned-byte)
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in))
    (format t "~a~%" (read-u4 in)))
  nil)


(defstruct rating m c d r)

(defun create-rating (m c d r)
  (make-rating :m m :c c :d d :r r))

(defun create-rating-from-line (m line)
  (let ((fields (split line)))
    (let ((c (parse-integer (car fields)))
	  (r (parse-integer (cadr fields)))
	  (d (caddr fields)))
      (create-rating m c d r))))

(defun load-ratings (movie-id)
  (with-open-file (in (training-set-filename movie-id))
    (do ((line (read-line in nil nil) (read-line in nil nil))
	 (ratings)
	 (ratings-tail))
	((null line) ratings)
      (if (not (ends-with line ":"))
	  (let ((record (create-rating-from-line movie-id line)))
	    (if (not ratings)
		(setq ratings-tail (setq ratings (cons record nil)))
		(progn (rplacd ratings-tail (cons record nil))
		       (setq ratings-tail (cdr ratings-tail)))))))))

(defun load-ratings-hash (m)
  (let ((ratings (load-ratings m))
	(h (make-hash-table :test 'equal)))
    (dolist (rating ratings h)
      (let ((c (rating-c rating))
	    (r (rating-r rating)))
	(setf (gethash c h) r)))))

(defparameter *pulp-fiction-h* (load-ratings-hash 11064))

(defun compare-movies (m1 m2)
  (let ((m1h *pulp-fiction-h*)
	(m2h (load-ratings-hash m2))
	(h (make-hash-table :test 'equal)))
    (maphash
     (lambda (c v1)
       (let ((v2 (gethash c m2h nil)))
	 (if v2
	     (setf (gethash c h) (abs (- v1 v2))))))
     m1h)
    (let ((seq))
      (maphash
       (lambda (c dr)
;	 (push (list c (gethash c m1h) (gethash c m2h) dr) seq))
;	 (push dr seq))
	 (push (list (gethash c m1h) (gethash c m2h)) seq))
       h)
      (sort (sort seq #'< :key #'cadr) #'< :key #'car))))


(defun corr (m1 m2)
  (let ((mx (compare-movies m1 m2)))
    (let ((xs (mapcar #'car mx))
	  (ys (mapcar #'cadr mx)))
      (let ((n 0)
	    (sumxy 0)
	    (sumx 0)
	    (sumy 0)
	    (sumx2 0)
	    (sumy2 0))
	(mapcar
	 (lambda (x y)
	   (incf n)
	   (incf sumxy (* x y))
	   (incf sumx x)
	   (incf sumy y)
	   (incf sumx2 (* x x))
	   (incf sumy2 (* y y)))
	 xs ys)
	(/ (- (* n sumxy) (* sumx sumy))
	   (* (sqrt (- (* n sumx2) (* sumx sumx)))
	      (sqrt (- (* n sumy2) (* sumy sumy)))))))))

(defun neighbors (m k)
  (let ((all ()))
    (do ((n 1 (+ n 1))) ((> n 17770))
      (if (not (equal m n))
	  (let ((p (corr m n)))
	    (push (list n p) all)
	    (format t "~a ~a ~a~%" m n p))))
    (setq all (sort all :#< :key #'cadr))
    (loop for i from 1 to k for x in all collect x)))
	       

(defun first-k (seq k)
  (loop for i from 1 to k))


(defun compute-neighbors (training-data)
  )

(with-open-file (out "temp.dat"
		     :direction :output
		     :if-exists :supersede
		     :element-type 'single-float)
  (write-byte 3.4 out))

(defun count-knn-full (n)
  (declare (optimize (safety 0) (debug 0) (speed 3))
	   (fixnum n))
  (do ((i 1 (+ i 1))
       (c 0))
      ((> i n) c)
    (declare (fixnum i c))
    (do ((j (+ i 1) (+ j 1)))
	((> j n))
      (declare (fixnum j))
      (incf c))))

(defun test-loading-all-via-hash ()
  (do ((m 1 (+ 1 m)) (c 0)) ((> m 17770) c)
    (declare (fixnum m c))
    (let ((h (load-ratings-hash m)))
      (incf c (hash-table-size h)))))

(defun movie-ratings (m)
  "Load all the ratings for the movie m as a simple list of just the ratings numbers."
  (with-open-file (in (training-set-filename m))
    (read-line in nil nil) ; skip first line
    (do ((line (read-line in nil nil) (read-line in nil nil))
	 (ratings ())
	 (n 0 (+ n 1)))
	((null line) (make-array n :initial-contents (sort ratings #'<) :element-type 'fixnum))
      (declare ((or null string) line))
      (let ((fields (split line)))
	(let ((rating (parse-integer (cadr fields))))
	  (setq ratings (cons rating ratings)))))))

(defun movie-ratings-alt (m)
  "Load all the ratings for the movie m as a simple list of just the ratings numbers."
  (with-open-file (in (training-set-filename m))
    (read-line in nil nil) ; skip first line
    (do ((line (read-line in nil ".") (read-line in nil "."))
	 (ratings ())
	 (n 0 (+ n 1)))
	((string= "." line) n)
      (declare (string line)))))

;(defun store (obj place)
;  (cl-store:store obj place))

;(defun restore (place)
;  (cl-store:restore place))


(defun movie-average (m)
  (let ((sum 0) (count 0))
    (declare (fixnum sum count))
    (dolist (rating (movie-ratings m) (float (/ sum count)))
      (incf sum rating)
      (incf count))))

(defun integer-range (m n)
  (do ((i m (+ i 1))
       (r () (cons i r)))
      ((> i n) (nreverse r))))

(defstruct stat n min max mean median mode stddev n1 n2 n3 n4 n5)

(defun stat->list (instance)
  (struct->list instance stat (n min max mean median mode stddev n1 n2 n3 n4 n5)))

(defmacro struct->list (instance-form struct fns)
  (let ((instance (gensym)))
    (let ((struct-fns (mapcar (lambda (fn) (struct-function-symbol struct fn)) fns)))
      `(let ((,instance ,instance-form))
	 (list ,@(mapcar (lambda (fn) `(,fn ,instance)) struct-fns))))))

(defun stat->line (instance)
  (format nil "~{~a~^,~}" (stat->list instance)))

(defmacro list->struct (lst-form struct fns)
  (let ((lst (gensym))
	(c 0)
	(constructor (find-symbol (format nil "MAKE-~a" struct)))
	(args (mapcar (lambda (fn) (find-symbol (symbol-name fn) (find-package 'keyword))) fns)))
    (let ((lambda-lst ()))
      (dolist (arg args)
	(push arg lambda-lst)
	(push `(nth ,c ,lst) lambda-lst)
	(incf c))
      `(let ((,lst ,lst-form))
	 (,constructor ,@(reverse lambda-lst))))))  

(defun line->stat (line)
  (list->struct (split line) stat (n min max mean median mode stddev n1 n2 n3 n4 n5)))

(defun collect-movie-stats (movies)
  (with-open-file (out "/Users/Corey/Netflix/data/movie-stats.txt"
		       :direction :output :if-exists :supersede)
    (dolist (m movies)
      (format out "~a,~a~%" m (stat->line (stats (movie-ratings m)))))))

(defun struct-function (s f)
  (symbol-function (find-symbol (format nil "~a-~a" s f))))

(defun struct-function-symbol (s f)
  (find-symbol (format nil "~a-~a" s f)))



(defun sum (data)
  (declare ((vector fixnum) data))
  (let ((n (length data)))
    (declare (fixnum n))
    (do ((i 0 (+ i 1))
	 (s 0))
	((>= i n) s)
      (declare (fixnum i s))
      (let ((x (aref data i)))
	(declare (fixnum x))
	(incf s x)))))

(declaim (inline sum))

(defun stats (data)
  (declare ((vector fixnum) data))
  (let ((n (length data))
	(min (reduce #'min data))
	(max (reduce #'max data))
	(sum (sum data))
	(freq (freq data)))
    (declare (fixnum n min max sum))
    (let ((mean (float (/ sum n)))
	  (median (aref data (floor (/ n 2))))
	  (mode (mode data)))
      (declare (fixnum median mode) (single-float mean))
      (let ((stddev (stddev data mean n)))
	(declare (single-float stddev))
	(make-stat :n n :min min :max max :mean mean
		   :median median :mode mode :stddev stddev
		   :n1 (or (cdr (assoc 1 freq)) 0)
		   :n2 (or (cdr (assoc 2 freq)) 0)
		   :n3 (or (cdr (assoc 3 freq)) 0)
		   :n4 (or (cdr (assoc 4 freq)) 0)
		   :n5 (or (cdr (assoc 5 freq)) 0))))))

(declaim (inline stddev))

(defun mean (data)
  (let ((n (length data)))
    (declare (fixnum n))
    (do ((i 0 (+ i 1))
	 (s 0))
	((>= i n) (the float (float (/ s n))))
      (incf s (aref data i)))))

(defun stddev (data mean n)
  (declare ((vector fixnum) data)
	   (single-float mean)
	   (fixnum n))
  (let ((nf (float n)))
    (declare (single-float nf))
    (do ((i 0 (+ i 1))
	 (s 0.0))
	((>= i n) (sqrt (/ s nf)))
      (declare (fixnum i) (single-float s))
      (let ((x (float (aref data i))))
	(declare (single-float x))
	(incf s (expt (- x mean) 2))))))

  
       
	  
(defun mode (data)
  (car (car (sort (freq data) #'> :key #'cdr))))

(defun freq (data)
  (let ((h (make-hash-table :test 'equal)))
    (do ((i 0 (+ i 1)))
	((>= i (length data)))
      (let ((x (aref data i)))
	(declare (fixnum x))
	(incf (gethash x h 0))))
    (let ((f ()))
      (maphash (lambda (k v) (push (cons k v) f)) h)
      (sort f #'< :key #'car))))
  
(defun write-movie-averages (filename movies)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dolist (m movies)
      (let ((a (movie-average m)))
	(format out "~a ~a~%" m a)))))

(defstruct sample movie customer date rating)

(defun load-ratings (m)
  (with-open-file (in (training-set-filename m))
    (read-line in nil nil)
    (do ((line (read-line in nil nil) (read-line in nil nil))
	 (ratings ())
	 (n 0 (+ n 1)))
	((null line) (sort (make-array n :element-type 'sample :initial-contents ratings)
			   #'< :key #'sample-customer))
      (let ((fields (split line)))
	(push (make-sample :movie m
			   :customer (parse-integer (nth 0 fields))
			   :rating (parse-integer (nth 1 fields))
			   :date (nth 2 fields))
	      ratings)))))

(defun quick-load-filename (m)
  (format nil "/Users/Corey/Netflix/data/mv_~7,'0d.dat" m))

(defun convert-to-quick (m)
  (store (load-ratings m) (quick-load-filename m)))

(defun customer-ratings-filename (c)
  (format nil "/Users/Corey/Netflix/data/customers/cu_~7,'0d.txt" c))

(defun make-customer-files (movies)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (m movies h)
      (with-open-file (in (training-set-filename m))
	(read-line in nil nil)
	(do ((line (read-line in nil nil) (read-line in nil nil)))
	    ((null line) nil)
	  (let ((fields (split line)))
	    (let ((c (parse-integer (nth 0 fields)))
		  (r (parse-integer (nth 1 fields)))
		  (d (nth 2 fields)))
	      (push (format nil "~a,~a,~a" m r d) (gethash c h ())))))))
    (maphash (lambda (c lines)
	       (with-open-file (out (customer-ratings-filename c)
				    :direction :output :if-exists :append)
		 (dolist (line (reverse lines))
		   (format out "~a~%" line))))
	     h)))
	       