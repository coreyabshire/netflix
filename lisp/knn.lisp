(defstruct item id ratings)
(defstruct kb items)

(defun knn (k seq dfn)
  "Find the k nearest neighbors by distance function dfn in seq."
  (subseq (sort (copy-seq seq) #'< :key dfn) 0 k))

(defun item-corr (a b)
  "Find the correlation coefficient of two items."
  (

(defun corr-coef ()
  "Find the correlation coefficient wrt a and b."
  ())

(defun pearson (points)
  (loop for p in points
	for x = (car p)
	for y = (cdr p)
	sum x into sx
	sum y into sy
	sum (* x y) into sxy
	sum (* x x) into sxx
	sum (* y y) into syy
	count p into n
	finally (return (/ (- (* n sxy) (* sx sy))
			   (* (sqrt (- (* n sxx) (* sx sx)))
			      (sqrt (- (* n syy) (* sy sy))))))))

(let ((x (list 52 49 26 28 63 44 70 32 49 51))
      (y (list 48 49 27 24 59 40 72 31 50 49)))
  (corr (pairlis x y)))

(let ((a '(1 ((1 . 4) (2 . 4) (3 . 5))))
      (b '(2 ((2 . 4) (3 . 5) (4 . 4)))))
  (pearson (matchup (cadr a) (cadr b))))

(defun read-item (i)
  (with-open-file (in (format nil "/Users/Corey/Netflix/download/training_set/mv_~7,'0d.txt" i))
    (read-line in)
    (loop for line = (read-line in nil nil) while line
	  for pos1 = (position #\, line :start 0)
	  for pos2 = (position #\, line :start (+ pos1 1))
	  for c = (parse-integer (subseq line 0 pos1))
	  for r = (parse-integer (subseq line (+ pos1 1) pos2))
	  collect (cons c r))))

(defun matchup (list1 list2)
  (loop for item in (intersection (mapcar #'car list1) (mapcar #'car list2))
	collect (cons (cdr (assoc item list1)) (cdr (assoc item list2)))))

(let ((list1 '((1 . 4) (2 . 3) (3 . 4) (4 . 5)))
      (list2 '((2 . 3) (3 . 4) (4 . 5) (5 . 3))))
  (pearson (matchup list1 list2)))

(pearson (matchup (read-item 1) (read-item 4)))

(loop with kb = *kb*
      with i = 1 with ri = (gethash i kb)
      for j from 2 upto 10 for rj = (gethash j kb)
      for points = (matchup ri rj)
      collect (cons j (pearson points)))

(defstruct kb by-item by-user)

(defun load-kb ()
  (loop with by-item = (make-hash-table :test #'equal :size 100)
	with by-user = (make-hash-table :test #'equal)
	for i from 1 to 100
	for ri = (read-item i)
	do (setf (gethash i by-item) ri)
	do (loop for r in ri do (push i (gethash (car r) by-user ())))
	finally (return (make-kb :by-item by-item :by-user by-user))))

(defun rated-by-user (u kb)
  (gethash u (kb-by-user kb)))

(defun ratings-of-item (i kb)
  (gethash i (kb-by-item kb)))

(defun make-dfn (i fn kb)
  (let ((ri (ratings-of-item i kb)))
    (lambda (j)
      (let ((rj (ratings-of-item j kb)))
	(funcall fn (matchup ri rj))))))
    

(defun predict (u i k kb fn)
  "Predict rating by user u of item i given knowledge in kb."
  (loop for m in (knn k (rated-by-user u kb) (make-dfn i fn kb))
	collect m))

(let ((h (let ((h (make-hash-table :test #'equal :size 10)))
	   (dotimes (n 10 h) (setf (gethash n h) (* n n))))))
  (loop for k being the hash-keys of h using (hash-value v)
	collect (cons k v)))


  (let ((iv (rated-by-user u kb)))
    (let ((nn (knn k seq (lambda (j) (- 1 (pearson i j))))))
      (loop for vi in (union iv nn) average (rating-for u vi)))))

(loop with h = (kb-by-user *kb*)
      with maxn = 0 with maxk = nil
      for k being the hash-keys of h
      for v being the hash-values of h
      for n = (length v)
      when (> n maxn) do (setq maxn n maxk k)
      finally (return (cons maxk maxn)))
      