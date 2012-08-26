(defstruct agent svd knn)
(defstruct svd mf cf)
(defstruct knn dd)

(defun train (agent samples))

(defun predict (agent m c d))

(defun probe (agent samples))

(defun submit (agent filename))

(defmacro dosamples ((m r c data) &body body)
  (let ((nm (gensym)) (nr (gensym)) (cdata (gensym)) (nc (gensym)) (ci (gensym)))
    `(let ((,nm (array-dimension ,data 0))
	  (,nr (array-dimension ,data 1)))
      (dotimes (,m ,nm)
	(dotimes (,r ,nr)
	  (let ((,cdata (aref ,data ,m ,r)))
	    (let ((,nc (array-dimension ,cdata 0)))
	      (dotimes (,ci ,nc)
		(let ((,c (aref ,cdata ,ci)))
		  ,@body)))))))))

