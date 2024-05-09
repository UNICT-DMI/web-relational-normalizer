(defun check-decomposition (schema decomposition &optional st1) ; -> set-rel-schema
  "check if decomposition has all the attributes of schema"
  (declare (type schema schema) (type schema-set decomposition))
  (declare (ignore st1))
  (let ((attrschema (sch-att-s schema))
	(other-attrs (sch-s-att-s decomposition))
	(contained-in-others nil))
    (values (and (subsetp attrschema other-attrs)
		 (subsetp other-attrs attrschema)
		 (loop for (sc . res) on  (sort (copy-list decomposition) #'< :key #'(lambda(s) (length (sch-att-s s))))
		    never (some (lambda(x) (when (subsetp (sch-att-s sc) (sch-att-s x)) (pushnew sc contained-in-others))) res)))
	    contained-in-others)))

(defun check-lossless (schema decomposition)
  "test if a decomposition is lossless or not"
  (declare (type schema schema) (type schema-set decomposition))
  (unless (check-decomposition schema decomposition)
    (error "trying to check the lossless property for an invalid decomposition"))
  (make-chase schema decomposition))


(defun make-random-schema (n m)
  (mk-sch 'r (mk-att-s (loop for i from 1 to n collect (symb 'a i)))
	  (mk-f-dep-s (loop for i from 1 to m
			 collect
			   (mk-f-dep (loop for i from 1 to (1- n)
					collect (symb 'a (random n)))
				     (loop for i from 1 to (1- n)
					collect (symb'a (random n))))))))

(defun test(a n m)
	  (let* ((sch (make-random-schema n m))
		 (att1 (sch-att-s sch))
		 (fd1 (sch-f-dep-s sch))
		 (sub1 (subseq att1 0 a)))
	    (print "attr-set-closure")
	    (time (attr-set-closure sub1 fd1))
	    (print "attr-set-closure2")
	    (time (attr-set-closure2 sub1 fd1))
	    (print "attr-set-closure3")
	    (time (attr-set-closure3 sub1 fd1))
	    "Done!"))

(defun attr-set-closure2 (x f &optional st1) ; -> set-attribute
  "Compute the closure of a set of attributes X with respect to the functional dependencies F with the efficient method of Beeri and Bernstein (1979)"
  (declare (type attribute-set x) (type functional-dependency-set f) (type mystream st1))
  (let* ((clo x)
	 (update x)
	 (list (make-hash-table))
	 (fds (apply #'vector f))
	 (count (make-array (length f) :initial-element 0)))
    (expl st1 "Starting computing the closure of (~/fundep:att-s/) with respect to:~%~%~/fundep:f-dep-s/:~%~%" x f)
    (loop for (a b) in f
	 for i from 0
       do (loop for ai in a
	     do (setf (gethash ai list) (cons i (gethash ai list)))
	     do (incf (elt count i))))
    (expl st1 "List of a[i] ~%~{~a ~a~%~}~%"
	  (loop for k being the hash-key using (hash-value v) of list collect (list k v)))
    (expl st1 "The initial closure is: ~%~%(~/fundep:att-s/)~%~%" clo)
    (loop while update
	 for a = (pop update)
	 do (loop for i in (gethash a list)
	       when (= (decf (elt count i)) 0)
	       do (loop for b in (determinate (svref fds i))
		     unless (member b clo)
		     do (push b clo)
		     and do (push b update))))
    (mk-att-s clo)))

(defun attr-set-closure3 (x f &optional st1) ; -> set-attribute
  "Compute the closure of a set of attributes X with respect to the functional dependencies F with the efficient method of Beeri and Bernstein (1979). Optimized version"
  (declare (type attribute-set x) (type functional-dependency-set f) (type mystream st1))
  (let* ((clo x)
	 (update x)
	 (len (length f))
	 (fds (make-array len))
	 (count (make-array len :initial-element 0)))
    (expl st1 "Starting computing the closure of (~/fundep:att-s/) with respect to:~%~%~/fundep:f-dep-s/:~%~%" x f)
    (loop for a in x do (setf (get a 'list) nil))
    (loop for fd in f
       for i from 0
       do (setf (aref fds i) fd)
       do (loop for ai in (determinate fd)
	     for j from 1
	     do (setf (get ai 'list) (cons i (get ai 'list)))
	     finally (setf (aref count i) j)))
    (expl st1 "List of a[i] ~%~{~a ~a~%~}~%"
	  (loop for att in x collect (list att (get att 'list))))
    (expl st1 "The initial closure is: ~%~%(~/fundep:att-s/)~%~%" clo)
    (loop while update
       for a = (pop update)
       do (loop for i in (get a 'list)
	     when (= (decf (elt count i)) 0)
	     do (loop for b in (determinate (svref fds i))
		   unless (member b clo)
		   do (push b clo)
		   and do (push b update))))
    (mk-att-s clo)))

(defun my-power (list fun) ; apply fun to all the subsets of list generate in increasing length order
  (labels ((my-combine (list len) ; call fun on all the combinations of len elements of list
	     (let ((combination (subseq list 0 len)))
	       (labels ((combine-list (c-tail o-tail)
			  (if (not c-tail)
			      (funcall fun combination)
			      (do ((tail o-tail (cdr tail)))
				  ((not tail))
				(setf (car c-tail) (car tail))
				(combine-list (cdr c-tail) (cdr tail))))))
		 (combine-list combination list)))))
    (loop for x from 0 to (length list) do (my-combine list x))))

(defun my-power-s (list seeds fun)
  (let ((ss  seeds )) ;(sort (copy-list seeds) #'<= :key #'list-length)))
    (labels ((my-combine (list len) ; call fun on all the combinations of len elements of list
	       (let ((combination (subseq list 0 len)))
		 (labels ((combine-list (c-tail o-tail)
			    (if (not c-tail)
				(funcall fun combination)
				(do ((tail o-tail (cdr tail)))
				    ((not tail))
				  (setf (car c-tail) (car tail))
				  (combine-list (cdr c-tail) (cdr tail))))))
		   (combine-list combination list)))))
      (loop for x from 0 to (length ss) do (my-combine ss x)))))

(defun project-set-fun-dep1 (t1 f1 &optional st1) ; -> set-fun-dep
  "project the functional dependencies f1 over the attributes t1"
  (declare (ignore st1))
  (let* ((attrs2 (f-dep-s-att-s f1))
	 (attrs (intersection t1 attrs2))
	 (attrslen (length attrs))
	 (res nil))
    (labels ((my-check (x)
	       (let ((xplus (attr-set-closure x f1)))
		 (loop for el in xplus
		    when (and (member el attrs) (not (member el x)))
		    do (push (mk-f-dep x (list el)) res)))))
      (loop for level from 1 to (1- attrslen)
	   do (map-combinations #'my-check attrs :length level :copy nil))
      (canonical-set (mk-f-dep-s res)))))

(defun my-combine (list len &optional (fun #'print))
  (let ((combination (subseq list 0 len)))
    (labels ((combine-list (c-tail o-tail)
	       (if (not c-tail)
		   (funcall fun combination)
		   (do ((tail o-tail (cdr tail)))
		       ((not tail))
		     (setf (car c-tail) (car tail))
		     (combine-list (cdr c-tail) (cdr tail))))))
      (combine-list combination list))))

(defun project-set-fun-dep0 (t1 f1 &optional st1) ; -> set-fun-dep ;;; quello originale !!!***
"project the functional dependencies f1 over the attributes t1"
  (declare (ignore st1))
  (let* ((attrs2 (f-dep-s-att-s f1))
	 (attrs (intersection t1 attrs2))
	 (res nil))
    (dolist (pk (powerset attrs)
	     (mk-f-dep-s
	      (remove-if-not (lambda(dep)
			       (let* ((d dep))
				 (and (subsetp (first d) attrs) (subsetp (second d) attrs))))
			     (canonical-set (mk-f-dep-s res)))))
      (push (mk-f-dep pk (attr-set-closure (mk-att-s pk) f1)) res))))

(defun project-set-fun-dep2 (z f &optional st1)
  "algoritmo dei cinesi"
  (declare (ignore st1))
  (setf f (canonical-set f))
  #?"1 Before first loop"
  (let ((kend t) d newf)
    (do () ((not kend) (canonical-set d))
      (setf newf f)
      #?"1 before first loop"
      (loop for fd in f
	 when (subsetp (attributes fd) z)
	 do (push fd d)
	 and do (setf newf (remove fd newf :test 'equal)))
      (setf f newf)
      #?"1 end first loop"
      (let ((e (set-difference (attributes f)
			       (attributes (loop for fd in f collect (determinate fd))))))
	(setf newf f)
	(loop for fd in f
	   for x = (determinant fd)
	   do #?"2 before when"
	   when (intersection (set-difference x z) e)
	   do (setf newf (remove fd newf :test 'equal)) and do #?"2 after when")
	(setf kend nil)
        #?"1 end second loop"
	(setf f newf
	      newf f)
	(loop for fd in f
	   for x = (determinant fd)
	   for a = (first (determinate fd))
	   do #?"1 before second when"
	   when (and (subsetp x z) (not (member a z)))
	   do (setf newf (remove fd newf :test 'equal))
	   and do (setf f newf newf f kend t)
	   and do #?"1 before loop inside third loop"
	   and do (loop for (n (b)) in f
		     do #?"2 before third when"
		     when (member a n)
		     do (push (mk-f-dep (union x (remove a n)) (list b)) newf)
		     and do #?"2 after third when")
	   and do (setf f newf
			newf f)
	   and do #?"1 after second when")
	(setf f newf)))))

(defun project-set-fun-dep2bis (z f &optional st1)
  "algoritmo dei cinesi"
  (declare (ignore st1))
  (setf f (canonical-set f))
  #?"1 Before first loop"
  (let ((kend t) d)
    (do () ((not kend) (canonical-set d))
      #?"1 before first loop"
      (setf f
	    (loop for fd in f
	       if (subsetp (attributes fd) z)
	       do (push fd d)
	       else collect fd))
      #?"1 end first loop"
      (let ((e (set-difference (attributes f)
			       (attributes (loop for fd in f collect (determinate fd))))))
	(setf f (loop for fd in f
		   for x = (determinant fd)
		   do #?"2 before when"
		   unless (intersection (set-difference x z) e)
		   collect fd))
	(setf kend nil)
        #?"1 end second loop"
	(setf f (do* (newf
		      (fs f (cdr fs))
		      (fd (car fs))
		      (x (determinant fd))
		      (a (first (determinate fd))))
		     ((null fs) newf)
		  #?"1 New inner loop"
		  (if (and (subsetp x z) (not (member a z)))
		      (progn
			(setf kend t)
			(loop for (n (b)) in (remove fd f :test 'equal)
			   when (member a n)
			   do (push (mk-f-dep (union x (remove a n)) (list b)) newf)))
		      (push fd newf))))))))

(defun project-set-fun-dep2 (z f &optional st1)
  "algoritmo dei cinesi"
  (declare (ignore st1))
  (setf f (canonical-set f))
  (let ((kend t) d newf e added newd fromn)
    (loop
       #?"1 before first loop"
       (setf newf nil newd nil)
       (loop for fd in f
	  if (subsetp (attributes fd) z)
	  do (push fd newd)
	  else do (push fd newf))
       #?"1 end first loop"
       (setf f newf
	     d (append d newd)
	     e (set-difference (attributes (loop for fd in f collect (determinant fd)))
			       (attributes (loop for fd in f collect (determinate fd)))))
       (setf newf (loop for fd in f
		     for x = (determinant fd)
		     unless (intersection (set-difference x z) e)
		     collect fd))
       (setf f newf kend nil)
       #?"1 end second loop, starting third one"
       (setf newf f added nil)
       (loop for fd in f
	  for x = (determinant fd)
	  for a = (first (determinate fd))
	  when (and (subsetp x z) (not (member a z)))
	  do (setf newf (remove fd newf :test 'equal)
		   kend t
		   fromn nil)
	  and do #?"2 during third loop, starting inner loop"
	  and do (loop for (n (b)) in newf
		    when (member a n)
		    do (when (equal n (list a)) (push (mk-f-dep n (list a)) fromn))
		    and do (push (mk-f-dep (union x (remove a n)) (list b)) added))
	  and do (loop for fd in fromn do (setf newf (remove fd newf :test 'equal)))
	  and do #?"2 during third loop, end of inner loop")
       (setf f (append newf added))
       #?"1 end of third loop"
       (when (not kend) (return-from project-set-fun-dep2 (canonical-set d))))))
 
(defun bcnftsou (s &optional st1)
  "algoritmo di Tsou & Fischer"
  (declare (ignore st1))
  (let ((x (sch-att-s s))
	(ns (sch-name s))
	(f (canonical-set (sch-f-dep-s s)))
	(y nil)
	(z nil)
	(res nil))
    #?"1 before loop"
    (loop while (not (set-equal x y))
       do (tagbody
	    (setf y x
		  z nil)
	   repeat
	     #?"1 before when"
	     (when (> (length y) 2)
	       (loop for a in y
		  do (loop for b in (remove a y)
			do  #?"2 inner"
			when (member a (attr-set-closure (remove b (remove a y)) f))
			do (setf y (remove b y)
				 z a)
			and do (go repeat))))
	     #?"1 after when"
	     (setf x (remove z x))
	     (push (mk-sch (genCountName ns) y nil) res)))
    res))	    
	    
