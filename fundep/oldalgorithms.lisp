(defun old3nf (S &optional st1 wpd)  ;  -> set-schemas
  "Decompose the rel-schema s in Third Normal Form. The third parameter require the
projection of the dependencies over the decomposed schemas"
  (declare (type schema S) (type mystream st1) (type t wpd))
  (let* ((f1 (sch-f-dep-s s))       ; get all the fds of the input schema
	 (ns (sch-name s))          ; get the schema name
	 (deps (canonical-set f1))  ; compute the canonical cover of the dependencies
	 (g deps)
	 (nd (list (first g)))      ; prepare for building groups, each group is “collapsed”
	 (others (rest g)))         ; into a unique dependency, so nd contains the first “group”
    (unless deps              ; if the schema is without fds, then the relations is already in 3nf
      (return-from 3nf (mk-sch-s (list s))))
    (resetCountName)          ; prepare for naming the relations generated
    (expl st1 "A canonical cover of the set dependencies is:~%~%~/fundep:f-dep-s/~%~%" deps)
    ;; now generate the groups
    (dolist (d1 others)       ; for each dependency not considered
      (block outer            ; see if its determinant is aleady present in a “group”
	(loop for d2 in nd    ; for each determinant of a group
	   when (my-set-equal (determinant d1) (determinant d2)) ; see if it is equal to lhs(d1)
	   do (nsubstitute (mk-f-dep (determinant d1) 
				       (att-s-union (determinate d1) (determinate d2)))
			   d2 nd)       ; if true add the rhs(d1) to the group
	   and do (return-from outer))  ; repeat the external loop for a new dependency d1
	(setq nd (cons d1 nd))))        ; if it is not present, add d2 as a new “group”
    (setq nd (reverse nd))              ; for aestethic reasons in the result
    (expl st1 "The groups of dependencies are:~%~%~/fundep:f-dep-s/~%~%" (mk-f-dep-s nd))
    ;; now generate the schemas from the groups
    (let* ((rho (mapcar (lambda(d)
			  (let ((newatts (mk-att-s  (att-s-union (determinant d) (determinate d)))))
			    (mk-sch (genCountName ns)
					 newatts
					  (if wpd
					      (project-set-fun-dep newatts f1)
					      (mk-f-dep-s (list d))))))
			nd))
	   ;; now remove schemas if their attributes are present in other shemas
	   (nrho (mk-sch-s (remove-duplicates (sort rho #'< :key #'(lambda(s) (length (sch-att-s s))))
					      :test #'(lambda(s1 s2) (subsetp (sch-att-s s1) (sch-att-s s2)))))))
      (when (> (length rho) (length nrho))
	(expl st1 "Elimination of relations contained in others:~%~%~/fundep:sch-s/~%~%" nrho))
      ;; if the key is not present then add a schema with the key in the result
      (let ((res (mk-sch-s (if (some (lambda(x) (is-superkey (sch-att-s x) s)) nrho)
			       nrho
			       (append nrho (list (mk-sch (genCountName ns)
							  (find-a-key s) nil)))))))
	(expl st1 "The final decomposition is:~%~%~/fundep:sch-s/~%~%" res)
	res))))

