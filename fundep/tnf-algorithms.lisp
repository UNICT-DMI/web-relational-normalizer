;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 20015-2016, Renzo Orsini.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



;;; This file contains the main algorithms for functional dependencies
;;; and normalization

(in-package #:fundep)

;(declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun is-3nf (S &optional st1) ; -> bool
  "Check if rel-schema S is in third normal form"
  (declare (type schema S) (type mystream st1))
  (let* ((res t)
	 (primes (find-primes s))
	 (f1 (sch-f-dep-s s)))
    (dolist (dep (canonical-set f1) res)
      (when (and (not (is-superkey (determinant dep) s))
		 (not (member (first (determinate dep)) primes)))
	(setq res nil)
	(expl st1 "The determinant of ~/fundep:f-dep/ is not a superkey and ~A is not prime~%~%" dep  (first (determinate dep)))))))


(defun my-set-equal (s1 s2)
  "check if two sets are equal"
  (declare (type list s1 s2))
  (and (subsetp s1 s2) (subsetp s2 s1)))

(defun 3nf (S &optional st1 wpd)  ;  -> set-schemas
  "Decompose the rel-schema s in Third Normal Form. The third parameter require the
projection of the dependencies over the decomposed schemas"
  (declare (type schema S) (type mystream st1) (type t wpd))
  (let* ((f1 (sch-f-dep-s s))       ; get all the fds of the input schema
	 (ns (sch-name s))          ; get the schema name
	 (deps (canonical-set f1))  ; compute the canonical cover of the dependencies
	 (g deps)
	 (nd (list (list (first g)))) ; prepare for building groups, each group is a set of fd
	 (others (rest g)))           ; into a unique dependency, so nd contains the first “group”
    (unless deps              ; if the schema is without fds, then the relations is already in 3nf
      (return-from 3nf (mk-sch-s (list s))))
    (resetCountName)          ; prepare for naming the relations generated
    (expl st1 "A canonical cover of the set dependencies is:~%~%~/fundep:f-dep-s/~%~%" deps)
    ;; now generate the groups - for now each group is made by a single fd
    (dolist (d1 others)       ; for each dependency not considered
      (block outer            ; see if its determinant is aleady present in a “group”
	(loop for (d2) in nd    ; for each determinant of a group
	   when (my-set-equal (determinant d1) (determinant d2)) ; see if it is equal to lhs(d1)
	   do (setf (determinate d2) (att-s-union (determinate d2) (determinate d1)))
	   and do (return-from outer))   ; repeat the external loop for a new dependency d1
	(setq nd (cons (list d1) nd))))  ; if it is not present, add d1 as a new “group”
    (setq nd (reverse nd))               ; just for aestethic reasons in the result
    (expl st1 "The groups of dependencies are:~%~%~/fundep:f-dep-s/~%~%" (mapcar #'car  nd))
    ;; now fuse the groups with equivalent determinants
    (setf g nil)
    
    ;; now generate the schemas from the groups
    (let* ((rho (mapcar (lambda(d)
			  (let ((newatts (f-dep-s-att-s d)));; (mk-att-s  (att-s-union (determinant d) (determinate d)))))
			    (mk-sch (genCountName ns)
				    newatts
					  (if wpd
					      (project-set-fun-dep newatts f1)
					      (mk-f-dep-s d)))))
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

(defun 3nfpd (S &optional st1)  ;  -> set-set-attribute
  "Decompose the rel-schema s in Third Normal Form return the projected dependencies"
  (declare (type schema S) (type mystream st1))
  (3nf s st1 t))

(defun 3nftbc (s &optional st1)  ;  -> set-set-attribute
  "Decompose the rel-schema s in Third Normal Form checking if the final schemas are in BCNF"
  (declare (type schema s) (type mystream st1))
  (let ((rho (3nf s st1 t))
	(sat t)
	(st2 (when st1 (make-string-output-stream))))
    (list rho (dolist (s1 rho
		       (progn (when (null sat)
				(expl st1 "The dependencies that violates the BCNF are: ~%~%~A.~%~%" (get-output-stream-string st2)))
			      sat))
		(when (violates-boyce-codd s1 st2)
		  (setq sat nil))))))

