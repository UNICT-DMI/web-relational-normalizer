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

(defun make-chase (schema decomposition)
  "build the chase for the decomposition of a schema"
  (declare (type schema schema) (type schema-set decomposition))
  (let ((tableau-changed t))
    (labels ((poss (att1 atts)
	       "return a list with the positions of attributes att1 in atts"
	       (loop for a in att1 collect (position a atts)))
	     (proj (row ids)
	       "project a row over a list of indices"
	       (loop for i in ids collect (nth i row)))
	     (unify (tableau r1 r2 bid)
	       "unification step for the chase procedure"
	       (labels ((ch-all (bid old-val new-val)
			  (setf tableau-changed t)
			  (loop for row in tableau
			     when (eql (nth bid row) old-val)
			     do (setf (nth bid row) new-val))))
		 (let ((v1 (nth bid r1))
		       (v2 (nth bid r2)))
		   (cond ((eql v1 v2))
			 ((and (symbolp v1) (symbolp v2)) (return-from make-chase nil))
			 ((symbolp v2) (ch-all bid v1 v2))
			 (t (ch-all bid v2 v1))))))
	     (satisfy (tableau)
	       "see if there is a row with all symbols (lossless)"
	       (loop for row in tableau
		  thereis (every #'symbolp row))))
      (let* ((atts (sch-att-s schema)) ;; all the attributes of the schema
	     (n-atts (length atts))    ;; num. of attributes of the schema
	     (fds (canonical-set (sch-f-dep-s schema))) ;; can. cover of the schema
	     (d-size (length decomposition))  ;; num. of relations in decomposition
	     (all-atts (loop for i from 0 below n-atts collect i)) ;; (0 1 ... n-atts)
	     (d-atts (loop for d in decomposition  ;; list of indices of atts. in decomposition
			collect (poss (sch-att-s d) atts)))  ;; w.r.t. R
	     (tableau (loop for i from 1 to d-size   ;; initial tableau
			 collect (copy-list atts))))
	(loop for i from 0
	   for s1 = (set-difference all-atts (nth i d-atts))
	   for row in tableau
	   do (loop for s in s1 do (setf (nth s row) i)))
	(loop while tableau-changed
	   do (setf tableau-changed nil)
	   do (loop for (a (b)) in fds
		 for aidx = (poss a atts)
		 for bid = (position b atts)
		 do (loop for i from 0 to (- d-size 2)
		       for r1 = (nth i tableau)
		       for p1 = (proj r1 aidx)
		       do (loop for j from (1+ i) to (1- d-size)
			     for r2 = (nth j tableau)
			     for p2 = (proj r2 aidx)
			     when (equal p1 p2)
			     do (unify tableau r1 r2 bid)))))
	(satisfy tableau)))))

(defun full-decomposition (schema decomposition &optional st1)
  "test the kind of decomposition provided"
  (declare (type schema schema) (type schema-set decomposition) (type mystream st1))
  (declare (ignore st1))
  (with-output-to-string (str)
    (int-decomposition schema decomposition str)))

(defun attr-set-closure-for-decomposition (X F decomposition)
  "Compute XG+, the closure of X with respect to G, the union of the projection of the dependencies of F over decomposition"
  (declare (type attribute-set X) (type functional-dependency-set F) (type schema-set decomposition))
  (let* ((XG+ X)
	 (non-modified nil))
    (do () (non-modified (sort-symbols XG+))
      (setf non-modified t)
      (dolist (rel decomposition)
	(let* ((att (sch-att-s rel))
	       (XGT (intersection XG+ att))
	       (XGT+ (attr-set-closure XGT F))
	       (XGTT (intersection XGT+ att)))
	  (when (not (subsetp XGTT XG+))
	    (setf non-modified nil
		  XG+ (union XG+ XGTT))))))))

(defun sch-s-non-preserves-fd (R decomposition)
  "Returns the dependencies of R not preserved in the decomposition"
  (declare (type schema R) (type schema-set decomposition))
  (let ((fds (sch-f-dep-s R))
	(non-preserved nil))
    (loop for (lh rh) in fds
       when (not (subsetp rh (attr-set-closure-for-decomposition lh fds decomposition)))
       do (push (mk-f-dep lh rh) non-preserved))
    non-preserved))

(defun dep-s-violating-2nf (S &optional st1) ; -> bool
  "Check if rel-schema S is in second normal form"
  (declare (type schema S) (type mystream st1))
  (let* ((bad-deps nil)
	 (f1 (canonical-set (sch-f-dep-s s) st1))
	 (keys (find-all-keys s))
	 (primes (att-s-s-att-s keys)))
    (loop for dep in f1
       for (a (b)) = dep
       when (and (not (member b primes))
		 (some (lambda (k) (and (subsetp a k) (not (subsetp k a)))) keys))
       do (push dep bad-deps))
    (when bad-deps (expl st1 "~/fundep:f-dep-s/ in ~A" (mk-f-dep-s bad-deps) (sch-name s)))
    bad-deps))

(defun int-decomposition (schema decomposition &optional st1)
  "test the kind of decomposition provided"
  (declare (type schema schema) (type schema-set decomposition) (type mystream st1))
  (flet ((max-normal-form (s &optional st1)
	   (declare (type schema s))
	   (cond ((not (violates-boyce-codd s st1)) 3.5)
		 ((is-3nf s st1) 3)
		 ((not (dep-s-violating-2nf s st1)) 2)
		 (t 1))))
    (unless st1 (setf st1 (make-string-output-stream)))
    (format st1 "Given the schema: ~%~%~/fundep:sch/~% the following relations: ~%~%~/fundep:sch-s/~%" schema decomposition)
    (let ((attrschema (sch-att-s schema))
	  (other-attrs (sch-s-att-s decomposition))
	  (contained-in-others nil)
	  (continue nil))
      (cond ((not (subsetp attrschema other-attrs))
	     (format st1 "are an invalid decomposition since the attributes ~/fundep:att-s/ of the schema are not present in the decomposition.~%~%"
		     (set-difference attrschema other-attrs)))
	    ((not (subsetp other-attrs attrschema))
	     (format st1 "are an invalid decomposition since the attributes ~/fundep:att-s/ are not present in the original schema.~%~%"
		     (set-difference other-attrs attrschema)))
	    ((not (loop for (sc . res) on  (sort (copy-list decomposition) #'< :key #'(lambda(s) (length (sch-att-s s))))
		     do (some (lambda(x) (when (subsetp (sch-att-s sc) (sch-att-s x)) (pushnew sc contained-in-others))) res)
		     finally (return (not contained-in-others))))
	     (format st1 "are a redundant decomposition since the relations ~/fundep:sch-s/~%are contained in others.~%~%" contained-in-others)
	     (format st1 "Reducing the decomposition to ~/fundep:sch-s/~%~%"
		     (setf continue t
			   decomposition (mk-sch-s (set-difference decomposition contained-in-others)))))
	    (t (format st1 "form a correct decomposition.~%~%") (setf continue t)))
      (when continue
	(if (make-chase schema decomposition)
	    (format st1 "This is a lossless (nonadditive) decomposition.~%~%")
	    (format st1 "This decomposition is not lossless.~%~%"))
	(let ((bad (sch-s-non-preserves-fd schema decomposition)))
	  (if bad
	      (format st1 "This decomposition does not preserve the following functional dependencies:~%~%~/fundep:f-dep-s/~%~%" bad)
	      (format st1 "This decomposition preserves the functional dependencies.~%~%")))
	(loop for s in decomposition
	   for s1 = (mk-sch (sch-name s) (sch-att-s s) (sch-project-set-fun-dep schema (sch-att-s s)))
	   for maxn = (max-normal-form s1)
	   do (format st1 "The schema (with projected dependencies):~%~% ~/fundep:sch/~%is in ~ANF.~%~%"
		      s1 (case maxn ((3.5) "BC") (3 "3") (2 "2") (1 "1"))))))))
	  
	
	  
 

