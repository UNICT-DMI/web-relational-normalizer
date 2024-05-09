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


(in-package #:fundep)

;(declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun analyze (fname)
  "brute-force analysis of a table to find all the possible functional dependencies"
  (let* ((data (with-open-file (str fname :direction :input)
		 (read str)))
	 (nrows (length data))
	 (ncols (length (first data)))
	 (table (make-array (list nrows ncols) :initial-contents data))
	 (attrs (loop for i from (char-code #\A)
		   for j from 1 to ncols
		   collect (code-char i)))
	 (nattrs (loop for i from 0 to (1- ncols) collect i))
	 (tests (let (tests)
		  (loop for i from 1 to (1- ncols)
		     do (alexandria:map-combinations
			 (lambda (comb) (push comb tests))
			 nattrs :length i))
		  (sort tests #'<= :key #'length)))
	 (hash (make-hash-table :size 50 :test #'equal)))
    (labels ((possible-dependencies (det others)
	       (clrhash hash)
	       (loop for row from 0 to (1- nrows)
		  for key = (loop for x in det collect (aref table row x))
		  for val = (gethash key hash)
		  if (null val)
		  do (setf (gethash key hash) row)
		  else do (let* ((excluded
				  (loop for o in others
				     unless (equal (aref table row o) (aref table val o))
				     collect o)))
			    (setf others (set-difference others excluded)))
		  end
		  unless others do (return-from possible-dependencies (cons nil nil)))
	       (cons
		(loop for x in others collect (list det x))
		others))
	     (num-to-char (n)
	       (nth n attrs))
	     (nums-to-char (l)
	       (loop for n in l collect (nth n attrs))))
      (loop with all-deps = ()
	 while tests
	 for comb = (pop tests)
	 for others = (set-difference nattrs comb)
	 for (deps . excl) = (possible-dependencies comb others)
	 do (setf all-deps (union all-deps deps))
	 do (setf tests (loop for ts in tests
			   unless (and (subsetp comb ts) (intersection ts excl))
			   collect ts))
	 finally (loop for dep in all-deps
		    do (format t "~{~a~^ ~} → ~a~%"
			       (nums-to-char (car dep))
			       (num-to-char (cadr dep))))))))
	      
(defun analyze2 (fname)
  "brute-force analysis of a table to find all the possible functional dependencies"
  (let* ((data (with-open-file (str fname :direction :input)
		 (read str)))
	 (nrows (length data))
	 (ncols (length (first data)))
	 (table (make-array (list nrows ncols) :initial-contents data))
	 (attrs (loop for i from (char-code #\A)
		   for j from 1 to ncols
		   collect (code-char i)))
	 (nattrs (loop for i from 0 to (1- ncols) collect i))
	 (tests (let (tests)
		  (loop for i from 1 to (1- ncols)
		     do (alexandria:map-combinations
			 (lambda (comb) (push comb tests))
			 nattrs :length i))
		  tests))
	 (hash (make-hash-table :size 50 :test #'equal)))
    (labels ((possible-dependencies (det)
	       (let ((others (set-difference nattrs det)))
		 (loop for o in others
		    for res =
		      (block alpha
			(clrhash hash)
			(loop for row from 0 to (1- nrows)
			   for h = (aref table row o)
			   do (let* ((key (loop for col in det
					     collect (aref table row col)))
				     (val (gethash key hash)))
				(cond ((null val) (setf (gethash key hash) h))
				      ((eq h val))
				      (t (return-from alpha))))
			   finally (return t)))
		    when res collect (list det o))))
	     (num-to-char (n)
	       (nth n attrs))
	     (nums-to-char (l)
	       (loop for n in l collect (nth n attrs))))
      (loop for comb in tests
	 for pd = (possible-dependencies comb)
	 when pd
	 do (loop for dep in pd
	       do (format t "~{~a~^ ~} → ~a~%"
			  (nums-to-char (car dep))
			  (num-to-char (cadr dep))))))))
	      

(defun post-process (l &aux (ht (make-hash-table :test 'equal)))
  (setf l (stable-sort (copy-list l)  (lambda(x y) (<= (length (determinant x)) (length (determinant y))))))		 
  (setf l (let (res newl)
	    (do ((l1 l newl)) ((null l1))
	      (let* ((first-fd (car l1))
		     (a1 (determinant first-fd))
		     (b1 (first (determinate first-fd)))
		     (rest-fd (cdr l1)))
		(push first-fd res)
		(print first-fd)
		(setf newl
		      (loop for fd in rest-fd
			 unless (and (subsetp a1 (determinant fd)) (eq b1 (first (determinate fd))))
			 collect fd))))
	     res))
  (loop for (x y) in l do (setf (gethash x ht) (append y (gethash x ht))))
  (setf l (loop for x being the hash-key using (hash-value y) of ht collect (mk-f-dep x y)))
  (setf l (canonical-set l))
  (f-dep-s t l)
  l)
    
(defun inferfd (headers table &aux
				(h-dim (length headers))
				(fds nil)
				(ht (make-hash-table :test 'equal)))
  "Infer the functional dependencies not violated in a table"
  (labels ((comps (idx row) (loop for id in idx collect (nth id row)))
	   (uncomps (atts row) (loop for a in atts collect (position a row :test 'eql)))
	   (checkfds (lhs rhs)
	     (loop for det in rhs
		do (clrhash ht)
		when (loop for row in table
			for key = (comps lhs row)
			for new-val = (nth det row)
			for val = (gethash key ht)
			always (if val
				   (eql val new-val)
				   (setf (gethash key ht) new-val)))
		do (push (list lhs (list det)) fds)))
	   (checkall-fds (atts)
	     (checkfds (uncomps atts headers) (uncomps (set-difference headers atts :test 'eql) headers))))
    (loop for level from 0 to (1- h-dim)
       do (alexandria:map-combinations #'checkall-fds headers :length level :copy nil))
    (post-process (loop for (a b) in fds collect (list (comps a headers) (comps b headers))))))

