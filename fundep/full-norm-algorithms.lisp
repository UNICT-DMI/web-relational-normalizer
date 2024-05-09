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

(defun full-normalization (s &optional st1)
  "Perform a set of automatic operations in order to normalize the schema s"
  (declare (type schema s) (type mystream st1) (ignore st1))
  (let* ((start-time (get-internal-run-time))
;	 (hh hunchentoot:*request*)
;	 (acc hunchentoot:*acceptor*)
;	 (ct (bordeaux-threads:current-thread))
;	 (nt (bordeaux-threads:make-thread (lambda() (kill-on-timeout start-time ct hh acc))))
	 (result (with-output-to-string (str)
		   (int-full-normalization s str)))
	 (end-time (get-internal-run-time))
	 (time-elapsed (float (* 1 (/ (- end-time start-time) internal-time-units-per-second))))
	 (units "seconds"))
    (when (< time-elapsed 0.1)
      (setf time-elapsed (* time-elapsed 1000))
      (setf units "milliseconds"))
    (format nil "~a~%~%Processing time: ~a ~a." result time-elapsed units)))

(defun int-full-normalization (s &optional st1)
  "Perform a set of automatic operations in order to normalize the schema s"
  (declare (type schema s) (type mystream st1))
  (unless st1 (setf st1 (make-string-output-stream)))
  (format st1 "Automatic normalization of the schema: ~%~%~/fundep:sch/~%" s)
  (let ((can (sch-canonical-set s)))
    (if (equal can (sch-f-dep-s s))
	(format st1 "1. The dependencies are already a canonical cover and do not require further reduction.~%~%")
	(format st1 "1. A canonical cover of the dependencies is: ~%~% ~/fundep:f-dep-s/.~%~%"
		(sch-canonical-set s))))
  (format st1 "2. The schema has the following keys: ~%~%~/fundep:att-s-s/.~%~%"
	  (find-all-keys s))
  (let ((no-boyce (violates-boyce-codd s)))
    (if (not no-boyce)
	(format st1 "3. The schema is already in Boyce-Codd Normal Form.~%~%No further normalization is needed.")
	(progn
	  (format st1 "3. The schema is not in Boyce-Codd Normal form.~%~%")
	  (let ((is-3 (is-3nf s)))
	    (if is-3
		(format st1 "4. The schema is already in Third Normal Form. ~%~%No further normalization is performed.")
		(let* ((bc-res (bcnftdp s))
		       (res (first bc-res))
		       (mdep (second bc-res)))
		  (if mdep
		      (format st1 "4. The schema is neither in 3NF.~%~%5. The schema has been decomposed in BCNF, and this decomposition preserves the dependencies. Here is the result:~%~%~/fundep:sch-s/" res)
		      (let* ((st2 (make-string-output-stream))
			     (d3nf (3nftbc s st2))
			     (dec (first d3nf))
			     (is-bc (second d3nf)))
			(format st1 "4. The schema is neither in 3NF.~%~%5. Since the attempt to decompose the schema in BCNF is producing a loss of dependencies, the schema has been decomposed in 3NF. Here is the result:~%~%~/fundep:sch-s/~a"
				dec
				(if is-bc
				    (format nil "~%~%All the decomposed schemas are in BCNF.")
				    (format nil "~%~%The following functional dependencies in the decomposed schemas violate the BCNF: ~%~%~/fundep:f-dep-s/."
					    (loop for sch of-type schema in dec append (violates-boyce-codd sch))))))))))))))


	      
