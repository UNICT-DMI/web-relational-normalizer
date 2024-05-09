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


(defstruct (element (:print-function print-element))
  fun
  att-s
  is-t
  is-active
  is-alive
  is-fd
  size
  other)
(defun print-element (el str lvl)
  (declare (ignore lvl))
  (format str "<~a ~a ~:[S~;T~] ~:[nonactive~;active~], ~:[dead~;alive~]~:[~;~%   other= ~a~]>~%"
	  (element-fun el) (element-att-s el) (element-is-t el) (element-is-active el)
	  (element-is-alive el) (element-other el) (element-other el)))

(defstruct (attelem (:print-function print-attelem))
  att
  size
  dlist)
(defun print-attelem (ae str lvl)
     (declare (ignore lvl))
     (format str "    {~a ~{~a ~}}~%" (attelem-att ae)
	     (dlist->list (mapdlist (lambda(d) (dep-index (element-fun d))) (attelem-dlist ae)))))

(defstruct (construct (:print-function print-construct))
  atts
  dlist
  attlist
  size
  totsize)
(defun print-construct (cs str lvl)
  (declare (ignore lvl))
  (format str "===~%~%P = ~a~%DLIST = ~%~a~%ATTLIST = ~%~a~% TOTSIZE = ~a~%~%===~%~%"
	  (construct-atts cs) (construct-dlist cs) (construct-attlist cs) (construct-totsize cs)))

(defstruct (dep (:print-function print-dep))
  index
  mvd
  s
  ta
  tc)
(defun print-dep (dep str lvl)
     (declare (ignore lvl))
     (format str "~a:[~a ~:[->~;->>~] ~a] ~%"
	     (dep-index dep) (dep-s dep) (dep-mvd dep) (dep-ta dep)))

(defun process-dep-s(u ds)
  "preprocess the dependencies of s"
  (declare (type attribute-set u) (type dependency-set ds))
  (let* ((len (length ds))
	 (darr (make-array len)))
    (loop for d in ds
       for i from 0
       do (setf (aref darr i)
		(make-dep :index i
			  :mvd (mv-depp d)
			  :s (determinant d)
			  :ta (determinate d)
			  :tc (att-s-set-difference u (determinate d)))))
    darr))					

(defun build-construct(arr p)
  "Build a construct for the Galil 82 algorithm"
  (declare (type (array dep *) arr) (type attribute-set p))
  (let* ((actives (dlist))
	 (nonactives (dlist))
	 (attlist (dlist))
	 (dlist nil)
	 (mp 0))
    (loop for d across arr for i from 0
       for pis = (att-s-intersection (dep-s d) p)
       for pit = (att-s-intersection (dep-ta d) p)
       for pitc = (att-s-intersection (dep-tc d) p)
       for alive = (and pit pitc)
       for active = (and alive (null pis))
       for elt = (make-element :fun d :att-s pit :is-t t :is-active active
			       :is-fd (f-depp d) :is-alive alive :size (length pit))
       if pis
       do (setf (element-other elt)
		(make-element :fun d :att-s pis :is-t nil :is-active active
			      :is-fd (f-depp d) :is-alive alive :size (length pis)))
       and do (dlist-push elt nonactives :at-end t)
       else do (dlist-push elt actives :at-end t))
    (setf dlist (if (and actives nonactives)
		    (dlist-nconc actives nonactives)
		    (or actives nonactives)))
    (loop for a in p
       for adlist = (dlist)
       do (dodlist (el dlist)
	    (let ((elo (element-other el)))
	      (when (att-s-member a (element-att-s el))
		(dlist-push el adlist :at-end t))
	      (when (and elo (att-s-member a (element-att-s elo)))
		(dlist-push el adlist :at-end t))))
       do (let ((ma (dlist-length adlist)))
	    (dlist-push (make-attelem :att a :size ma :dlist adlist)
			attlist)
	    (incf mp ma)))
    (make-construct :atts p :dlist dlist :attlist attlist :size (length p) :totsize mp)))

(defun compute-dep(s x &optional st1)
  "Calculus of Dependency Basis with the algorithm of Galil 82"
  (declare (type schema s) (type attribute-set x) (type mystream st1))
  (let* ((u (sch-att-s s))
	 (ds (sch-dep-s s))
	 (arr (process-dep-s u ds))
	 (queue (list (build-construct arr (att-s-set-difference u x))))
	 p p1 p2 result)
    (format t "Dependencies: ~%~a~%" arr)
    (loop (unless queue (return))
       (setq p (pop queue))
       (push p result)
       (format t "~%P = ~a ~a " (construct-atts p) (mapdlist (lambda(x) (element-is-active x)) (construct-dlist p)))
       (when (element-is-active (data (dlist-first (construct-dlist p))))
	 (dodlist (el (construct-dlist p))
	  ; (cond ((element-is-active el)
	;	  (cond ((< (element-size el)) (print "BOTH"))
	;		(t (print "ACTIVE"))))
	;	 (t (cond ((< (element-size el)) (print "SIZE"))
	;		  (t (print "NO")))))
	   (when (element-is-active el)
	     (pop result)
	     (when (< (element-size el) (construct-size p))
	       (setf p1 (att-s-intersection (construct-atts p) (dep-ta (element-fun el)))
		     p2 (att-s-intersection (construct-atts p) (dep-tc (element-fun el))))
	       (let ((c1 (build-construct arr p1))
		     (c2 (build-construct arr p2)))
		 (format t "P1 = ~a  " (construct-atts c1))
		 (format t "P2 = ~a  " (construct-atts c2))
		 (setf queue (cons c1 (cons c2 queue)))))))))
    (mk-att-s-s (append (loop for a in x collect (list a))
			(loop for c in result collect (construct-atts c))))))


(defun 4nfnew (s &optional st1)
  "Decompose the rel-schema s in Fourth Normal Form"
  (declare (type schema s) (type mystream st1))
  (setf s (mk-sch (sch-name s) (sch-att-s s) (canonical-set (sch-f-dep-s s)) (sch-mv-dep-s s)))
  (let* ((rho (list s))
	 (ns (sch-name s))
	 (bads rho)
	 (goods nil))
    (resetCountName)
    (loop
       while bads
       for sch = (pop bads)
       for atts = (sch-att-s sch)
       for deps = (sch-f-dep-s sch)
       for dep = (find-if (lambda(dep) (not (is-superkey (determinant dep) sch))) deps)
       if dep
       do (progn (expl st1 "In: ~%~%~/fundep:sch/~% the determinant of ~/fundep:f-dep/ is not a superkey " sch dep)
		 (let* ((x (determinant dep))
			(xP (attr-set-closure x deps))
			(newschema1 (mk-sch (genCountName ns) 
					    xP 
					    (project-set-fun-dep xP deps)))
			(atts2 (mk-att-s (set-difference atts
							 (set-difference xP x))))
			(newschema2 (mk-sch  (genCountName ns) 
					     atts2
					     (project-set-fun-dep atts2 deps))))
		   (push newschema1 bads)
		   (push newschema2 bads)
		   (expl st1 "and so ~A is replaced by: ~%~%~/fundep:sch/~% and:~%~%~/fundep:sch/~%~%"
			 (sch-name sch) newschema1 newschema2)))
       else do (push sch goods))
    (setq goods (mk-sch-s goods))
    (expl st1 "The final result is:~%~%~/fundep:sch-s/~%~%" goods)
    goods))
  

(defun is-4nf (s &optional st1)
  "Check if schema s is in 4NF"
  (declare (type schema s) (type mystream st1))
  (and (not (violates-boyce-codd s st1))
       (loop for x in (sch-mv-dep-s s)
	    always (is-superkey (determinant x) s))))

(defun proj-mv-on (mvs sch)
  (loop for mv in mvs
     when (subsetp (mv-dep-att-s mv) (sch-att-s sch))
     collect mv))

(defun 4nf (s &optional st1)
  "Decompose the rel-schema s in Fourth Normal Form"
  (declare (type schema s) (type mystream st1))
  (let ((ns (sch-name s))
	(bc (bcnf s st1))
	(mvds (sch-mv-dep-s s))
	(goods nil))
    (loop
       while bc
       for sch = (pop bc)
       for mvs = (proj-mv-on mvds sch)
       if (cdr mvs)
       do (loop for (mv . rest) on mvs
	     do (let ((sch1 (mk-sch (genCountName ns)
				    (mk-att-s (mv-dep-att-s mv))
				    (sch-f-dep-s sch)
				    (mk-mv-dep-s (list mv)))))
		  (push sch1 goods)
		  (setf sch (mk-sch (genCountName ns)
				    (set-difference (sch-att-s sch) (determinate mv))
				    (sch-f-dep-s sch)
				    (mk-mv-dep-s rest)))))
	;     finally (when (and sch (sch-att-s sch)) (break "altro schema") (push sch goods)))
       else do (push sch goods))
    goods))

;; (defun 4nf (s &optional st1)
;;   "Decompose the rel-schema s in Fourth Normal Form"
;;   (declare (type schema s) (type mystream st1))
;;   (let ((ns (sch-name s))
;; 	(fds (sch-f-dep-s s))
;; 	(mvds (sch-mv-dep-s s))
;; 	(goods nil)
;; 	(bads (list s)))
;;     (
