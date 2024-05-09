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


;;; ----------------------------------------------------------------------

;;; This file contains the definition of the types and primitive operators
;;; used to represent the following data types:
;;; attributes, set of attributes, set of sets of attributes,
;;; functional dependencies, set of functional dependencies,
;;; schemas, set of schemas.

;;; The types are not opaque, in the sense that they are just symbols and lists,
;;; nor objects or structs, and they could be manipulated by the user.
;;; The only guarantee is that if you use the right constructors, all the
;;; operations defined in this package are correct.

;;; Main types:
;;;
;;; an attribute is a symbol
;;; a set of attributes is a list of symbol SORTED in lexicographic order
;;; a functional dependency abc→cd is a list with two elements,
;;;   determinant and deterimnate, both set of attributes, ((a b c) (c d))
;;; a set of function dependencies is a list of f.d.
;;; a schema is a three-element list: (name attributes f.d.set)

;;; Naming conventions
;;;
;;; Each type has a corresponding abbreviation used in the composite names:
;;; attribute => att
;;; functional-dependency => f-dep
;;; multivalued-dependency => mv-dep
;;; dependency => dep (union type of f-dep or mv-dep)
;;; schema => sch
;;; ------
;;; Each type has a correspondig set type, with name type-set
;;; and abbreviation: abbreviation-set  (e.g. attribute-set => att-s)
;;; ------
;;; Each type has the following functions: (abbr is the abbreviate name)
;;; abbrp: o -> bool -- predicate to check if object o is of type abbr
;;; mk-abbr: args    -- where args is a set of elements (for set types)
;;;                     or the components of the type
;;; abbr: stream arg1 args -- helper function to format values of type abbr
;;;                           through the format specification ~/helper-function/
;;; selectors of the components for composite type, with name of the form
;;; abbr1-abbr1 where abbr1 is the type of the object, and abbr2 is the type
;;;             of the component returned by the function when there is no ambiguity
;;;    for instance sch-s-f-dep-s take a set of schemas (sch-s) and returns the
;;;    set of all the functional dependences (f-dep-s) of all the schemas
;;;    so parsing the name should reveal the meaning of the operator
;;; for f-dep the selectors are determinant and determinate
;;; Finally, there is a set of generic operators on sets of objects with a name
;;; usually composed as: abbr-operator, to apply the operator specified to the
;;; set type specified by abbr. Examples: att-s-intersection (intersect two set
;;; of attributes), etc.

(in-package #:fundep)

;(declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; attributes
;; they are symbols
;; standard abbreviation: att

(defun attp(s)
  "true if the argument is an attribute"
  (declare (inline attp))
  (symbolp s))

(deftype attribute()
  "a non-opaque type represented with a symbol"
  `(satisfies attp))
    
(defun mk-att(a)
  "returns an attribute with the argument as name; idempotent"
  (declare (type attribute a) (inline mk-att))
  a)

;; set of attributes
;; they are lists of attributes, and should be unique and sorted
;; standard abbreviation: att-s

(defun att-sp(l)
  "true if the argument is a list of attributes"
  (declare (inline att-sp))
  (every #'attp l))

(deftype attribute-set()
  "a non-opaque type represented with an ordered list of non-duplicated symbols"
  `(satisfies att-sp))

(defun mk-att-s(l)
  "return a set of attributes given the list of attributes (without duplicates); idempotent"
  (declare (type attribute-set l) (inline mk-att-s))
  (remove-duplicates (sort (copy-list l) #'string<=)))

(defun att-s(str arg &rest args)
  "print a set of attributes"
  (declare (ignore args) (type attribute-set arg) (inline att-s))
  (format str "~{~A~^ ~}" arg))


;; set of set of attributes
;; they are lists of lists of attributes
;; standard abbreviation: att-s-s

(defun att-s-sp(l)
  "true if the argument is a list of set of attributes"
  (declare (inline att-s-sp))
  (every #'att-sp l))

(deftype attribute-set-set()
  "a non-opaque type represented with an ordered list of lists"
  `(satisfies att-s-sp))

(defun mk-att-s-s(l)
  "make a set of sets of attributes, starting from a list that
contains set of attributes; idempotent"
  (declare (type attribute-set-set l) (inline mk-att-s-s))
  (remove-duplicates (sort (copy-list l) #'string<= :key #'car) :test 'equal))

(defun att-s-s-att-s(l)
  "return all the attributes of a set of sets of attributes"
  (declare (type attribute-set-set l) (inline att-s-s-att-s))
  (att-s-s-union l))

(defun att-s-s(str arg &rest args)
  "print a set of setf of attributes"
  (declare (ignore args) (type attribute-set-set arg) (inline att-s-s))
  (format str "{ ~{(~/fundep:att-s/)~^ ~} }" arg))


;; functional dependencies
;; they are lists of two elements, determinant and determinate
;; standard abbreviation: f-dep

(defun f-depp(f)
  "true if the argument is a functional dependency"
  (declare (inline f-depp))
  (and (consp f)
       (= 2 (list-length f))
       (att-sp (first f))
       (att-sp (second f))))

(deftype functional-dependency ()
  "a non-opaque type represented with two sets of attributes"
  `(satisfies f-depp))

(defun mk-f-dep(d1 d2)
  "make a functional dependency from two sets of attributes"
  (declare (type attribute-set d1 d2) (inline mk-f-dep))
  (list (mk-att-s d1) (mk-att-s d2)))

(defun determinant(fd)
  "return the set of attributes that constitute the determinant of the functional dependency"
  (declare (type dependency fd) (inline determinant))
  (first fd))

(defun determinate(fd)
  "return the set of attributes that constitute the determinate of the functional dependency"
  (declare (type dependency fd) (inline determinate))
  (second fd))

(defun (setf determinate) (newdet fd)
  "modify the determinate of a functional dependency"
  (declare (type dependency fd) (type attribute-set newdet))
  (rplaca (cdr fd) newdet))

(defun f-dep-att-s(fd)
  "find all the attributes of a functional dependency"
  (declare (type dependency fd) (inline f-dep-att-s))
  (dep-att-s fd))

(defun f-dep(str arg &rest args)
  "print a functional dependency"
  (declare (ignore args) (type functional-dependency arg) (inline f-dep))
  (format str "~{~A~^ ~} → ~{~A~^ ~}" (first arg) (second arg)))

(defun equivdet (fd1 fd2)
  "See if the determinants of two fds are equivalent"
  (declare (type functional-dependency fd1 fd2))
  (and (subsetp (determinant fd1) (determinate fd2))
       (subsetp (determinant fd2) (determinate fd1))))

;; sets of functional dependencies
;; they are lists of functional dependencies
;; standard abbreviation: f-dep-s

(defun f-dep-sp(l)
  "true if the argument is a set of functional dependencies"
  (declare (inline f-dep-sp))
  (every #'f-depp l))

(deftype functional-dependency-set ()
  "a non-opaque type represented with a list of functional dependencies"
  `(satisfies f-dep-sp))

(defun mk-f-dep-s(l)
  "make a set of functional dependencies from a list of fd"
  (declare (type functional-dependency-set l) (inline mk-f-dep-s))
  (remove-duplicates (sort (copy-list l) #'string<= :key #'caar) :test 'equal))

(defun f-dep-s-att-s(fds)
  "find all the attributes of a set of functional dependencies"
  (declare (type functional-dependency-set fds) (inline f-dep-s-att-s))
  (att-s-s-union fds :key #'f-dep-att-s))

(defun f-dep-s(str arg &rest args)
  "print a set of functional dependencies"
  (declare (ignore args) (type functional-dependency-set arg) (inline f-dep-s))
  (format str "{ ~{~/fundep:f-dep/~^~% ~} }" arg))


;; multivalued dependencies
;; thery are lists of three elements: determinant, determinate, :-
;; standard abbreviation: mv-dep

(defun mv-depp(f)
  "true if the argument is a multivalued dependency"
  (and (consp f)
       (= 3 (list-length f))
       (eq :- (third f))
       (att-sp (first f))
       (att-sp (second f))))

(deftype multivalued-dependency ()
  "a non-opaque type represented with a :- and two sets of attributes"
  `(satisfies mv-depp))

(defun depp (d)
  "true if the argument is a dependency"
  (declare (inline depp))
  (or (f-depp d) (mv-depp d)))

(deftype dependency()
  "a non-opaque type resenting either a fd or a mvd"
  `(satisfies depp))

(defun mk-mv-dep(d1 d2)
  "make a multivalued dependency from two sets of attributes"
  (declare (type attribute-set d1 d2) (inline mk-mv-dep))
  (list (mk-att-s d1) (mk-att-s d2) :-))

(defun dep-att-s(d)
  "find all the attribute of a dependency"
  (declare (type dependency d) (inline dep-att-s))
  (att-s-union (determinant d) (determinate d)))

(defun mv-dep-att-s(md)
  "find all the attributes of a multivalued dependency"
  (declare (type multivalued-dependency md) (inline mv-dep-att-s))
  (dep-att-s md))

(defun mv-dep(str arg &rest args)
  "print a multivalued dependency"
  (declare (ignore args) (type multivalued-dependency arg) (inline mv-dep))
  (format str "~{~A~^ ~} ↠ ~{~A~^ ~}" (first arg) (second arg)))

(defun dep(str arg &rest args)
  "print a dependency"
  (declare (ignore args) (type dependency arg) (inline dep))
  (format str "~{~A~^ ~} ~:[→→~;→~] ~{~A~^ ~}"
	  (determinant arg) (f-depp arg) (determinate arg)))


;; set of multivalued dependencies
;; they are lists of multivalued dependencies
;; standard abbrevitaion mv-dep-s

(defun mv-dep-sp(l)
  "true if the argument is a set of multivalued dependencies"
  (declare (inline mv-dep-sp))
  (every #'mv-depp l))

(deftype multivalued-dependency-set ()
  "a non-opaque type represented with a list of multivalued dependencies"
  `(satisfies mv-dep-sp))

(defun dep-sp(l)
  "true if the argument is a set of dependencies"
  (declare (inline dep-sp))
  (every #'depp l))

(deftype dependency-set ()
  `(satisfies dep-sp))

(defun mk-mv-dep-s(l)
  "make a set of mvds from a list of mvd"
  (declare (type multivalued-dependency-set l) (inline mk-mv-dep-s))
  (remove-duplicates (sort (copy-list l) #'string<= :key #'caar) :test 'equal))

(defun mv-dep-s-att-s(mds)
  "find all the attributes of a set of multivalued dependencies"
  (declare (type multivalued-dependency-set mds) (inline mv-dep-s-att-s))
  (att-s-s-union mds :key #'mv-dep-att-s))

(defun mv-dep-s(str arg &rest args)
  "print a set of multivalued dependencies"
  (declare (ignore args) (type multivalued-dependency-set arg) (inline mv-dep-s))
  (format str "{ ~{~/fundep:mv-dep/~^~% ~} }" arg))

;; schemas
;; they are lists of three elements: name, attributes, functional dependencies
;; standard abbreviation: sch

(defun schp(s)
  "true if the argument is a schema"
  (and (consp s)
       (= 3 (length s))
       (symbolp (first s))
       (att-sp (second s))
       (dep-sp (third s))))

(deftype schema ()
  "a non-opaque type represented with a list with name, set of attributes, set of fd"
  `(satisfies schp))

(defun mk-sch(n a d &optional m)
  "make a schema given a name, a set of attributes, a set of fd
and maybe a set of multivalued dependency"
  (declare (type symbol n) (type attribute-set a) (type dependency-set d)
	   (type multivalued-dependency-set m) (inline mk-sch))
  (list n a (append d m)))

(defun sch-name(s)
  "return the name of the schema passed as argument"
  (declare (type schema s) (inline sch-name))
  (first s))

(defun sch-att-s(s)
  "return the attributes of a schema"
  (declare (type schema s) (inline sch-att-s))
  (second s))

(defun sch-f-dep-s(s)
  "return the functional dependencies of a schema"
  (declare (type schema s) (inline sch-f-dep-s))
  (remove-if #'mv-depp (third s)))

(defun sch-mv-dep-s(s)
  "return the multivalued dependencies of a schema"
  (declare (type schema s) (inline sch-mv-dep-s))
  (remove-if #'f-depp (third s)))

(defun sch-dep-s(s)
  (declare (type schema s) (inline sch-dep-s))
  (third s))

(defun sch(str arg &rest args)
  "print a schema"
  (declare (ignore args) (type schema arg) (inline sch))
  (format str "~a < (~/fundep:att-s/) ,~% ~/fundep:f-dep-s/ ~a>~%"
	  (sch-name arg) (sch-att-s arg) (sch-f-dep-s arg)
	  (if (sch-mv-dep-s arg)
	      (format nil "~/fundep:mv-dep-s/" (sch-mv-dep-s arg))
	      "")))

;; set of schemas
;; they are lists of schemas
;; standard abbreviation: sch-s

(defun sch-sp(s)
  "true if the argument is a set of schemas"
  (declare (inline sch-sp))
  (every #'schp s))
 
(deftype schema-set ()
  "a non-opaque type represent with a list of schemas"
  `(satisfies sch-sp))

(defun mk-sch-s(l)
  "given a list of schemas return a set of schemas with them"
  (declare (type schema-set l) (inline mk-sch-s))
  (remove-duplicates (sort (copy-list l) #'string<= :key #'car) :test 'equal))

(defun sch-s-att-s(s)
  "returns all the attributes of a set of schemas"  
  (declare (type schema-set s) (inline sch-s-att-s))
  (att-s-s-union s :key #'sch-att-s))

(defun sch-s-f-dep-s(s)
  "returns all the functional dependencies of a set of schemas"
  (declare (type schema-set s) (inline sch-s-f-dep-s))
  (reduce #'union s :key #'sch-f-dep-s))

(defun sch-s-mv-dep-s(s)
  "return all the multivalued dependencies of a set of schemas"
  (declare (type schema-set s) (inline sch-s-mv-dep-s))
  (reduce #'union s :key #'sch-mv-dep-s :initial-value nil))

(defun sch-s-dep-s(s)
  "return all the dependencies of a set of schemas"
  (declare (type schema-set s) (inline sch-s-dep-s))
  (reduce #'union s :key #'sch-dep-s :initial-value nil))

(defun sch-s(str arg &rest args)
  "print a set of schemas"
  (declare (ignore args) (type schema-set arg) (inline sch-s))
  (format str "~{~/fundep:sch/~^~%~}" arg))


;; generic functions and utilities

(defmacro cdr! (v)
  "utility to scan a list"
  `(setf ,v (cdr ,v)))

;;
;; Code by Rainer Joswig (see StackOverflow:
;; http://stackoverflow.com/a/34795092/2382734
;;
(defun att-s-union (x y &aux (first (list nil)) (last first) cx cy)
  "make the union of two set of attributes"
  (declare (type attribute-set x y))
  (loop (unless (and x y) (return))
     (setf cx (car x) cy (car y))
     (cond ((string= cx cy) (cdr! x))
	   ((string< cx cy) (rplacd last (list cx)) (cdr! last) (cdr! x))
	   (t (rplacd last (list cy)) (cdr! last) (cdr! y))))
  (rplacd last (or x y))
  (cdr first))

(defun att-s-subsetp (x y &aux cx cy)
  "check if x is a subset of y"
  (declare (type attribute-set x y))
  (loop (unless (and x y) (return))
     (setf cx (car x) cy (car y))
     (cond ((string= cx cy) (cdr! x) (cdr! y))
	   ((string> cx cy) (cdr! y))
	   (t (return))))
  (not x))

(defun att-s-set-difference (x y &aux (first (list nil)) (last first) cx cy)
  "remove the set of attributes y from x"
  (declare (type attribute-set x y))
  (loop (unless x (return))
     (setf cx (car x) cy (car y))
     (cond ((string= cx cy) (cdr! x) (cdr! y))
	   ((string< cx cy) (rplacd last (list cx)) (cdr! last) (cdr! x))
	   (t (rplacd last (list cx)) (cdr! last) (cdr! x))))
  (cdr first))

(defun att-s-intersection (x y &aux (first (list nil)) (last first) cx cy)
  "intersect two set of attributes"
  (declare (type attribute-set x y))
  (loop (unless (and x y) (return))
     (setf cx (car x) cy (car y))
     (cond ((string= cx cy) (rplacd last (list cx)) (cdr! last) (cdr! x) (cdr! y))
	   ((string< cx cy) (cdr! x))
	   (t (cdr! y))))
  (cdr first))

(defun att-s-s-union (l &key key)
  "make the union of a set of set of attributes"
  (declare (type list l) (inline att-s-s-union))
  (when key (setf l (loop for e in l collect (funcall key e))))
  (reduce #'att-s-union l :initial-value nil))

(defun att-s-cons (a l  &aux (first (list nil)) (last first) cl)
  "create a new att-s inserting a into l (a is not present)"
  (declare (type attribute a) (type attribute-set l))
  (loop (unless l (rplacd last (list a)) (return))
     (setf cl (car l))
     (if (string< a cl)
	 (progn (rplacd last (cons a l)) (return))
	 (progn (rplacd last (list cl)) (cdr! last) (cdr! l))))
  (cdr first))

(defun att-s-member (att l &aux cl)
  "see if a attribute is member of a set of attributes"
  (declare (type attribute att) (type attribute-set l))
  (loop (unless l (return))
     (setf cl (car l))
     (cond ((eq att cl) (return-from att-s-member att))
	   ((string< att cl) (return-from att-s-member))
	   (t (cdr! l)))))
	  
(defun my-sort(x)
  "sort a list of symbol x without duplicates"
  (remove-duplicates (sort (copy-list x) #'string<=)))

(defun sort-symbols (l)
"Sort a set of symbols non-destructively"
  (sort (copy-seq l) 'string<= :key 'symbol-name))

;; (defun all-attributes(s)
;;   "return all the attributes of the argument, that can be:
;; a) an attribute; b) a set of attributes; c) a functional dependency;
;; d) a set of functional dependencies; e) a schema; f) a set of schemas"
;;   (cond ((attp s) (list s))
;; 	((att-sp s) s)
;; 	((att-s-sp s) (reduce #'att-s-union s))
;; 	((f-depp s) (f-dep-att-s s))
;; 	((f-dep-sp s) (reduce #'att-s-union s :key #'f-dep-att-s :initial-value nil))
;; 	((mv-depp s) (mv-dep-att-s s))
;; 	((mv-dep-sp s) (mv-dep-s-att-s s))
;; 	((schp s) (sch-att-s s))
;; 	((sch-sp s) (sch-s-att-s s))
;; 	(t (error "unkown object in all-attributes"))))
