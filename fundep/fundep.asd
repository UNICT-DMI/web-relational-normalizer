;;;; fundep.asd

(asdf:defsystem #:fundep
  :serial t
  :description "Package for relational theory algorithms"
  :author "Renzo Orsini <orsini@unive.it>"
  :license "MIT Licence"
  :depends-on (#:alexandria #:dlist)
  :components ((:file "package")
               (:file "utilities")
               (:file "fundep-basic-types")
	       (:file "attributes-algorithms")
	       (:file "f-dep-algorithms")
	       (:file "f-dep-inferences")
	       (:file "cover-algorithms")
	       (:file "keys-algorithms")
	       (:file "decompositions")
	       (:file "bcnf-algorithms")
	       (:file "tnf-algorithms")
	       (:file "full-norm-algorithms")
	       (:file "fundep-basic-algorithms")
	       (:file "fundep-multi-valued")))

