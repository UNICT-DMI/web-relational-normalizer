;;;; newnormalizer.asd

(asdf:defsystem #:newnormalizer
  :serial t
  :description "Web application for relational normalization"
  :author "Renzo Orsini <orsini@unive.it>"
  :license "MIT Licence"
  :depends-on (#:fundep #:hunchentoot #:ht-simple-ajax #:cl-who)
  :components (
    (:file "package")
    (:file "parser")
    (:file "newnormalizer")
  )
)

