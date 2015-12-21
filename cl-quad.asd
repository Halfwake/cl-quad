;;;; cl-quad.asd

(asdf:defsystem #:cl-quad
  :description "An implementation of a point region quad tree."
  :author "Andrew \"Drew\" Dudash <drewd8@vt.edu>"
  :license "GPL3"
  :serial t
  :components ((:file "package")
               (:file "cl-quad")
	       (:file "bound")
	       (:file "other"))
  :depends-on ("fiveam"))

