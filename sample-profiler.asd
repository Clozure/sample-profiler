;; Copyright (c) 2016 Clozure Associates LLC

#-CCL (error "Only works in CCL")

(asdf:defsystem :sample-profiler
  :author "Gail Zacharias <gz@clozure.com>"
  :license "Apache2"
  :description "Collect and display sample profilling data"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "profile")))
