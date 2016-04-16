;; Copyright (c) 2016 Clozure Associates LLC

(cl:defpackage :sample-profiler
  (:use :common-lisp)
  (:export   #:start-profiling
             #:end-profiling
             #:*show-foreign-addresses*
             ))
