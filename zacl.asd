;;;; zacl.asd

(asdf:defsystem #:zacl
  :description "A layer for loading and running some Allegro CL
  projects."
  :author "Zach Beane"
  :license "BSD"
  :defsystem-depends-on (#:setup-cl+ssl)
  :depends-on (#:uiop
               #:asdf ;; FLAG -- why do we depend on asdf??
               #:usocket
               #:bordeaux-threads
               #:cl-ppcre
               #:babel-streams
               #:quri
               #:trivial-garbage
               #:trivial-backtrace
               #:split-sequence
               #:queues.simple-queue
               #:cl-store
               #:alexandria
               #:md5
               #:cl+ssl
	       #:local-time
	       #:cl-base64)
  :serial t
  :components ((:file "package")
               #+clozure
               (:file "clozure-specific")
               #+sbcl
               (:file "sbcl-specific")
               #+egcl
               (:file "egcl-specific")
               #+dotcl
               (:file "dotcl-specific")
               #+ecl
               (:file "ecl-specific")
               #+abcl
               (:file "abcl-specific")
               #+clasp
               (:file "clasp-specific")
               #-clozure
               (:file "portable-process")
               #+clozure
               (:file "ccl-timers")
               #+clozure
               (:file "clozure-process")
               (:file "defpackage-hack")
               (:file "reader")
               (:file "fake-slots")
               (:file "if-star")
               (:file "package-excl")
               (:file "package-sys")
               (:file "package-mp")
               (:file "package-socket")
               (:file "package-si")
               (:file "package-ff")
               (:file "package-net.uri")
               (:file "package-toplevel.debug")
               (:file "package-util.zip")
               (:file "package-util.string")
               (:file "utils")
               (:file "asdf-tricks")
               (:file "provides")))

