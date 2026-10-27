;;;; ci/fetch-deps.lisp -- the pipeline's deps job (.gitlab-ci.yml), on SBCL.
;;;;
;;;; Installs a Quicklisp client under deps/quicklisp/ (deps/quicklisp.lisp
;;;; was fetched by the job) and quickloads zacl, zaserve and zacl-tests
;;;; from this checkout and deps/zaserve/, so that every dist release they
;;;; need lands in deps/quicklisp/dists/quicklisp/software/.  That source
;;;; tree is the artifact the test jobs on every implementation load with
;;;; plain ASDF (ci/run-tests.lisp); no test job runs a Quicklisp client.

(require :asdf)

(defvar *root* (uiop:ensure-directory-pathname
                (or (uiop:getenv "CI_PROJECT_DIR") (uiop:getcwd))))
(defvar *deps* (merge-pathnames "deps/" *root*))

(load (merge-pathnames "quicklisp.lisp" *deps*))
(funcall (read-from-string "quicklisp-quickstart:install")
         :path (merge-pathnames "quicklisp/" *deps*))

;; This checkout and the zaserve clone are local projects: they win over
;; the dist's own zacl and zaserve releases.
(push *root* ql:*local-project-directories*)
(push (merge-pathnames "zaserve/" *deps*) ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload '(:zacl :zaserve :zacl-tests))

(format t "~&Releases installed: ~{~a~^ ~}~%"
        (sort (mapcar #'ql-dist:name
                      (ql-dist:installed-releases (ql-dist:dist "quicklisp")))
              #'string<))
(uiop:quit 0)
