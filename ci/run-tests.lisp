;;;; ci/run-tests.lisp -- one test job of the pipeline (.gitlab-ci.yml), on
;;;; whichever Lisp the job's image carries.
;;;;
;;;; Loads zacl and zaserve from this checkout and the deps/ artifact the
;;;; deps job left (ci/fetch-deps.lisp), publishes a page, fetches it with
;;;; AllegroServe's own client, then runs the zacl-tests suite.  Exits 0
;;;; only when every step passed; an unhandled error anywhere is a
;;;; failure, never a debugger waiting on a job's stdin.
;;;;
;;;; Needs only ASDF: the dist sources are loaded by source registry, so
;;;; the same script serves an implementation Quicklisp's client has
;;;; never heard of.

(require :asdf)

(defvar *root* (uiop:ensure-directory-pathname
                (or (uiop:getenv "CI_PROJECT_DIR") (uiop:getcwd))))
(defvar *deps* (merge-pathnames "deps/" *root*))
(defvar *failures* '())

(defun note (fmt &rest args)
  (apply #'format t (concatenate 'string "~&TEST ~a: " fmt "~%")
         (lisp-implementation-type) args)
  (finish-output))

(defmacro try (name &body body)
  "Run BODY; an error records NAME as a failure and returns NIL."
  `(handler-case (progn ,@body)
     (error (e)
       (note "~a FAILED: ~a: ~a" ,name (type-of e) (princ-to-string e))
       (push ,name *failures*)
       nil)))

(defun finish ()
  (if *failures*
      (note "FAILED: ~{~a~^, ~}" (reverse *failures*))
      (note "all passed"))
  (finish-output)
  (uiop:quit (if *failures* 1 0)))

(setf *debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (note "unhandled: ~a" condition)
        (push "unhandled error" *failures*)
        (finish)))

(note "~a, ASDF ~a" (lisp-implementation-version) (asdf:asdf-version))

;; EGCL 0.0.10 loads asdf-flv (which fiveam depends on) with its
;; *FILE-LOCAL-VARIABLES* unbound, although the same DEFVAR with a NIL
;; initial value is bound when compiled and loaded on its own; a value
;; given before the system loads is kept.  Reported upstream; until it is
;; fixed, bind it here.  Two top-level forms: the second is read only
;; once the first has made the package.
#+egcl
(defpackage #:net.didierverna.asdf-flv (:use #:cl))
#+egcl
(defvar net.didierverna.asdf-flv::*file-local-variables* nil)

(asdf:initialize-source-registry
 `(:source-registry
   ;; Evergreen's forks of the portability libraries, ahead of the dist
   #+egcl (:tree ,(merge-pathnames "forks/" *deps*))
   #+egcl (:tree ,(merge-pathnames "extra/" *deps*))
   (:directory ,*root*)
   (:directory ,(merge-pathnames "setup-cl+ssl/" *root*))
   (:tree ,(merge-pathnames "zaserve/" *deps*))
   (:tree ,(merge-pathnames "quicklisp/dists/quicklisp/software/" *deps*))
   :ignore-inherited-configuration))

;; A string, not a pathname: ASDF adds its wildcards to a string and takes
;; a pathname as it is, which matches nothing.
(asdf:initialize-output-translations
 `(:output-translations
   (t ,(namestring (merge-pathnames "zacl-ci-fasl/" (uiop:temporary-directory))))
   :ignore-inherited-configuration))

(defvar *t0* (get-internal-real-time))
(defun secs ()
  (/ (- (get-internal-real-time) *t0*) (float internal-time-units-per-second)))

(unless (try "load zacl" (asdf:load-system :zacl) t)
  (finish))
(note "zacl loaded in ~,1f s" (secs))

(unless (try "load zaserve" (asdf:load-system :zaserve) t)
  (finish))
(note "zaserve loaded in ~,1f s" (secs))

(defun sym (name) (read-from-string name))

(try "serve and fetch"
  (funcall (sym "net.aserve:publish")
           :path "/hello"
           :function (lambda (req ent)
                       (eval `(,(sym "net.aserve:with-http-response") (,req ,ent)
                               (,(sym "net.aserve:with-http-body") (,req ,ent)
                                ;; a list on purpose: the pretty printer asks
                                ;; the stream its column
                                (format ,(sym "net.html.generator:*html-stream*")
                                        "hello from ~a; a list: ~a"
                                        (lisp-implementation-type) '(1 2 3)))))))
  (let ((port (let ((s (usocket:socket-listen "127.0.0.1" 0)))
                (prog1 (usocket:get-local-port s) (usocket:socket-close s)))))
    (funcall (sym "net.aserve:start") :port port :host "127.0.0.1" :listeners 2)
    (flet ((url (path) (format nil "http://127.0.0.1:~a~a" port path))
           (fetch (&rest args) (apply (sym "net.aserve.client:do-http-request") args)))
      (loop repeat 20 until (ignore-errors (fetch (url "/hello"))) do (sleep 1))
      (multiple-value-bind (body code) (fetch (url "/hello"))
        (note "GET /hello => ~a ~s" code body)
        (unless (and (eql code 200) (search "a list: (1 2 3)" body))
          (error "GET /hello: expected 200 and the list, got ~a ~s" code body)))
      (multiple-value-bind (body code)
          (fetch (url "/hello") :method :post :content "a=1&b=2"
                 :content-type "application/x-www-form-urlencoded")
        (note "POST /hello => ~a" code)
        (unless (and (eql code 200) (search "a list: (1 2 3)" body))
          (error "POST /hello: expected 200 and the list, got ~a ~s" code body)))
      (multiple-value-bind (body code) (fetch (url "/nothing"))
        (declare (ignore body))
        (note "GET /nothing => ~a" code)
        (unless (eql code 404)
          (error "GET /nothing: expected 404, got ~a" code))))
    (funcall (sym "net.aserve:shutdown"))
    (note "server shut down")))

(when (try "load zacl-tests" (asdf:load-system :zacl-tests) t)
  (note "zacl-tests loaded in ~,1f s" (secs))
  (try "run zacl-tests"
    (let* ((results (funcall (sym "fiveam:run") (sym "zacl-tests::zacl-tests")))
           (passed (count-if (lambda (r) (typep r (sym "fiveam::test-passed"))) results)))
      (funcall (sym "fiveam:explain!") results)
      (note "zacl-tests: ~a of ~a passed" passed (length results))
      (unless (and (plusp (length results)) (= passed (length results)))
        (error "~a of ~a zacl-tests failed" (- (length results) passed) (length results))))))

(note "done in ~,1f s" (secs))
(finish)
