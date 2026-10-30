;;;; abcl-specific.lisp

(in-package #:zacl)

;;; ABCL runs on the JVM, which gives Lisp neither file descriptors nor
;;; stat(2), so the eight functions the Clozure CL and SBCL files take from
;;; their implementation internals are answered here with ANSI stream and
;;; pathname operations, as on ECL, EGCL and dotcl.  STREAM-UNIX-FD
;;; answers the stream itself: the "fd" that excl:stream-input-fn hands to
;;; excl::filesys-size and excl::filesys-write-date is then the open file
;;; stream, and the fstat- functions read it with FILE-LENGTH and
;;; FILE-WRITE-DATE.
;;;
;;; ABCL's SYSTEM package is also SYS, a name AllegroServe's sources use,
;;; so on ABCL zacl's SYS (and with it MP and SI) are ZACL.SYS, ZACL.MP
;;; and ZACL.SI under package-local nicknames, as on ECL (package.lisp,
;;; defpackage-hack.lisp).

(defun stream-unix-fd (stream)
  stream)

(defun fstat-size (fd)
  (if (streamp fd)
      (file-length fd)
      (stat-size fd)))

(defun fstat-mtime (fd)
  (stat-mtime (if (streamp fd) (pathname fd) fd)))

(defun stat-mtime (file)
  "Seconds since the Unix epoch, as sb-posix:stat-mtime answers."
  (- (file-write-date file)
     (encode-universal-time 0 0 0 1 1 1970 0)))

(defun stat-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun file-kind (pathname)
  (cond ((uiop:directory-exists-p pathname) :directory)
        ((uiop:file-exists-p pathname) :file)
        (t nil)))

(defun socket-error-identifier (condition)
  (declare (ignore condition))
  :unknown)

(defun socket-error-code (condition)
  (if (search "Broken pipe" (princ-to-string condition))
      32
      99))
