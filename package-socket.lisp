;;;; package-socket.lisp

(in-package #:zacl)

(defparameter *print-hostname-in-stream* nil)

;;; Addresses: an IPv4 address is a 32-bit integer, as Allegro has it,
;;; and an IPv6 address a 128-bit one (2026-10-08), so that what a
;;; dual-stack listening socket reports for a peer goes through the
;;; same functions; the text forms tell them apart.

(defvar *ipv6?* #-dotcl t #+dotcl nil
  "Whether a passive socket asked for on no particular host listens on
the IPv6 wildcard, which takes IPv4 connections too; a host without
IPv6 falls back to IPv4.  Off on dotcl, whose wildcard IPv6 listener
takes no IPv4 connections (2026-10-27: 127.0.0.1 refused, ::1 and
localhost served), so a server started with no :host would be
unreachable over IPv4 there.")

(defun ip-address-integer (ip-address)
  "An octet vector of 4 (IPv4) or 16 (IPv6) as one integer."
  ;; Any vector, not only a simple one: usocket's CCL backend reports the
  ;; IPv4 peer of a dual-stack listener as a vector displaced into the last
  ;; four octets of the IPv4-mapped IPv6 address, and a SIMPLE-ARRAY check
  ;; refused it, failing every accept from an IPv4 client.
  (check-type ip-address vector "octet vector")
  (unless (member (length ip-address) '(4 16))
    (error "An IP address of ~d octets." (length ip-address)))
  (let ((integer 0))
    (loop for byte across ip-address do (setq integer (logior (ash integer 8) byte)))
    integer))

(defun ipv6-integer-p (integer)
  (> integer #xFFFFFFFF))

(defun ipv4-mapped-p (integer)
  "An IPv6 address of the form ::ffff:a.b.c.d, as a dual-stack socket
reports an IPv4 peer."
  (and (ipv6-integer-p integer) (= (ash integer -32) #xFFFF)))

(defun integer-ip-address (integer)
  "INTEGER as an octet vector: 4 octets for an IPv4 address, 16 for IPv6."
  (check-type integer (unsigned-byte 128))
  (let* ((n (if (ipv6-integer-p integer) 16 4))
         (ip-address (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n ip-address)
      (setf (aref ip-address i) (ldb (byte 8 (* 8 (- n 1 i))) integer)))))

(defun ipv6-integer-text (integer)
  "INTEGER (128 bits) in the usual text form, the longest run of zero
groups elided; a mapped IPv4 address as ::ffff:a.b.c.d."
  (if (ipv4-mapped-p integer)
      (format nil "::ffff:~a" (socket:ipaddr-to-dotted (ldb (byte 32 0) integer)))
      (let* ((groups (loop for i from 7 downto 0 collect (ldb (byte 16 (* 16 i)) integer)))
             (best-start nil) (best-length 0))
        ;; the longest run of zeros, of at least two groups
        (loop with start = nil
              for group in (append groups (list -1)) for i from 0
              do (cond ((and (zerop group) (null start)) (setq start i))
                       ((and (not (zerop group)) start)
                        (when (> (- i start) (max best-length 1))
                          (setq best-start start best-length (- i start)))
                        (setq start nil))))
        (with-output-to-string (out)
          (loop for group in groups for i from 0
                do (cond ((and best-start (= i best-start)) (write-string "::" out))
                         ((and best-start (< best-start i (+ best-start best-length))))
                         (t (unless (or (zerop i) (and best-start (= i (+ best-start best-length))))
                              (write-char #\: out))
                            (format out "~(~x~)" group))))))))

(defun wildcard-host (host)
  "The host to listen on for HOST as make-socket was given it: with
*ipv6?*, no host or the IPv4 wildcard becomes the IPv6 wildcard, which
takes both families."
  (if (and *ipv6?* (or (null host) (equal host "0.0.0.0") (eql host 0)))
      "::"
      host))


(defclass zacl-socket (fundamental-binary-output-stream
                       fundamental-character-output-stream
                       fundamental-binary-input-stream
                       fundamental-character-input-stream)
  ((socket
    :initarg :socket
    :initform nil
    :reader socket)
   (real-stream
    :initarg :real-stream
    :initform nil
    :accessor real-stream
    :reader underlying-input-stream
    :reader underlying-output-stream)
   (bytes-written
    :initarg :bytes-written
    :initform 0
    :accessor bytes-written)
   (external-format
    :initarg :external-format
    :initform :latin-1
    :reader socket-ef
    :accessor zacl-cl:stream-external-format)
   #+sbcl
   (read-timeout
    :accessor read-timeout
    :initform nil)
   #+sbcl
   (write-timeout
    :accessor write-timeout
    :initform nil)))

#+sbcl
(defun call-with-deadline-maybe (identifier seconds body-fn)
  (handler-bind
      ((sb-sys:deadline-timeout
	 (lambda (e)
	   (declare (ignore e))
	   (error 'excl:socket-error :identifier identifier))))
    (sb-sys:with-deadline (:seconds seconds)
      (funcall body-fn))))

;; CCL and EGCL keep the timeout on the stream itself (see SOCKET-CONTROL),
;; so only their timeout condition needs turning into Allegro's.
#+(or ccl egcl)
(defun call-translating-timeout (identifier body-fn)
  (handler-bind
      ((#+ccl (or ccl:input-timeout ccl:output-timeout) #+egcl egcl-ext:io-timeout
        (lambda (e)
          (declare (ignore e))
          (error 'excl:socket-error :identifier identifier))))
    (funcall body-fn)))

(defmacro with-deadline-maybe (identifier seconds &body body)
  #-sbcl (declare (ignore seconds))
  #-(or sbcl ccl egcl) (declare (ignore identifier))
  #+sbcl `(call-with-deadline-maybe ,identifier ,seconds (lambda () ,@body))
  #+(or ccl egcl) `(call-translating-timeout ,identifier (lambda () ,@body))
  #-(or sbcl ccl egcl) `(progn ,@body))

(defmethod print-object ((object zacl-socket) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((socket (socket object)))
      (if (real-stream object)
	  (format stream
		  "connected from ~a:~a to ~a:~a"
		 (get-local-address socket)
		 (get-local-port socket)
		 (get-peer-address socket)
		 (get-peer-port socket))
	  (format stream
		  "waiting for connection at ~a:~a"
		 (get-local-address socket)
		 (get-local-port socket))))))

(defmethod stream-write-byte :after ((stream zacl-socket) byte)
  (declare (ignore byte))
  (incf (bytes-written stream)))

(defmethod stream-write-byte ((stream zacl-socket) byte)
  (write-byte byte (real-stream stream)))

(defmethod stream-write-char ((stream zacl-socket) char)
  (map nil (lambda (octet)
             (stream-write-byte stream octet))
       (string-to-octets (string char) :encoding (socket-ef stream))))

(defmethod stream-write-sequence :after ((stream zacl-socket) sequence
                                         start end &key &allow-other-keys)
  ;; FIXME: Could be affected by string encoding?
  (declare (ignore sequence))
  (incf (bytes-written stream) (- end start)))

(defmethod excl::socket-bytes-written ((socket zacl-socket) &optional set)
  (if set
      (setf (bytes-written socket) set)
      (bytes-written socket)))

(defmethod stream-write-sequence ((stream zacl-socket) sequence start end
                                  &key &allow-other-keys)
  (when (typep sequence 'string)
    (setf sequence (string-to-octets sequence :start start :end end
                                     :encoding (socket-ef stream)))
    (setf start 0)
    (setf end (length sequence)))
  (with-deadline-maybe :write-timeout (write-timeout stream)
    (write-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-read-byte ((stream zacl-socket))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (read-byte (real-stream stream) nil :eof)))

(defmethod stream-read-char ((stream zacl-socket))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (let ((byte (read-byte (real-stream stream) nil :eof)))
      (if (eql byte :eof)
	  :eof
	  (code-char byte)))))

(defmethod stream-read-char-no-hang ((stream zacl-socket))
  (let ((ready (wait-for-input (socket stream) :timeout 0 :ready-only t)))
    (when ready
      (stream-read-char stream))))

(defmethod stream-read-sequence ((stream zacl-socket) sequence start end
                                 &key &allow-other-keys)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (with-deadline-maybe :read-timeout (read-timeout stream)
    (if (stringp sequence)
	(let ((offset start)
	      (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
	  (let* ((after-index (read-sequence buffer (real-stream stream)))
		 (string (octets-to-string buffer :start 0 :end after-index
						  :encoding :latin-1)))
	    (replace sequence string :start1 start :end1 end
				     :start2 0 :end2 after-index)
	    (+ offset after-index)))
	(read-sequence sequence (real-stream stream) :start start :end end))))

#+ccl
(defmethod ccl:stream-read-vector ((stream zacl-socket) sequence start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (if (stringp sequence)
      (let ((offset start)
            (buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (let* ((after-index (read-sequence buffer (real-stream stream)))
               (string (octets-to-string buffer :start 0 :end after-index
                                         :encoding :latin-1)))
          (replace sequence string :start1 start :end1 end
                   :start2 0 :end2 after-index)
          (+ offset after-index)))
      (read-sequence sequence (real-stream stream) :start start :end end)))

(defmethod stream-force-output ((stream zacl-socket))
  (with-deadline-maybe :write-timeout (write-timeout stream)
    (force-output (real-stream stream))))

(defmethod close ((stream zacl-socket) &key abort)
  (declare (ignore abort))
  (socket-close (socket stream))
  ;; a passive (listening) socket has no stream
  (when (real-stream stream)
    (close (real-stream stream)))
  t)

(defun socket:make-socket (&key connect local-port local-host reuse-address
                             remote-port remote-host
                             format (backlog 5) type nodelay)
  (declare (ignore format type))
  (unless backlog
    (setf backlog 5))
  (ecase connect
    (:passive
     (let ((socket
             (flet ((listen-on (host)
                      (socket-listen host (or local-port 0)
                                     :reuseaddress reuse-address
                                     :element-type '(unsigned-byte 8)
                                     :backlog backlog)))
               (let ((host (wildcard-host local-host)))
                 (if (equal host local-host)
                     (listen-on host)
                     ;; the IPv6 wildcard first; a host without IPv6
                     ;; (a container, often) refuses it, and so does a
                     ;; usocket backend without INET6 (ECL's), whose
                     ;; fallback must then be the IPv4 wildcard itself,
                     ;; not the NIL it was asked for
                     (handler-case (listen-on host)
                       (error () (listen-on (or local-host usocket:*wildcard-host*)))))))))
       (make-instance 'zacl-socket
                      :socket socket)))

    ((nil :active)
     (let ((socket
            (socket-connect remote-host remote-port
                            :nodelay nodelay
                            :element-type '(unsigned-byte 8))))
       (make-instance 'zacl-socket
                      :socket socket
                      :real-stream (socket-stream socket))))))

;; A process that AllegroServe's SHUTDOWN has killed while it was blocked
;; here is woken by the listener being closed under it, not interrupted,
;; on a Lisp without thread interruption (EGCL; see KILL in
;; portable-process.lisp).  The error that wakes it must not reach
;; HTTP-ACCEPT-THREAD's handler, which logs every accept error and loops
;; for ever by design -- it spun on the closed listener, logging, until
;; the process exited.  Leave the way KILL does.
(defun socket:accept-connection (socket)
  (let ((incoming (handler-bind ((error (lambda (condition)
                                           (declare (ignore condition))
                                           (let ((process *current-process*))
                                             (when (and (typep process 'process)
                                                        (killedp process))
                                               (throw 'killed nil))))))
                    (socket-accept (socket socket)))))
    (make-instance 'zacl-socket
                   :socket incoming
                   :real-stream (socket-stream incoming))))

(defun socket:local-host (socket)
  (ip-address-integer (get-local-address (socket socket))))

(defun socket:local-port (socket)
  (get-local-port (socket socket)))

(defun socket:set-socket-options (socket &key nodelay)
  (setf (socket-option (socket socket) :tcp-no-delay) nodelay))

(defun socket:remote-host (socket)
  (ip-address-integer (get-peer-address (socket socket))))

(defun socket:ipaddr-to-dotted (ip-integer)
  "The text of an address: dotted for IPv4, colon groups for IPv6."
  (if (ipv6-integer-p ip-integer)
      (ipv6-integer-text ip-integer)
      (format nil "~A.~A.~A.~A"
              (ldb (byte 8 24) ip-integer)
              (ldb (byte 8 16) ip-integer)
              (ldb (byte 8  8) ip-integer)
              (ldb (byte 8  0) ip-integer))))

(defun socket:dotted-to-ipaddr (dotted &key errorp)
  (when (find #\: dotted)
    ;; an IPv6 address in text; ::ffff:a.b.c.d carries an IPv4 address
    ;; in its low 32 bits
    (let* ((colon (position #\: dotted :from-end t))
           (mapped? (find #\. dotted :start colon))
           (groups (if mapped? (concatenate 'string (subseq dotted 0 (1+ colon)) "0:0") dotted))
           (vector (ignore-errors (usocket:ipv6-host-to-vector groups)))
           (low (and mapped? (socket:dotted-to-ipaddr (subseq dotted (1+ colon)) :errorp errorp))))
      (cond ((and vector (or (not mapped?) low))
             (return-from socket:dotted-to-ipaddr
               (logior (ip-address-integer (coerce vector '(simple-array (unsigned-byte 8) (16))))
                       (or low 0))))
            (errorp (error "Malformed IPv6 address: ~A" dotted))
            (t (return-from socket:dotted-to-ipaddr nil)))))
  (let ((result 0)
        (length (length dotted))
        (i 0)
        (start 0))
    (loop
      (when (= i 4)
        (return result))
      (when (<= length start)
        (if errorp
            (error "Malformed IP string - overrun: ~A" dotted)
            (return nil)))
      (let* ((end (or (position #\. dotted :start start) length))
             (integer (ignore-errors
                        (parse-integer dotted :start start :end end))))
        (unless integer
          (if errorp
              (error "Cannot parse integer at ~A-~A of ~A"
                     start end dotted)
              (return nil)))
        (setf result (logior integer
                             (ash result 8)))
        (incf i)
        (setf start (1+ end))))))

(defmacro socket:with-pending-connect (&body body)
  `(progn ,@body))

(defun socket:ipaddr-to-hostname (ipaddr)
  (if (and (integerp ipaddr) (ipv6-integer-p ipaddr))
      ;; no reverse lookup for IPv6 here: the address itself
      (socket:ipaddr-to-dotted ipaddr)
      (progn
        ;; ECL's sockets contrib carries the SB-BSD-SOCKETS interface too.
        #+(or sbcl ecl) (sb-bsd-sockets:host-ent-name
                (sb-bsd-sockets:get-host-by-address
                 (if (typep ipaddr 'string)
                     (sb-bsd-sockets:make-inet-address ipaddr)
                     ipaddr)))
        #+dotcl (dotcl-bsd-sockets:host-ent-name
                 (dotcl-bsd-sockets:get-host-by-address ipaddr))
        #+ccl (ipaddr-to-hostname ipaddr)
        ;; no reverse lookup on EGCL, ABCL or Clasp either: the address itself
        #+(or egcl abcl clasp) (if (stringp ipaddr) ipaddr (socket:ipaddr-to-dotted ipaddr)))))

(defun socket:lookup-hostname (name)
  #+ccl (lookup-hostname name)
  #+(or sbcl egcl dotcl ecl abcl clasp) (let ((address (get-host-by-name name)))
           (if (= (length address) 16)
               (usocket:vector-to-ipv6-host address)
               (format nil "~{~a~^.~}" (coerce address 'list)))))

(defun make-ssl-stream (socket stream-type &key context
						method
						certificate key certificate-password
						max-depth
						ca-file ca-directory
						ciphers
						crl-check crl-file
						verify
						server-name)
  (declare (ignore crl-check crl-file))
  (ecase stream-type
    ((:client :server)))

  (when (and ca-file
	     (pathnamep ca-file))
    (setf ca-file (namestring ca-file)))
  (when (and ca-directory
	     (pathnamep ca-directory))
    (setf ca-directory (namestring ca-directory)))

  (let ((stream (if context
		    (with-global-context ((ssl-context-open-ssl-context context))
		      (let ((arguments nil))
			(setf (getf arguments :certificate) (or certificate (ssl-context-certificate context)))
			(setf (getf arguments :key) (or key
							(ssl-context-key context)
							;; CL+SSL fails to automatically extract the private key
							;; from PEM certificates, therefore we must point the
							;; key to the certificate if no key is specified
							(ssl-context-certificate context)))
			(alexandria:when-let (key-password (ssl-context-key-password context))
			  (setf (getf arguments :password) key-password))
			(when (eq stream-type :client)
			  (when verify
			    (setf (getf arguments :verify) verify))
			  (when server-name
			    (setf (getf arguments :hostname) server-name)))
			(apply (if (eq stream-type :client)
				   #'make-ssl-client-stream
				   #'make-ssl-server-stream)
			       (real-stream socket)
			       arguments)))
		    (with-global-context ((ssl-context-open-ssl-context (if (eq stream-type :client)
									    (make-ssl-context
									     :method method
									     :max-depth max-depth
									     :ca-file ca-file
									     :certificate certificate
									     :key key
									     :certificate-password certificate-password
									     :verify verify
									     :max-depth max-depth
									     :ca-file ca-file
									     :ca-directory ca-directory
									     :ciphers ciphers)
									    (make-ssl-context
									     :method method
									     :max-depth max-depth
									     :ca-file ca-file
									     :certificate certificate
									     :key key
									     :certificate-password certificate-password
									     :verify verify
									     :max-depth max-depth
									     :ca-file ca-file
									     :ca-directory ca-directory)))
					  :auto-free-p t)
		      (if (eq stream-type :client)
			  (make-ssl-client-stream (real-stream socket)
						  :certificate certificate
						  :key (or key certificate)
						  :password certificate-password
						  :verify verify
						  :hostname server-name)
			  (make-ssl-server-stream (real-stream socket)
					      :certificate certificate
					      :key (or key certificate)
					      :password certificate-password))))))
    (setf (real-stream socket) stream)
    socket))

(defun socket::make-ssl-client-stream (socket
				       &rest args
				       &key context
					    method
					    ciphers
					    certificate key certificate-password
					    verify max-depth server-name
					    ca-file ca-directory
					    crl-check crl-file)
  (declare (ignorable method ciphers verify max-depth server-name))
  (when (and context
	     (or certificate key ca-file ca-directory certificate-password crl-file crl-check))
    (error "Cannot supply both a CONTEXT and one of CERTIFICATE, KEY, CA-FILE, CA-DIRECTORY, CERTIFICATE-PASSWORD, CRL-FILE, or CRL-CHECK."))
  (apply #'make-ssl-stream socket :client args))

(defun socket::make-ssl-server-stream (socket
                                       &rest args
				       &key context
					    method
                                            certificate key certificate-password
                                            verify max-depth
                                            ca-file ca-directory
					    crl-file crl-check)
  (declare (ignorable method verify max-depth))
  (when (and context
	     (or certificate key ca-file ca-directory certificate-password crl-file crl-check))
    (error "Cannot supply both a CONTEXT and one of CERTIFICATE, KEY, CA-FILE, CA-DIRECTORY, CERTIFICATE-PASSWORD, CRL-FILE, or CRL-CHECK."))
  (apply #'make-ssl-stream socket :server args))

(defun socket:socket-control (socket &key (read-timeout nil read-timeout-p)
					  (write-timeout nil write-timeout-p))
  "Set SOCKET's read and write timeouts in seconds, NIL for none, as on
Allegro CL: a read or write that runs past one signals EXCL:SOCKET-ERROR."
  ;; AllegroServe relies on the read timeout to drop a client that stalls
  ;; inside a request body; without it the worker thread waited for CCL's
  ;; five-minute response timeout, and on EGCL forever.  EGCL has a
  ;; receive timeout but no send timeout yet.
  #-(or sbcl ccl egcl) (declare (ignore socket read-timeout write-timeout read-timeout-p write-timeout-p))
  #+egcl (declare (ignore write-timeout write-timeout-p))
  #+sbcl
  (when read-timeout-p
    (setf (read-timeout socket) read-timeout))
  #+sbcl
  (when write-timeout-p
    (setf (write-timeout socket) write-timeout))
  #+(or ccl egcl)
  (let ((stream (tcp-stream socket)))
    (when stream
      #+ccl
      (when read-timeout-p
        (setf (ccl:stream-input-timeout stream) read-timeout))
      #+ccl
      (when write-timeout-p
        (setf (ccl:stream-output-timeout stream) write-timeout))
      #+egcl
      (when read-timeout-p
        (egcl::%socket-read-timeout stream (and read-timeout (plusp read-timeout)
                                                (ceiling (* read-timeout 1000))))))))

#+(or ccl egcl)
(defun tcp-stream (socket)
  "SOCKET's own TCP stream, beneath any TLS layer; NIL for a listener."
  (let ((usocket (socket socket)))
    (when (typep usocket 'stream-usocket)
      (socket-stream usocket))))

(defun make-ssl-context (&key method
			      certificate
			      key
			      certificate-password
			      verify
			      max-depth
			      ca-file
			      ca-directory
			      ciphers)
  (let ((arguments (list)))
    (when method
      (if (listp method)
	  (error "Specifying a list of enabled SSL Methods is unsupported at the time.")
	  (ecase method
	    (:tlsv1+) ;; the default for CL+SSL: TLS-method
	    (:sslv3+
	     (warn "SSL v3 is deprecated and should not be used. Defaulting to TLS v1+ methods."))
	    (:sslv23
	     (error "SSL v2 is an unsecure protocol."))
	    (:sslv2
	     (error "SSL v2 is an unsecure protocol."))
	    (:sslv3
	     (warn "SSL v3 is deprecated and should not be used.")
	     (setf (getf arguments :method) (cl+ssl::ssl-v3-method)))
	    (:tlsv1
	     (setf (getf arguments :method) (cl+ssl::ssl-TLSv1-method))))))
    (when ciphers
      (unless (stringp ciphers) (error "Argument CIPHERS must be a string"))
      (setf (getf arguments :cipher-list) ciphers))
    ;; Verification documentation: https://www.openssl.org/docs/man1.1.1/man3/SSL_CTX_set_verify.html
    (if verify
	(ecase verify
	  (:optional
	   (setf (getf arguments :verify-mode)
		 cl+ssl::+SSL-VERIFY-PEER+))
	  (:required
	   (setf (getf arguments :verify-mode)
		 (logior cl+ssl::+SSL-VERIFY-PEER+
			 cl+ssl::+SSL-VERIFY-FAIL-IF-NO-PEER-CERT+))))
	(setf (getf arguments :verify-mode)
	      cl+ssl::+SSL-VERIFY-NONE+))
    (when (and ca-file
	       (pathnamep ca-file))
      (setf ca-file (namestring ca-file)))
    (when (and ca-directory
	       (pathnamep ca-directory))
      (setf ca-directory (namestring ca-directory)))
    (cond ((and ca-file ca-directory)
	   (setf (getf arguments :verify-location) (list ca-file ca-directory)))
	  (ca-file (setf (getf arguments :verify-location) ca-file))
	  (ca-directory (setf (getf arguments :verify-location) ca-directory)))
    (when max-depth
      (setf (getf arguments :verify-depth) max-depth))
    (when (and certificate
	       (pathnamep certificate))
      (setf certificate (namestring certificate)))
    (when (and key
	       (pathnamep key))
      (setf key (namestring key)))
    (let ((context (apply #'make-context arguments)))
      (when certificate
	(with-global-context (context)
	  (use-certificate-chain-file certificate)))
      (excl::make-ssl-context :open-ssl-context context
			      :certificate certificate
			      :key key
			      :key-password certificate-password))))

(defun socket:make-ssl-server-context (&rest args
				       &key (method :tlsv1+)
					    certificate
					    key
					    certificate-password
					    (verify nil)
					    (max-depth 10)
					    ca-file
					    ca-directory
					    ciphers
					    crl-check
					    crl-file
					    prefer-server-cipher-order
					    server-name)
  (declare (ignore certificate key certificate-password
		   ca-file ca-directory ciphers))
  (when (or crl-check
	    crl-file
	    prefer-server-cipher-order
	    server-name)
    (error "The arguments CRL-CHECK, CRL-FILE, PREFER-SERVER-CIPHER-ORDER, and SERVER-NAME are unsupported at the moment."))
  ;; Don't loose the defaults
  (setf (getf args :method) method)
  (setf (getf args :verify) verify)
  (setf (getf args :max-depth) max-depth)
  (apply #'make-ssl-context args))

(defun socket:make-ssl-client-context (&rest args
				       &key (method :tlsv1+)
					    certificate
					    key
					    certificate-password
					    (verify :optional)
					    (max-depth 10)
					    ca-file
					    ca-directory
					    ciphers
					    crl-check
					    crl-file
					    prefer-server-cipher-order)
  (declare (ignore certificate key certificate-password
		   ca-file ca-directory ciphers))
  (when (or crl-check
	    crl-file
	    prefer-server-cipher-order)
    (error "The arguments CRL-CHECK, CRL-FILE, and PREFER-SERVER-CIPHER-ORDER are unsupported at the moment."))
  ;; Don't loose the defaults
  (setf (getf args :method) method)
  (setf (getf args :verify) verify)
  (setf (getf args :max-depth) max-depth)
  (apply #'make-ssl-context args))

(defun socket:get-ssl-verify-result (ssl-stream)
  (cl+ssl::ssl-get-verify-result
   (cl+ssl::ssl-stream-handle
    (real-stream ssl-stream))))

(defun socket:get-ssl-peer-certificate (ssl-stream)
  (cl+ssl::ssl-get-peer-certificate
   (cl+ssl::ssl-stream-handle
    (real-stream ssl-stream))))

(defun socket:x509-certificate-subject (x509-certificate)
  (let ((subject-one-liner
	  (unless (cffi:null-pointer-p x509-certificate)
	    (cffi:with-foreign-pointer (buf 1024)
	      (let ((subject-name (cl+ssl::x509-get-subject-name x509-certificate)))
		(unless (cffi:null-pointer-p subject-name)
		  (cl+ssl::x509-name-oneline subject-name buf 1024)
		  (cffi:foreign-string-to-lisp buf)))))))
    (mapcar (lambda (element)
	      (let ((splitted (uiop:split-string element
						 :separator '(#\=))))
		(cons (first splitted)
		      (second splitted))))
	    (remove "" (uiop:split-string subject-one-liner
					  :separator '(#\/))
		    :test #'string=))))
