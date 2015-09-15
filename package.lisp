;;;; package.lisp

(defpackage #:fsocket
  (:use #:cl #:usocket)
  (:export
   ;; connect and listen
   #:file-socket-connect
   #:file-socket-listen
   ;; classes
   #:fsocket
   #:stream-fsocket
   #:stream-server-fsocket
   #:datagram-fsocket
   ;; macros
   #:with-client-file-socket
   #:with-file-socket-listener
   ;; server
   #:file-socket-server
   ;; filename accessors
   #:get-local-filename
   #:get-peer-filename))
