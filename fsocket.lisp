;;;; fsocket.lisp

(in-package #:fsocket)


(defclass fsocket (usocket) ()
  (:documentation "The main socket class for local domain (unix) sockets.

Also called file sockets, since they are (usually) assocated with a file."))

(defclass stream-fsocket (fsocket stream-usocket) ())
(defclass stream-server-fsocket (fsocket stream-server-usocket) ())
(defclass datagram-fsocket (fsocket datagram-usocket) ())

(defun file-socket-p (usocket)
  "Test if a socket is a local domain (UNIX, file) socket."
  (typep usocket 'stream-fsocket))

;;; interface for fsockets

(defgeneric get-local-filename (socket)
  (:documentation "Get the local filename for a file socket."))
(defgeneric get-peer-filename (socket)
  (:documentation "Get the peer (remote) file name for a file socket."))

(defmethod get-local-address ((socket fsocket))
  (get-local-filename socket))
(defmethod get-peer-address ((socket stream-fsocket))
  (get-peer-filename socket))

(defmethod get-local-port ((socket fsocket))
  nil)
(defmethod get-peer-port ((socket stream-fsocket))
  nil)

(defmethod get-local-name ((socket fsocket))
  (values (get-local-filename socket) nil))
(defmethod get-peer-name ((socket stream-fsocket))
  (values (get-peer-filename socket) nil))

(defgeneric fsocket-send (socket buffer length &key filename)
  (:documentation "Like socket-send but allows specifying the remote filename rather than a
host and port."))

(defmethod socket-send ((usocket datagram-fsocket) buffer length &key host port)
  "For fsockets, we define socket-send in terms of fsocket-send, and treat the host, if
supplied as the filename. Not that the host must be a string.

This makes it a little easier to treat inet sockets and unix sockets transparently."
  (declare (ignore port))
  (fsocket-send usocket buffer length :filename host))


;;; Macros

(defmacro with-client-file-socket ((socket-var stream-var file &rest args &key
                                               protocol element-type local-filename)
                                   &body body)
  "Bind the socket resulting from a call to `file-socket-connect' for `file'
with the keyword arguments to `socket-var' and if `stream-var' is non-nil
bind the assocated socket stream to it."
  `(with-connected-socket (,socket-var (file-socket-connect file ,@args))
     ,(if stream-var
          `(progn ,@body)
          `(let ((,stream-var (socket-stream ,socket-var)))
             ,@body))))

(defmacro with-file-socket-listener ((socket-var file &rest args &key backlog element-type)
                                     &body body)
  "Bind the socket resulting from a call to `file-socket-listen' for `file'
to `socket-var'. Keyword arguments are passed through."
  `(with-connected-socket (,socket-var (file-socket-listen file ,@args))
     ,@body))

;;; server

(defvar *peer-filename*)

(defun file-socket-server (file function &optional arguments
                           &key in-new-thread (protocol :stream)
                             ;; for datagrams
                             (timeout 1) (max-buffer-size +max-datagram-packet-size+)
                             ;; for streams
                             (element-type 'character) multi-threading
                             name)
  "Like socket-server but uses a file socket instead. `function' is called in a
context where *peer-filename* is bound to the filename of the client, if set."
  (let ((socket (ecase protocol
                  (:stream
                   (file-socket-listen file :element-type element-type))
                  (:datagram
                   (file-socket-connect nil :type :datagram
                                        :local-filename file)))))
    (let ((real-call
           (ecase protocol
             (:stream
              (lambda ()
                (stream-event-loop socket function arguments
                                   :multi-threading multi-threading)))
             (:datagram
              (lambda ()
                (datagram-event-loop socket function arguments
                                     :timeout timeout
                                     :max-buffer-size max-buffer-size))))))
      (if in-new-thread
          (values (spawn-thread (or name "FSOCKET Server") real-call))
          (funcall real-call)))))

(defun datagram-event-loop (socket function &optional arguments
                            &key timeout max-buffer-size)
  (let ((buffer (make-array max-buffer-size :element-type '(unsigned-byte 8) :initial-element 0))
        (sockets (list socket)))
    (unwind-protect
         (loop
            (multiple-value-bind (return-sockets time-remaining)
                (wait-for-input sockets :timeout timeout)
              (declare (ignore return-sockets))
              (when time-remaining
                (multiple-value-bind (recv n *peer-filename*)
                    (socket-receive socket buffer max-buffer-size)
                  (declare (ignore recv))
                  (if (plusp n)
                      (progn
                        (let ((reply
                               (apply function (subseq buffer 0 n) arguments)))
                          (when reply
                            (replace buffer reply)
                            (let ((n (socket-send socket buffer (length reply)
                                                  (fsocket-send socket buffer (length reply)
                                                                :filename *peer-filename*))))
                              (when (minusp n)
                                (error "send error: ~a~%" n))))))
                      (error "receive error: ~a~%" n))))
              #+scl (when thread:*quitting-lisp* (return))
              #+(and cmu mp) (mp:process-yield)))
      (socket-close socket)
      (values))))

(defun stream-event-loop (socket function &optional arguments
                          &key multi-threading)
  (let ((real-function (lambda (client-socket &rest arguments)
                         (unwind-protect
                              (let ((*remote-filename* (get-peer-filename client-socket)))
                                (apply function (socket-stream client-socket) arguments))
                           (close (socket-stream client-socket))
                           (socket-close client-socket)
                           nil))))
    (unwind-protect
         (loop
            (let* ((client-socket (socket-accept socket)))
              (if multi-threading
                  (apply #'spawn-thread "FSOCKET Client" real-function client-socket arguments)
                  (apply real-function client-socket arguments))
              #+scl (when thread:*quitting-lisp* (return))
              #+(and cmu mp) (mp:process-yield)))
      (socket-close socket)
      (values))))
