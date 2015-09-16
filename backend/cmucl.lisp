;;; cmucl.lisp

(in-package :fsocket)

(defun %get-stream-fscoket (socket element-typ)
  )

(defun file-socket-connect (filename &key (protocol :stream)
                                       (element-type 'character)
                                       local-filename)
  (let (socket)
    (with-mapped-conditions (socket)
      (ecase protocol
        (:stream
         (setf socket (ext:connect-to-unix-socket filename :stream))
         (%get-stream-fsocket socket element-type))
        (:datagram
         (unsupported '(protocol :datagram) 'file-socket-connect))))))

(defun file-socket-listen (filename &key (backlog 5) (element-type 'character))
  (with-mapped-conditions ()
    (let ((sock (ext:create-unix-listener filename :stream :backlog backlog)))
      (make-instance 'stream-server-fsocket :socket sock :element-type element-type))))

(defmethod socket-accept ((fsocket stream-server-fsocket) &key element-type)
  (with-mapped-conditions (fsocket)
    (let ((sock (ext:accept-unix-connection (socket fsocket))))
      (%get-stream-fsocket sock (or element-type
                                    (element-type fsocket))))))
(defmethod socket-send ((fsocket datagram-fsocket) buffer length &key filename)
  (unsupported 'fsocket 'socket-send))

(defmethod get-local-filename ((fsocket fsocket))
  nil)

(defmethod get-peer-filename ((fsocket stream-fsocket))
  nil)
