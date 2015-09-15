;;; clozure.lisp

(in-package :fsocket)

(defclass ccl-stream-fsocket (stream-fsocket)
  ((peer-filename :initarg :peer-filename
                  :reader get-peer-filename
                  :initform nil)
   (local-filename :initarg :local-filename
                   :reader get-local-filename
                   :initform nil)))
(defclass ccl-stream-server-fsocket (stream-server-fsocket)
  ((filename :initarg :filename
             :reader get-local-filename
             :initform nil)))

(defun file-socket-connect (filename &key (protocol :stream) (element-type 'character) local-filename)
  (with-mapped-conditions ()
    (let ((sock (ccl:make-socket :address-family :file
                                 :type typ
                                 :remote-filename filename
                                 :local-filename local-filename)))
      (ecase type
        (:stream
         (make-instance 'ccl-stream-fsocket :socket sock :stream sock
                        :peer-filename filename
                        :local-filename local-filename))
        (:datagram
         ;; ccl doesn't support datagram file-sockets
         (unsupported :datagram 'file-socket-connect)
         (make-instance 'datagram-fsocket :socket sock :connected-p (and filename t)))))))

(defun file-socket-listen (filename &key (backlog 5) (element-type 'character))
  (with-mapped-conditions ()
    (let ((sock (ccl:make-socket :address-family :file
                                 :type :stream
                                 :connect :passive
                                 :local-filename filename)))
      (make-instance 'ccl-stream-server-fsocket
                     :socket sock
                     :element-type element-type
                     :filename filename))))

(defmethod fsocket-send ((foscket datagram-fsocket) buffer length &key filename)
  (unsupported 'fsocket 'socket-send))
