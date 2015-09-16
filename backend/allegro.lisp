;;; allegro.lisp

(defun file-socket-connect (filename &key (protocol :stream) (element-type 'character) local-filename)
  (let ((sock (with-mapped-conditions
                  (socket:make-socket :address-family :file
                                      :type protocol
                                      :remote-filename file
                                      :local-filename local-filename))))
    (ecase protocol
      (:stream
       (make-instance 'stream-fsocket :socket sock :stream sock))
      (:datagram
       (make-instance 'datagram-fsocket :socket sock :connected-p (and filename t))))))

(defun socket-listen (filename &key (backlog 5) (element-type 'character))
  (let (sock (with-mapped-conditions ()
               (socket:make-socket :address-family :file
                                   :type :stream
                                   :connect :passive
                                   :local-filename filename)))
    (make-instance 'stream-server-socket :socket sock :element-type element-type)))

(defmethod socket-send ((fsocket datagram-fsocket) buffer length &key filename)
  (with-mapped-conditions (fsocket)
    (socket:send-to (socket fsocket) buffer length :remote-filename filename)))

(defmethod get-local-filename ((fsocket fsocket))
  (socket:local-filename (socket fsocket)))

(defmethod get-peer-filename ((fsocket stream-fsocket))
  (socket:remote-filename (socket fsocket)))
