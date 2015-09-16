;; sbcl.lisp

(in-package :fsocket)

(defun file-socket-connect (filename &key
                                   (protocol :stream)
                                   (element-type 'character)
                                   local-filename)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                               :type protocol))
        usocket)
    (unwind-protect
         (with-mapped-conditions (usocket)
           (when local-filename
             (sb-bsd-sockets:socket-bind socket local-filename))
           (ecase protocol
             (:stream
              (sb-bsd-sockets:socket-connect socket filename)
              (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                               :input t :output t
                                                               :buffering :full
                                                               :element-type element-type
                                                               :serve-events nil)))
                (setf usocket (make-instance 'stream-fsocket
                                             :socket socket
                                             :stream stream))))
             (:datagram
              (when filename
                (sb-bsd-sockets:socket-connect socket filename))
              (setf usocket (make-instance 'datagram-fsocket
                                           :socket socket
                                           :connected-p (and filename t))))))
      (unless usocket
        (sb-bsd-sockets:socket-close socket :abort t)))
    usocket))

(defun file-socket-listen (filename &key (backlog 5) (element-type 'character))
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        usocket)
    (unwind-protect
         (with-mapped-conditions (usocket)
           (sb-bsd-sockets:socket-bind sock filename)
           (sb-bsd-sockets:socket-listen sock backlog)
           (setf usocket (make-instance 'stream-server-fsocket
                                        :socket sock
                                        :element-type element-type)))
      (unless usocket
        (sb-bsd-sockets:socket-close sock)))
    usocket))

(defmethod socket-send ((fsocket datagram-fsocket) buffer length &key filename)
  (with-mapped-conditions (fsocket)
    (sb-bsd-sockets:socket-send (socket fsocket) buffer length
                                :address (list filename))))

(defmethod get-local-filename ((fsocket fsocket))
  (nth-value 0 (sb-bsd-sockets:socket-name (socket fsocket))))

(defmethod get-peer-filename ((fsocket stream-fsocket))
  (nth-value 0 (sb-bsd-sockets:socket-peername (socket fsocket))))
