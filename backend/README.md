# Backends

An fsocket backend must implement the following:

### `(defun file-socket-connect (filename &key protocol element-type local-filename))`

This is the file socket equivalent of `socket-connect`. The arguments are the same except that
there is `local-filename` to bind the local addresss rather than `local-host` and `local-port`.
And `filename` is used for the remote address rather than a host and port.

### `(defun file-socket-listen (filename &key backlog element-type))`

This is the file socket equivalent of `socket-listen`. `filename` is the file to bind to,
`backlog` and `element-type` are the same as for `socket-listen`.

### `(defmethod socket-send ((fsocket datagram-fsocket) buffer length &key filename))`

The usocket method for socket-send doesn't work for datagram file sockets, because it assumes
the address is a host and port. Therefore the fsocket backend must implement the socket-send
method using the filename as the remote address if so desired.

### `(defmethod get-local-filename ((fsocket fsocket)))`

Accessor to get the locally bound filename for the socket.

### `(defmethod get-peer-filename ((fsocket stream-fsocket)))`

Accessor to get the remote filename for a stream socket.
