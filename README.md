# fsocket

foscket is an extension library to [usocket](https://common-lisp.net/project/usocket/) that provides support
for local-domain (UNIX) sockets.

It provides the additional functions and macros to usocket:

* file-socket-connect
* file-socket-listen
* with-client-file-socket
* with-file-socket-listener
* file-socket-server
* get-local-filename
* get-peer-filename

They are all very similar to the functions and macros of similar names in usocket
and behave very similarly but have slightly different parameters appropriate to
unix sockets (such as using a filename for an address instead of a combination of host
and port, and no connection timeout options).

The sockets produced by these functions are in the following classes:

* fsocket
* stream-fsocket
* stream-server-fsocket
* datagram-fsocket

Which are subclasses of the similarly named classes from usocket.

The following implementations are suppported so far:

* sbcl
* allegro
* clozure (ccl)
* scieneer (scl)
* cmucl

Note that clozure, scieneer, and cmucl don't support datagram file sockets.
(If you want them, either pressure your implementation of choice, or send me a pull
request that gets them to work.)
