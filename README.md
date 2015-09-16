# fsocket

foscket is an extension library to [usocket](https://common-lisp.net/project/usocket/) that provides support
for local-domain (UNIX) sockets.

The following implementations are suppported so far:

* sbcl
* allegro
* clozure (ccl)
* scieneer (scl)
* cmucl

Note that clozure, scieneer, and cmucl don't support datagram file sockets.
(If you want them, either pressure your implementation of choice, or send me a pull
request that gets them to work.)
