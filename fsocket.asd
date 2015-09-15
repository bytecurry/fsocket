;;;; fsocket.asd

(asdf:defsystem #:fsocket
  :description "Unix sockets extension for usocket."
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :depends-on (#:usocket)
  :serial t
  :components ((:file "package")
               (:file "fsocket")))
