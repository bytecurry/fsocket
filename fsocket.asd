;;;; fsocket.asd

(asdf:defsystem #:fsocket
  :description "Unix sockets extension for usocket."
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "fsocket" :depends-on ("package"))
               (:module backend :depends-on ("fsocket")
                :components (#+allegro (:file "allegro")
                             #+clozure (:file "clozure")
                             #+sbcl (:file "sbcl")))))
