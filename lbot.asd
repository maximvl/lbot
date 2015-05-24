;;;; lbot.asd

(asdf:defsystem #:lbot
  :serial t
  :description "Describe lbot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-xmpp
               #:bordeaux-threads
               #:optima
               #:optima.ppcre
               #:lparallel
               #:drakma)
  :components ((:file "package")
               (:file "lbot")))

