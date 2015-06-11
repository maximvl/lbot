;;;; lbot.asd

(asdf:defsystem #:lbot
  :serial t
  :description "Describe lbot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-xmpp
               #:cl-xmpp-sasl
               #:bordeaux-threads
               #:optima
               #:optima.ppcre
               #:lparallel
               #:drakma
               #:cl-csv
               #:split-sequence
               #:html-entities
               #:plump
               #:clon
               #:clsql
               #:cl-json
               #:local-time)
  :components ((:file "package")
               (:file "lbot")
               (:file "utils")))
