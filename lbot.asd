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
               #:drakma
               #:cl-csv
               #:split-sequence)
  :components ((:file "package")
               (:file "lbot")))

;; Testing ASDF system
(asdf:defsystem #:lbot-test
  :depends-on (#:lbot #:prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "lbot-test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
