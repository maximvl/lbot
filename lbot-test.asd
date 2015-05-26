;;;; lbot.asd

;; Testing ASDF system
(asdf:defsystem #:lbot-test
  :depends-on (#:lbot #:prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "lbot-test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
