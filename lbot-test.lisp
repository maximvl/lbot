(in-package :cl-user)

(defpackage lbot-test
  (:use :cl :prove :lbot))

(in-package :lbot-test)

(setf *default-reporter* :list)

(plan 6)

(is "test" (lbot::trim (format nil "  ~t ~% test ~t ~%")))

(is "test" (lbot::starts-with-nick "nick" "nick test"))
(is "test" (lbot::starts-with-nick "nick" "nick: test"))
(is nil (lbot::starts-with-nick "nick" "nic test"))

(let* ((stream (make-string-input-stream "some test string"))
       (res (lbot::read-until stream
                              #'(lambda (data)
                                  (position #\Space data :test #'char=))
                              :buff-size 1)))
  (is 4 res)
  (is 5 (file-position stream)))

(finalize)
