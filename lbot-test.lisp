(in-package :cl-user)

(defpackage lbot-test
  (:use :cl :prove :lbot))

(in-package :lbot-test)

(setf *default-reporter* :list)

(plan 9)

(diag "testing utils")

(is "test" (lbot::trim (format nil "  ~t ~% test ~t ~%")))

(is "test" (lbot::starts-with-nick "nick" "nick test"))
(is "test" (lbot::starts-with-nick "nick" "nick: test"))
(is nil (lbot::starts-with-nick "nick" "nic test"))

(diag "~&testing read-until")

(let* ((stream (make-string-input-stream "some test string"))
       (res (lbot::read-until stream
                              #'(lambda (data stream)
                                  (declare (ignore stream))
                                  (position #\Space data :test #'char=))
                              :buff-size 1)))
  (is 4 res)
  (is 5 (file-position stream)))

(diag "~&testing title parsing")

(defmacro with-input-from-encoded-string ((var string to-encode) &body body)
  (let ((stream (gensym)))
    `(let* ((,stream (flexi-streams:make-flexi-stream
                      (flexi-streams:make-in-memory-input-stream
                       (babel:string-to-octets ,string :encoding ,to-encode)))))
       (with-open-stream (,var ,stream)
         ,@body))))

(with-input-from-encoded-string (s "charset='utf-8' <title>тайтл-ютф</title>" :utf-8)
  (is "тайтл-ютф" (lbot::get-title s)))

(with-input-from-encoded-string (s "charset=\"windows-1251\" <title>тайтл-1251</title>"
                                   :windows-1251)
  (is "тайтл-1251" (lbot::get-title s)))

(with-input-from-encoded-string (s "<title>тайтл-1251-2</title> charset='windows-1251'"
                                   :windows-1251)
  (is "тайтл-1251-2" (lbot::get-title s :buff-size 10)))

(finalize)
