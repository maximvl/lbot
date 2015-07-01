(in-package :cl-user)

(defpackage lbot-test
  (:use :cl :prove :lbot)
  (:shadow :run)
  (:export :run))

(in-package :lbot-test)

(defun run ()
  (prove:run :lbot-test))

(setf *default-reporter* :list)

(plan 14)

(diag "testing utils")

(is "test" (lbot::trim (format nil "  ~t ~% test ~t ~%")))

(is "test" (lbot::starts-with-nick "nick" "nick test"))
(is "test" (lbot::starts-with-nick "nick" "nick: test"))
(is nil (lbot::starts-with-nick "nick" "nic test"))

(is :keyword (lbot::make-keyword "keyword"))

(is '(1 2 3) (lbot::my-subseq '(1 2 3 4 5) 0 3))
(is '(4 5) (lbot::my-subseq '(1 2 3 4 5) 3 5))
(is '(4 5) (lbot::my-subseq '(1 2 3 4 5) 3 10))
(is '(4 5) (lbot::my-subseq '(1 2 3 4 5) 3))

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

(with-input-from-encoded-string
    (s "<meta charset='utf-8'> <title>тайтл-ютф</title>" :utf-8)
  (is "тайтл-ютф" (lbot::get-title s)))

(with-input-from-encoded-string
    (s "<meta charset=\"windows-1251\"> <title>тайтл-1251</title>"
                                   :windows-1251)
  (is "тайтл-1251" (lbot::get-title s)))

(with-input-from-encoded-string
    (s "<title>тайтл-1251-2</title> <meta charset='windows-1251'>"
                                   :windows-1251)
  (is "тайтл-1251-2" (lbot::get-title s :buff-size 10)))

(with-input-from-encoded-string
    (s "<title>тайтл-ютф-реаль</title> <fake charset='windows-1251'> <meta some text charset='utf-8'>" :utf-8)
  (is "тайтл-ютф-реаль" (lbot::get-title s :buff-size 10)))


(finalize)
