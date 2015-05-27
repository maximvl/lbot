;;;; lbot.lisp

(in-package #:lbot)

;;; "lbot" goes here. Hacks and glory await!

(require :cl-xmpp-tls)

(defparameter *connection* nil)

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun process-message (connection message)
  (unless (search (concatenate 'string "/" (xmpp:username connection))
                  (xmpp:from message))
    (format xmpp:*debug-stream* "~&~a msg ~a from ~a to ~a~%"
            (xmpp::type- message)
            (xmpp:body message)
            (xmpp:from message)
            (xmpp:to message))
    (when (xmpp:body message)
      (let ((msg-type (xmpp::type- message)))
        (cond
          ((equal msg-type "chat") (process-chat-message connection message))
          ((equal msg-type "groupchat")
           (process-groupchat-message connection message)))))))

(defun starts-with-nick (nick msg)
  (multiple-value-bind (starts rest)
      (alexandria:starts-with-subseq nick (trim msg)
                                     :return-suffix t)
    (when starts
      (let ((rest2 (trim rest)))
        (case (aref rest2 0)
          ((#\: #\,) (trim (subseq rest2 1)))
          (t rest2))))))

(defun process-chat-message (connection message)
  (unless (process-personal connection message)
    (unless (process-common connection message)
      (reply-chat connection (xmpp:from message)
                  "No." (xmpp::type- message)))))

(defun process-personal (connection message)
  (optima:match (xmpp:body message)
    ((equal "errors")
     (reply-chat connection (xmpp:from message)
                 (format-errors) (xmpp::type- message)))
    ((equal "rates")
     (reply-chat connection (xmpp:from message)
                 (format-rates (get-rates '("USDRUB" "EURRUB"))) 
                 (xmpp::type- message)))
    ((optima.ppcre:ppcre "^rates ([^ ]*)$" pairs)
     (reply-chat connection (xmpp:from message)
                 (format-rates (get-rates (split-sequence:split-sequence
                                           #\Space
                                           (string-upcase pairs))))
                 (xmpp::type- message)))
    ((optima.ppcre:ppcre "^rates ([^ ]*) (.*)$" curr1 curr2)
     (reply-chat connection (xmpp:from message)
                 (format-rates (get-rates (string-upcase curr1)
                                          (string-upcase curr2)))
                 (xmpp::type- message)))
    ((optima.ppcre:ppcre "^say (.*)$" text)
     (reply-chat connection (xmpp:from message)
                 text (xmpp::type- message)
                 :highlight (reply-nick (xmpp:from message))))
    ((optima.ppcre:ppcre "^man ([^:]+):([^/]+)/(.+)$" m f a)
     (reply-chat connection (xmpp:from message)
                 (get-erl-man-info m f a) (xmpp::type- message)))
    ((optima.ppcre:ppcre "^man ([^:]+):([^/]+)$" m f)
     (reply-chat connection (xmpp:from message)
                 (get-erl-man-info m f) (xmpp::type- message)))
    ((optima.ppcre:ppcre "^man ([^:]+)$" m)
     (reply-chat connection (xmpp:from message)
                 (get-erl-man-info m) (xmpp::type- message)))
    ((optima.ppcre:ppcre "^idea (.+)$" idea)
     (add-idea (reply-nick (xmpp:from message)) idea)
     (reply-chat connection (xmpp:from message)
                 "got it" (xmpp::type- message) 
                 :highlight (reply-nick (xmpp:from message))))
    ((equal "ideas")
     (reply-chat connection (xmpp:from message)
                 (get-ideas) (xmpp::type- message)))
    ((or (optima.ppcre:ppcre "^.* 300 .*$")
         (optima.ppcre:ppcre "^.* тристо .*$")
         (optima.ppcre:ppcre "^.* триста .*$"))
     (reply-chat connection (xmpp:from message)
                 "отсоси у тракториста" (xmpp::type- message)))))

(defun my-subseq (seq start &optional end)
  (if (> end (length seq))
      (subseq seq start)
      (subseq seq start end)))

(defun format-errors ()
  (with-output-to-string (s)
    (format s "~a total, last errors:" (length *errors*))
    (loop for e in (my-subseq *errors* 0 5)
       do (format s "~&~a" e))))

(defun process-groupchat-message (connection message)
  (let ((starts (starts-with-nick (xmpp:username connection)
                                  (xmpp:body message))))
    (if starts
        (progn
          (setf (xmpp:body message) starts)
          (unless (process-personal connection message)
            (let* ((pos (position #\/ (xmpp:from message)))
                   (to (if pos
                           (subseq (xmpp:from message) (1+ pos))
                           (xmpp:from message))))
              (reply-chat connection (xmpp:from message)
                          (format nil "~a: No." to) (xmpp::type- message)))))
        (process-common connection message))))

(defparameter *errors* nil)

(defclass user-error ()
  ((object
    :accessor object
    :initarg :object)
   (catched-at
    :accessor catched-at
    :initarg :catched-at
    :initform (get-universal-time))))

(defun make-user-error (obj)
  (make-instance 'user-error :object obj))

(defmethod print-object ((object user-error) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "error ~A at ~A" (object object)
            (format-time (catched-at object)))))

(defun format-time (time)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore second day-of-week dst-p tz))
    (format nil "~a-~a-~a ~a:~a" date month year hour minute)))

(defun callback-with-restart (&rest args)
  (restart-case
      (handler-case (apply #'xmpp::default-stanza-callback args)
        (t (err) (push (make-user-error err) *errors*)))
    (skip-stanza () '(ignored))))

(defun read-until (stream condition &key (buff-size 1024) result-only)
  (let ((buffer (make-array buff-size :adjustable t :fill-pointer t))
        (readed 0)
        (total-readed 0)
        (result nil))
    (loop
       (setf readed (read-sequence buffer stream
                                   :start total-readed))
       (decf readed total-readed)
       (incf total-readed readed)
       (when (< readed buff-size)
         (setf (fill-pointer buffer) total-readed))
       (setf result (funcall condition buffer stream))
       (when result
         (when result-only
           (return result))
         (return (values result buffer total-readed)))
       (when (< readed buff-size)
         (when result-only
           (return result))
         (return (values nil buffer total-readed)))
       (adjust-array buffer
                     (+ (array-total-size buffer) buff-size)
                     :fill-pointer t))))

(defun convert (string from to)
  (babel:octets-to-string 
   (babel:string-to-octets string :encoding from)
   :encoding to))

(defun get-http-page-title (url)
  (multiple-value-bind (stream status headers)
      (drakma:http-request url :want-stream t)
    (unwind-protect
         (when (and (= status 200) 
                    (search "text/html" (cdr (assoc :content-type headers))
                            :test #'equalp))
           (let (charset-set)
             (read-until stream
                         #'(lambda (data stream)
                             (unless charset-set
                               (ppcre:register-groups-bind (charset)
                                   ("(?i)charset=[\"']?([^\"'\\s>]+)" data)
                                 (setf charset-set t)
                                 (setf (flexi-streams:flexi-stream-external-format stream)
                                       (make-keyword (coerce charset 'string)))))
                             (ppcre:register-groups-bind
                                 (title)
                                 ("(?i)<title>([^<]*)</title>" data :sharedp t)
                               (coerce title 'string)))
                         :result-only t)))
    (close stream))))

(defun process-common (connection message)
  (optima:match (xmpp:body message)
    ((optima.ppcre:ppcre "(http[s]?://[\\S]+)" url)
     (let ((title (get-http-page-title url)))
       (when title
         (reply-chat connection (xmpp:from message)
                     title (xmpp::type- message)))))))

(defun reply-nick (from)
  (let ((pos (position #\/ from)))
    (when pos (subseq from (1+ pos)))))

(defun reply-chat (connection to reply kind &key highlight)
  (if (string-equal kind "groupchat")
    (let* ((pos (position #\/ to))
           (to (if pos (subseq to 0 pos) to))
           (reply (if highlight (format nil "~a: ~a" highlight reply) reply)))
      (xmpp:message connection to reply :type :groupchat))
    (xmpp:message connection to reply :type :chat)))

(defmethod xmpp:handle ((connection xmpp:connection) (message xmpp:message))
  (format xmpp:*debug-stream* "~&message: ~a" (xmpp:body message))
  (process-message connection message)
  message)

(defmethod xmpp:handle ((connection xmpp:connection) (event xmpp:presence))
  (format xmpp:*debug-stream* "~&presence: ~a" event)
  event)

(defmethod xmpp:handle ((connection xmpp:connection) (event xmpp:xml-element))
  (format xmpp:*debug-stream* "~&UNKNOWN ELEMENT: ~a" event)
  event)

(defmethod xmpp:handle ((connection xmpp:connection)
                        (event (eql :session-initiated)))
  (declare (special *room*))
  (when *room*
    (xmpp:presence connection
                   ;; :priority 1
                   :to (format nil "~a/~a" *room* (xmpp:username connection))
                   :x "http://jabber.org/protocol/muc"
                   :max-stanzas 0)))

(defmethod xmpp:get-element ((obj (eql nil)) name &key (test 'eq))
  (format xmpp:*debug-stream* "~&get-element: ~a ~a" obj name)
  nil)

(defmethod xmpp:presence ((connection xmpp:connection)
                          &key type to status show priority x pass max-stanzas)
  (xmpp::with-xml-output (connection)
    (cxml:with-element "presence"
      (when type
        (cxml:attribute "type" type))
      (when to
        (cxml:attribute "to" to))
      (when status
        (cxml:with-element "status"
          (cxml:text status)))
      (when show
        (cxml:with-element "show"
          (cxml:text show)))
      (when priority
        (cxml:with-element "priority"
          (cxml:text (format nil "~A" priority))))
      (when x
        (cxml:with-element "x"
          (cxml:attribute "xmlns" x)
          (when pass
            (cxml:with-element "password"
              (cxml:text pass)))
          (when max-stanzas
            (cxml:with-element "history"
              (cxml:attribute "maxstanzas" max-stanzas))))))))

(defun make-thread-name (login)
  (format nil "~a-xmpp-loop" login))

(defun make-mailboxed-thread (&rest args)
  (let ((*mailbox* (lparallel.queue:make-queue)))
    (declare (special *mailbox*))
    (values (apply #'bordeaux-threads:make-thread args) *mailbox*)))

(defun connect-async (&rest args)
  (let ((login (car args)))
    (kill-thread (get-xmpp-thread login))
    (bordeaux-threads:make-thread #'(lambda () (apply #'connect args))
                                  :name (make-thread-name login))))

(defun connect (login pass &key room (nick login))
  (let ((*room* room))
    (declare (special *room*))
    (setf *connection* (xmpp:connect-tls :hostname "jabber.ru"))
    (setf room (format nil "~a/~a" room nick))
    (unwind-protect (progn
                      (xmpp:auth *connection* login pass "/bot"
                                 :mechanism :sasl-digest-md5)
                      (xmpp:bind *connection* "emacs")
                      (xmpp:session *connection*)
                      (xmpp:presence *connection*
                                     :show "chat"
                                     :status "online")
                      (xmpp:receive-stanza-loop
                       *connection*
                       :stanza-callback #'callback-with-restart))
      (xmpp:disconnect *connection*))))

(defun start-loop ()
  (xmpp:receive-stanza-loop *connection*))

(defun send (to msg)
  (xmpp:message *connection* to msg))

(defun get-stanza ()
  (xmpp:receive-stanza *connection*))

(defun get-thread (name)
  (find name (bordeaux-threads:all-threads)
        :key #'bordeaux-threads:thread-name
        :test #'equal))

(defun get-xmpp-thread (user)
  (get-thread (make-thread-name user)))

(defun kill-thread (thread)
  (when (bordeaux-threads:threadp thread)
    (bordeaux-threads:destroy-thread thread)))

;; (defun man-content (manfile)
;;   (let ((out (make-string-output-stream))
;;         (err (make-string-output-stream)))
;;     (multiple-value-bind (res code)
;;         (external-program:run "man" `("-P" "cat" ,manfile)
;;                               :output out :error err)
;;       (declare (ignore res))
;;       (if (zerop code)
;;           (values (get-output-stream-string out) t)
;;           (values (get-output-stream-string err) nil)))))

;; (defun find-in-man (man start-regexp &key end-regexp max-chars)
;;   (let ((start (ppcre:all-matches start-regexp man)))
;;     (when start
;;       (when end-regexp
;;         (let ((end (ppcre:all-matches end-regexp man :start (cadr start))))
;;           (subseq man (car start) (cadr end))))
;;       (when max-chars
;;         (subseq man (car start) (+ (car start) max-chars))))))

;; (defun erl-fun-regex (fun &optional arity)
;;   (if (null arity)
;;       (format nil "~a\\([^)]*\\)\\s+->" fun)
;;       (cond
;;         ((zerop arity) (format nil "~a\\(\\)\\s+->" fun))
;;         (t (format nil "~a\\(([^),]+,){~a}[^)]+\\)\\s+->" fun (1- arity))))))

;; (defun find-erlang-fun (man fun &optional arity)
;;   (find-in-man man (erl-fun-regex fun arity) :max-chars 300))

(defun erl-man-link (module &optional fun arity)
  (string-downcase
   (cond
     ((and fun arity) (format nil "http://www.erlang.org/doc/man/~a.html#~a-~a" module fun arity))
     (fun (format nil "http://www.erlang.org/doc/man/~a.html#~a" module fun))
     (t (format nil "http://www.erlang.org/doc/man/~a.html" module)))))

(defun get-erl-man-module-discription (data)
  (let ((match (ppcre:all-matches "MODULE SUMMARY" data)))
    (when match
      (ppcre:register-groups-bind (summary)
          ("<div class=\"REFBODY\">([^<]+)</div>"
           data
           :start (cadr match)
           :sharedp t)
        summary))))

(defun get-erl-man-function-description (data function &optional arity)
  (let* ((regex (if arity
                    (format nil "<a name=\"~a-~a\"" function arity)
                    (format nil "<a name=\"~a-" function)))
         (match (ppcre:all-matches regex data)))
    (when match
      (let ((sig
             (ppcre:register-groups-bind (text)
                 ("<span class=\"bold_code\">(.*)<\/span>"
                  data
                  :start (cadr match)
                  :sharedp t)
               (coerce text 'string)))
            (result
             (ppcre:register-groups-bind (text)
                 ("(?s)<div class=\"REFBODY\">.*<\/div><\/p>\\n<div class=\"REFBODY\">(.*)<a"
                  data
                  :start (cadr match)
                  :sharedp t)
               (coerce text 'string))))
        (when (and sig result)
          (format nil "~a~%~a" (sanitize-string sig) (sanitize-string result)))))))

(defun sanitize-string (str)
  (trim (ppcre:regex-replace-all
         "&gt;"
         (ppcre:regex-replace-all
          "\\s+"
          (ppcre:regex-replace-all "<[^>]*>" str "") " ")
         ">")))

(defun get-erl-man-info (module &optional fun arity)
  (let ((link (erl-man-link module fun arity)))
    (multiple-value-bind (stream status)
        (drakma:http-request link :want-stream t)
      (when (= status 200)
        (let ((condition
               (cond
                 (fun
                  #'(lambda (data stream)
                      (declare (ignore stream))
                      (get-erl-man-function-description data fun arity)))
                 (t #'(lambda (data stream)
                        (declare (ignore stream))
                        (get-erl-man-module-discription data))))))
          (read-until stream condition :result-only t))))))

(defun get-rates (pairs)
  (let ((api-url (format nil "http://finance.yahoo.com/d/quotes.csv?e=.csv&f=sl1d1t1&s=~{~a=X~^+~}" pairs)))
    (multiple-value-bind (data status) (drakma:http-request api-url)
      (when (= 200 status)
      (let ((data (flexi-streams:octets-to-string 
                   data :external-format :utf8)))
        (cl-csv:read-csv data))))))

(defun format-rates (rates)
  (with-output-to-string (s)
    (loop for r in rates
       do (destructuring-bind (name rate date time) r
            (let ((curr1 (subseq name 0 3))
                  (curr2 (subseq name 3 6)))
              (format s "~&1 ~a = ~a ~a (updated: ~a ~a)" 
                      curr1 rate curr2 date time))))))


(defparameter *ideas* nil)

(defun add-idea (from idea)
  (push (list from idea (get-universal-time)) *ideas*))

(defun get-ideas ()
  (format nil "ideas: ~{~&~a~}" *ideas*))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))
