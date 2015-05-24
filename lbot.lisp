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
        (if (char= (aref rest2 0) #\:)
            (trim (subseq rest2 1))
            rest2)))))

(defun process-chat-message (connection message)
  (unless (process-personal connection message)
    (unless (process-common connection message)
      (reply-chat connection (xmpp:from message)
                  "No." (xmpp::type- message)))))

(defun process-personal (connection message)
  ;; (format xmpp:*debug-stream* "~&personal: ~a" message)
  ;; (format xmpp:*debug-stream* "~&personal: ~a" (xmpp:body message))
  (optima:match (xmpp:body message)
    ((optima.ppcre:ppcre "^say (.*)$" text)
     (reply-chat connection (xmpp:from message)
                 text (xmpp::type- message)))))

(defun process-groupchat-message (connection message)
  (let ((starts (starts-with-nick (xmpp:username connection)
                                  (xmpp:body message))))
    (if starts
        (progn
          (setf (xmpp:body message) starts)
          (process-personal connection message))
        (process-common connection message))))

(defun callback-with-restart (&rest args)
  (restart-case (apply #'xmpp::default-stanza-callback args)
    (skip-stanza () '(ignored))))

(defun read-until (stream condition &key (buff-size 100))
  (let ((buffer (make-array buff-size :adjustable t :fill-pointer t))
        (readed 0)
        (total-readed 0)
        (result nil))
    (loop
       (setf readed (read-sequence buffer stream
                                   :start total-readed))
       (decf readed total-readed)
       (incf total-readed readed)
       (setf result (funcall condition buffer))
       (when result
         (setf (fill-pointer buffer) readed)
         (return (values result buffer total-readed)))
       (when (< readed buff-size)
         (setf (fill-pointer buffer) readed)
         (return (values nil buffer total-readed)))
       (adjust-array buffer
                     (+ (array-total-size buffer) buff-size)
                     :fill-pointer t))))

(defun process-common (connection message)
  (optima:match (xmpp:body message)
    ((optima.ppcre:ppcre "(http[s]?://[\\S]+)" url)
     (multiple-value-bind (stream status headers) (drakma:http-request url :want-stream t)
       (unwind-protect
            (when (and (= status 200) (search "text/html" (cdr (assoc :content-type headers))))
              (let ((match (read-until stream #'(lambda (data)
                                                  (ppcre:register-groups-bind
                                                      (title)
                                                      ("(?i)<title>([^<]*)</title>" data :sharedp t)
                                                    title)))))
                (when match
                  (reply-chat connection (xmpp:from message)
                              match (xmpp::type- message)))))
         (close stream))))))

(defun reply-chat (connection to reply kind)
  (if (string-equal kind "groupchat")
    (let* ((pos (position #\/ to))
           (to (if pos (subseq to 0 pos) to)))
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
