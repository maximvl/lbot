;;;; lbot.lisp

(in-package #:lbot)

;;; "lbot" goes here. Hacks and glory await!

(require :cl-xmpp-tls)

(defstruct lstack
  size
  content)

;; classes

(clsql:def-view-class idea ()
  ((id :db-kind :key
       :db-contstraints :not-null
       :type integer
       :initarg :id
       :accessor idea-id)
   (user :type string
         :initarg :user
         :accessor idea-user)
   (text :type (string 300)
         :initarg :text
         :accessor idea-text)
   (created-at :type integer
               :initarg :created-at
               :accessor idea-created-at
               :initform (local-time:timestamp-to-unix
                          (local-time:universal-to-timestamp
                           (get-universal-time))))))


(defmethod print-object ((object idea) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "idea: ~A by ~A at ~A"
            (idea-text object)
            (idea-user object)
            (format-time (idea-created-at object)))))

(defun add-idea (from idea)
  (let ((idea (make-instance 'idea :user from :text idea)))
    (clsql:update-records-from-instance idea)))

(defun format-ideas (&optional (n 10))
  (let ((ideas (clsql:select 'idea
                             :limit n
                             :order-by '(((id) :desc))
                             :flatp t
                             :refresh t)))
    (format nil "ideas: ~{~&~a~}" ideas)))

;; global vars

(defvar *connection* nil)
(defvar *db* (clsql:connect '("lbot.sqlite3")
                            :make-default t :database-type :sqlite3))

(defparameter *yandex-api-key* nil)
(defparameter *jabber-login* nil)
(defparameter *jabber-password* nil)
(defparameter *jabber-server* nil)
(defparameter *jabber-room* nil)

(defparameter *on-update-hooks* nil)
(defparameter *last-messages* (make-lstack :size 10))

(defun load-config (&optional (file "config.lisp"))
  (handler-case (load file :verbose t :print t)
    (error (e) (format t "CONFIG LOAD FAILED: ~a" e))))

(load-config)

;; code
(defun setup-db (&optional (db *db*))
  (unless (clsql:table-exists-p 'idea)
    (clsql:create-view-from-class 'idea :database db)))

(push #'setup-db *on-update-hooks*)

(defun process-message (connection message)
  (lstack-push (xmpp:body message) *last-messages*)
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
  (check-type nick string)
  (check-type msg string)
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
                  (nth (random 2) '("No." "Yes.")) (xmpp::type- message)))))

(defun replace-last-messages (text)
  (let ((content (lstack-content *last-messages*)))
    (ppcre:regex-replace-all
     "\\^"
     (ppcre:regex-replace-all
      "\\^\\^"
      (ppcre:regex-replace-all
       "\\^\\^\\^"
       text
       (fourth content))
      (third content))
     (second content))))

(defun process-personal (connection message)
  (let ((body (replace-last-messages (xmpp:body message))))
    (optima:match body
      ((equal "v")
       (reply-chat connection (xmpp:from message)
                   (handler-case (git-version)
                     (error (e) (format nil "~a" e)))
                   (xmpp::type- message)))
      ((equal "errors")
       (reply-chat connection (xmpp:from message)
                   (format-errors) (xmpp::type- message)))
      ((equal "rates")
       (reply-chat connection (xmpp:from message)
                   (format-rates (get-rates '("USDRUB" "EURRUB")))
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "^rates ([^\\s]+)$" pairs)
       (reply-chat connection (xmpp:from message)
                   (format-rates (get-rates (split-sequence:split-sequence
                                             #\Space pairs)))
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "^rates ([^\\s]+) ([^\\s]+)$" curr1 curr2)
       (reply-chat connection (xmpp:from message)
                   (format-rates (get-rates (list (concatenate 'string curr1 curr2))))
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "^rates ([0-9]+) ([^\\s]+) to ([^\\s]+)$" amount from to)
       (reply-chat connection (xmpp:from message)
                   (format nil "~a ~a = ~a ~a" amount from
                           (convert-money (read-from-string amount) from to) to)
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "^say$")
       (reply-chat connection (xmpp:from message)
                   (handler-case (fortune :short t)
                     (error (e) (format nil "~a" e)))
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "^say (.*)$" text)
       (reply-chat connection (xmpp:from message)
                   text (xmpp::type- message)
                   :highlight (reply-nick (xmpp:from message))))
      ((optima.ppcre:ppcre "^saychat (.*)$" text)
       (reply-chat connection (xmpp:from message)
                   text "groupchat"))
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
       (add-idea (xmpp:from message) idea)
       (reply-chat connection (xmpp:from message)
                   "got it" (xmpp::type- message)
                   :highlight (reply-nick (xmpp:from message))))
      ((equal "ideas")
       (reply-chat connection (xmpp:from message)
                   (format-ideas 10) (xmpp::type- message)))
      ((equal "0071")
       (reply-chat connection (xmpp:from message)
                   body (xmpp::type- message)
                   :xhtml (format nil "<a href='http://planet.lisp.org/'>lisp-planet</a>~%<img src='http://www.lisperati.com/lisplogo_alien_256.png'/>")))
      ((equal "reload")
       (let ((reply
              (handler-case
                  (let* ((repo (get-github-repo))
                         (status (travis-status repo)))
                    (if (eq status :passing)
                        (progn (handler-bind
                                   ((update-progress #'(lambda (p)
                                                         (reply-chat connection (xmpp:from message)
                                                                     (update-progress-message p) (xmpp::type- message)))))
                                 (update))
                               (git-version))
                        (format nil "ci status: ~a (https://travis-ci.org/~a)"
                                status repo)))
                (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((equal "reload!")
       (let ((reply (handler-case (progn
                                    (update)
                                    (git-version))
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((optima.ppcre:ppcre "^ci (.+)$" branch)
       (let ((reply (handler-case
                        (format nil "~a"
                                (travis-status (get-github-repo) :branch branch))
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((equal "ci")
       (let ((reply (handler-case
                        (format nil "~a"
                                (travis-status (get-github-repo)))
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((equal "hntop")
       (let* ((items (loop for id in (hn-top-items)
                        collect (hn-item-info id)))
              (reply (format nil "~{~a~^~&~}" (mapcar #'(lambda (e) (cdr (assoc :title e))) items)))
              (reply-xhtml (format nil "~{~a~^~&<br/>~}" (mapcar #'format-hn-info items))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message) :xhtml reply-xhtml)))
      ((equal "mem")
       (let ((reply (with-output-to-string (*standard-output*) (room))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((optima.ppcre:ppcre "^tr ([a-zA-Z]{2}) (.+)$" lang text)
       (let ((reply (handler-case (yandex-translate text :lang-to lang)
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((optima.ppcre:ppcre "^tr (.+)$" text)
       (let ((reply (handler-case (yandex-translate text)
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((or (optima.ppcre:ppcre "(?i)(http[s]?://[\\S]+.jpg)" url)
           (optima.ppcre:ppcre "(?i)(http[s]?://[\\S]+.jpeg)" url)
           (optima.ppcre:ppcre "(?i)(http[s]?://[\\S]+.png)" url)
           (optima.ppcre:ppcre "(?i)(http[s]?://[\\S]+.gif)" url))
       (reply-chat connection (xmpp:from message)
                   "force your client to support xep-0071 already" (xmpp::type- message)
                   :xhtml (format nil "<img src='~a'/>" url)))
      ((equal "spark rates")
       (let ((reply (handler-case (format-spark-data (cbr-rates))
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((optima.ppcre:ppcre "^top ([\\S]+) ?([0-9]+)?$" topic amount)
       (let* ((amount (or amount 5))
              (reply (handler-case (format-cnn-list :topic topic
                                                    :amount amount)
                       (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message))))
      ((equal "src")
       (reply-chat connection (xmpp:from message)
                   "https://github.com/maximvl/lbot"
                   (xmpp::type- message)))
      ((equal "?")
       (let ((reply (handler-case (get-random-advice)
                      (error (e) (format nil "~a" e)))))
         (reply-chat connection (xmpp:from message)
                     reply (xmpp::type- message)))))))

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
                          (format nil "~a: ~a" to (nth (random 2) '("No." "Yes.")))
                          (xmpp::type- message)))))
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

(defun callback-with-restart (&rest args)
  (restart-case
      (handler-case (apply #'xmpp::default-stanza-callback args)
        (error (err) (push (make-user-error err) *errors*)))
    (skip-stanza () '(ignored))))

(defun process-common (connection message)
  (declare (special *room*))
  (unless (string-equal (xmpp:from message) *room*)
      (optima:match (xmpp:body message)
                    ((optima.ppcre:ppcre "(http[s]?://[\\S]+)" url)
                     (let ((title (get-http-page-title url)))
                       (when title
                         (reply-chat connection (xmpp:from message)
                                     title (xmpp::type- message)))))
                    ((or (optima.ppcre:ppcre "(^|\\s)300($|\\s)")
                         (optima.ppcre:ppcre "(^|\\s)тристо($|\\s)")
                         (optima.ppcre:ppcre "(^|\\s)триста($|\\s)"))
                     (reply-chat connection (xmpp:from message)
                                 "отсоси у тракториста" (xmpp::type- message)
                                 :highlight (reply-nick (xmpp:from message)))))))

(defmethod xmpp:message ((connection xmpp:connection)
                         to body &key id (type :chat) xhtml-body)
  (cl-xmpp::with-xml-output (connection)
    (cxml:with-element "message"
      (cxml:attribute "to" to)
      (when id (cxml:attribute "id" id))
      (when type (cxml:attribute "type" (string-downcase (string type))))
      (cxml:with-element "body" (cxml:text body))
      (when xhtml-body
        (cxml:with-element "html"
          (cxml:attribute "xmlns" "http://jabber.org/protocol/xhtml-im")
          (cxml:with-element "body"
            (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
            (cxml:unescaped xhtml-body)))))))

(defun reply-chat (connection to reply kind &key highlight xhtml)
  (check-type reply (or string null))
  (check-type highlight (or string null))
  (if (string-equal kind "groupchat")
    (let* ((pos (position #\/ to))
           (to (if pos (subseq to 0 pos) to))
           (reply (if highlight (format nil "~a: ~a" highlight reply) reply)))
      (xmpp:message connection to reply :type :groupchat :xhtml-body xhtml))
    (xmpp:message connection to reply :type :chat :xhtml-body xhtml)))

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
  (check-type login string)
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

(defun connect (&optional (login *jabber-login*) (pass *jabber-password*)
                &key (room *jabber-room*)
                  (nick *jabber-login*)
                  (server *jabber-server*))
  (check-type login string)
  (check-type pass string)
  (check-type room string)
  (handler-case (setup-db)
    (error (e) (format t "error while setupping db: ~a" e)))

  (let ((*room* room))
    (declare (special *room*))
    (setf *connection* (xmpp:connect-tls :hostname server))
    (handler-case (unwind-protect
                       (progn (xmpp:auth *connection* login pass "/bot"
                                         :mechanism :sasl-digest-md5)
                              (xmpp:bind *connection* "emacs")
                              (xmpp:session *connection*)
                              (xmpp:presence *connection*
                                             :show "chat"
                                             :status "online")
                              (xmpp:receive-stanza-loop
                               *connection*
                               :stanza-callback #'callback-with-restart))
                    (xmpp:disconnect *connection*))
      (error (e)
        (progn
          (format t "~&XMPP ERROR: ~a" e)
          (push (make-user-error e) *errors*)
          (sleep 5)
          (connect login pass :room room :nick nick :server server))))))

(defun start-loop ()
  (xmpp:receive-stanza-loop *connection*))

(defun send (to msg)
  (xmpp:message *connection* to msg))

(defun send-groupchat (to msg)
  (reply-chat *connection* to msg "groupchat"))

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

(defun maybe-say (chat text-getter &key (idle-time (* 60 10)))
  (when (> (get-universal-time) (+ *last-message-time* idle-time))
    (send-groupchat chat (funcall text-getter))))
