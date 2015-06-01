;;;; lbot.lisp

(in-package #:lbot)

;;; "lbot" goes here. Hacks and glory await!

(require :cl-xmpp-tls)

(defun trim (string &key inside)
  (check-type string string)
  (string-trim '(#\Space #\Tab #\Newline)
               (if inside
                   (ppcre:regex-replace-all "\\s+" string " ")
                   string)))

(defmacro with-system-path ((var system) &body body)
  `(let ((,var (ql:where-is-system ,system)))
     ,@body))

(defun git-version ()
  (with-system-path (system "lbot")
    (if system
        (multiple-value-bind (out err status)
            (asdf/run-program:run-program
             (format nil "cd ~a && git log -1 --format='%cD (%cr)'" system)
             :output '(:string :stripped t)
             :error-output '(:string :stripped t)
             :ignore-error-status t)
          (if (zerop status) out err))
        "lbot system path not found")))

(defun reload ()
  (with-system-path (system "lbot")
    (when system
      (multiple-value-bind (out err status)
          (asdf/run-program:run-program
           (format nil "cd ~a && git pull" system)
           :output '(:string :stripped t)
           :error-output '(:string :stripped t)
           :ignore-error-status t)
        (if (zerop status)
            (ignore-errors
              (ql:quickload "lbot")
              (values t out))
            (values nil err))))))

(defparameter *connection* nil)

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
                  "No." (xmpp::type- message)))))

(defun process-personal (connection message)
  (optima:match (xmpp:body message)
    ((equal "v")
     (reply-chat connection (xmpp:from message)
                 (git-version) (xmpp::type- message)))
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
    ((optima.ppcre:ppcre "^rates ([1-9]+) ([^\\s]+) to ([^\\s]+)$" amount from to)
     (reply-chat connection (xmpp:from message)
                 (format nil "~a ~a = ~a ~a" amount from
                         (convert-money (read-from-string amount) from to) to)
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
                 (format-ideas *ideas*) (xmpp::type- message)))
    ((equal "reload")
     (let* ((repo (get-github-repo))
            (status (if repo (travis-status repo)))
            (text (if (eq status :passing)
                      (multiple-value-bind (success text) (reload)
                        (if success (git-version) text))
                      (format nil "ci status: ~a (https://travis-ci.org/~a)" status repo))))
       (reply-chat connection (xmpp:from message)
                   text (xmpp::type- message))))
    ((equal "ci")
     (let* ((repo (get-github-repo))
            (status (if repo (travis-status repo))))
       (reply-chat connection (xmpp:from message)
                   (format nil "~a" status) (xmpp::type- message))))
    ((or (optima.ppcre:ppcre "(^|\\s)300($|\\s)")
         (optima.ppcre:ppcre "(^|\\s)тристо($|\\s)")
         (optima.ppcre:ppcre "(^|\\s)триста($|\\s)"))
     (reply-chat connection (xmpp:from message)
                 "отсоси у тракториста" (xmpp::type- message)))))

(defun my-subseq (seq start &optional end)
  (check-type seq sequence)
  (check-type start fixnum)
  (check-type end (or fixnum null))
  (if (and end (> end (length seq)))
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

(defclass idea ()
  ((user
    :accessor idea-user
    :initarg :user)
   (created-at
    :accessor idea-created-at
    :initarg :created-at
    :initform (get-universal-time))
   (text
    :accessor idea-text
    :initarg :text)))

(defmethod print-object ((object idea) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "idea: ~A from ~A at ~A"
            (idea-text object)
            (idea-user object)
            (format-time (idea-created-at object)))))

(defun format-time (&optional (time (get-universal-time)))
  (check-type time integer)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore second day-of-week dst-p tz))
    (format nil "~2,'0d-~2,'0d-~d ~2,'0d:~2,'0d"
            date month year hour minute)))

(defun callback-with-restart (&rest args)
  (restart-case
      (handler-case (apply #'xmpp::default-stanza-callback args)
        (t (err) (push (make-user-error err) *errors*)))
    (skip-stanza () '(ignored))))

(defun read-until (stream condition &key (buff-size 1024) result-only)
  (check-type buff-size integer)
  (check-type condition function)
  (let ((buffer (make-array buff-size :adjustable t :fill-pointer t :element-type 'character))
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

(defun convert-string (string from to)
  (check-type string string)
  (check-type from keyword)
  (check-type to keyword)
  (babel:octets-to-string
   (babel:string-to-octets string :encoding from)
   :encoding to))

(defun get-http-page-title (url)
  (check-type url string)
  (multiple-value-bind (stream status headers)
      (drakma:http-request url :want-stream t)
    (when (and (= status 200)
               (search "text/html" (cdr (assoc :content-type headers))
                       :test #'equalp))
      (with-open-stream (s stream)
        (let ((title (get-title s)))
          (when title
            (trim title :inside t)))))))

(defun get-title (stream &key (buff-size 1024))
  (check-type buff-size integer)
  (let (charset-set title-set)
    (read-until stream
                #'(lambda (data stream)
                    (declare (ignore stream))
                    (unless charset-set
                      (ppcre:register-groups-bind (charset)
                          ("(?i)charset=[\"']?([^\"'\\s>]+)[\"']" data)
                        (setf charset-set (make-keyword charset))))
                    (unless title-set
                      (ppcre:register-groups-bind
                          (title)
                          ("(?i)<title>([^<]*)</title>" data :sharedp t)
                        (setf title-set (html-entities:decode-entities title))))
                    (cond
                      ((and charset-set title-set) t)
                      ((ppcre:all-matches "(?i)</head>" data) t)
                      (t nil)))
                :result-only t
                :buff-size buff-size)
    (if charset-set
        (convert-string title-set
                        (flexi-streams:external-format-name
                         (flexi-streams:flexi-stream-external-format
                          stream))
                        charset-set)
        title-set)))

(defun process-common (connection message)
  (optima:match (xmpp:body message)
    ((optima.ppcre:ppcre "(http[s]?://[\\S]+)" url)
     (let ((title (get-http-page-title url)))
       (when title
         (reply-chat connection (xmpp:from message)
                     title (xmpp::type- message)))))))

(defun reply-nick (from)
  (check-type from string)
  (let ((pos (position #\/ from)))
    (when pos (subseq from (1+ pos)))))

(defun reply-chat (connection to reply kind &key highlight)
  (check-type reply (or string null))
  (check-type highlight (or string null))
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

(defun connect (login pass &key room (nick login))
  (check-type login string)
  (check-type pass string)
  (check-type room string)
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
  (check-type module string)
  (check-type fun (or string null))
  (check-type arity (or string null))
  (string-downcase
   (cond
     ((and fun arity) (format nil "http://www.erlang.org/doc/man/~a.html#~a-~a"
                              module fun arity))
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
  (check-type str string)
  (trim (html-entities:decode-entities
         (ppcre:regex-replace-all "<[^>]+>" str ""))
        :inside t))

(defun get-erl-man-info (module &optional fun arity)
  (check-type module string)
  (check-type fun (or string null))
  (check-type arity (or string null))
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

(defun convert-money (amount from to)
  (check-type amount number)
  (check-type from string)
  (check-type to string)
  (let ((rate (cadar (get-rates (list (concatenate 'string from to))))))
    (let ((rate (read-from-string rate)))
      (when (numberp rate)
        (* amount rate)))))

(defvar *ideas* nil)

(defun add-idea (from idea)
  (push (make-instance 'idea
                       :user from
                       :text idea)
        *ideas*))

(defun format-ideas (ideas)
  (format nil "ideas: ~{~&~a~}" *ideas*))

(defun make-keyword (string)
  (check-type string string)
  (intern (string-upcase string) :keyword))

(defun travis-status (repo &key (branch "master"))
  (check-type repo string)
  (check-type branch string)
  (multiple-value-bind (res code)
      (ignore-errors
        (drakma:http-request
         (format nil "https://api.travis-ci.org/~a.svg?branch=~a" repo branch)))
    (when (and code (= code 200) res)
      (let* ((res (plump:parse (babel:octets-to-string res)))
             (last-child (alexandria:last-elt (plump:children (aref (plump:children res) 0))))
             (last-text (plump:text (aref (plump:children
                                           (alexandria:last-elt (plump:children last-child)))
                                          0))))
        (cond
          ((string-equal "passing" last-text) :passing)
          ((string-equal "failing" last-text) :failing)
          (t :unknown))))))

(defun get-github-repo (&optional (system (string-downcase (package-name *package*))))
  (check-type system (or string keyword))
  (with-system-path (path system)
    (if path
        (multiple-value-bind (out err status)
            (asdf/run-program:run-program "git config --get remote.origin.url"
                                          :output '(:string :stripped t)
                                          :error-output '(:string :stripped t)
                                          :ignore-error-status t)
          (if (zerop status)
              (let ((repo (ppcre:register-groups-bind (match) (":(.+).git$" out :sharedp t)
                            match)))
                (if repo
                    (values repo nil)
                    (values nil (format nil "repo not found in ~a" out))))
              (values nil err)))
        (values nil (format nil "~a system path not found" system)))))
