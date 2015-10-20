(in-package #:lbot)

(defparameter *text-content-types*
  '(("text" . nil)
    ("application" . "rss+xml")
    ("application" . "json")
    ("image" . "svg+xml")))

(defmacro with-content-types (&body body)
  `(let ((drakma:*text-content-types* *text-content-types*))
     ,@body))

(defun my-subseq (seq start &optional end)
  (check-type seq sequence)
  (check-type start fixnum)
  (check-type end (or fixnum null))
  (if (and end (> end (length seq)))
      (subseq seq start)
      (subseq seq start end)))

(defun last1 (l)
  (check-type l list)
  (car (last l)))

(defun trim (string &key inside)
  (check-type string string)
  (string-trim '(#\Space #\Tab #\Newline)
               (if inside
                   (ppcre:regex-replace-all "\\s+" string " ")
                   string)))

(defmacro with-system-path ((var system) &body body)
  `(let ((,var (ql:where-is-system ,system)))
     (if ,var
         (progn ,@body)
         (error (format nil "path for system \"~a\" not found" ,system)))))

(defun run-program (cmd)
  (multiple-value-bind (out err status)
      (asdf/run-program:run-program
       cmd
       :output '(:string :stripped t)
       :error-output '(:string :stripped t)
       :ignore-error-status t)
    (if (zerop status)
        out
        (error (format nil
                       "command ~s exited with status ~a: ~s"
                       cmd status err)))))

(defun git-version ()
  (with-system-path (path "lbot")
    (run-program (format nil
                         "cd ~a && git log -1 --format='%cD (%cr)'"
                         path))))

(define-condition update-progress ()
  ((message :initarg :message
            :accessor update-progress-message
            :initform "")))

(defun update ()
  (with-system-path (path "lbot")
    (signal 'update-progress :message "pulling code")
    (run-program (format nil "cd ~a && git pull" path))
    (loop for h in *on-update-hooks*
       do (progn
            (signal 'update-progress
                    :message (format nil "running hook ~a" h))
            (handler-case (funcall h)
              (error (e) (signal 'update-progress
                                 :message (format t "~a" e))))))
    (signal 'update-progress :message "reloading systems")
    (ql:quickload "lbot")
    (signal 'update-progress :message "done.")))

(defun format-time (&optional (time (get-universal-time)))
  (when (numberp time)
    (setf time (local-time:universal-to-timestamp time)))
  (format nil "~2,'0d-~2,'0d-~d ~2,'0d:~2,'0d"
          (local-time:timestamp-day time)
          (local-time:timestamp-month time)
          (local-time:timestamp-year time)
          (local-time:timestamp-hour time)
          (local-time:timestamp-minute time)))

(defun travis-status (repo &key (branch "master"))
  (check-type repo string)
  (check-type branch string)
  (let ((url (format nil "https://api.travis-ci.org/~a.svg?branch=~a" repo branch)))
    (with-content-types
      (multiple-value-bind (res code) (drakma:http-request url)
        (if (= code 200)
            (let* ((res (xmls:parse res))
                   (last-text (last1 (last1 (last1 res)))))
              (cond
                ((string-equal "passing" last-text) :passing)
                ((string-equal "failing" last-text) :failing)
                (t :unknown)))
            (error (format nil "http request to ~s returned ~a" url code)))))))

(defun get-github-repo (&optional (system (string-downcase (package-name #.*package*))))
  (check-type system (or string keyword))
  (with-system-path (path system)
    (let* ((out (run-program (format nil "cd ~a && git config --get remote.origin.url" path)))
           (repo (ppcre:register-groups-bind (match) ("^.*[:/]{1}(.+/.+).git$" out :sharedp t)
                   match)))
      (if repo repo (error (format nil "repo not found in ~a" out))))))

(defun convert-string (string from to)
  (check-type string string)
  (babel:octets-to-string
   (babel:string-to-octets string :encoding from)
   :encoding to))

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
                          ("(?i)<meta [^>]*charset=[\"']?([^\"'\\s>]+)[\"']" data)
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
        (let ((format (flexi-format-to-keyword
                       (flexi-streams:flexi-stream-external-format stream))))
          (if (eq format charset-set)
              title-set
              (convert-string title-set format (babel:make-external-format charset-set))))
        title-set)))

(defun flexi-format-to-keyword (format)
  (let* ((name (flexi-streams:external-format-name format))
         (page (if (eq name :code-page) (flexi-streams:external-format-id format) nil)))
    (if page
        (car (rassoc (list :code-page :id page) flexi-streams::+shortcut-map+ :test #'equal))
        name)))

(defun reply-nick (from)
  (check-type from string)
  (let ((pos (position #\/ from)))
    (when pos (subseq from (1+ pos)))))

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
          (format nil "~a~&~a" link (read-until stream condition :result-only t)))))))

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

(defun make-keyword (string)
  (check-type string string)
  (intern (string-upcase string) :keyword))

(defun fortune (&key short long file pattern)
  (let (args)
    (when file
      (push file args))
    (when short
      (push "-s" args))
    (when long
      (push "-l" args))
    (run-program (format nil "fortune -a ~{~a ~}" args))))

(defun yandex-translate (text &key lang-from (lang-to "ru"))
  (with-content-types
    (multiple-value-bind (data status)
        (drakma:http-request "https://translate.yandex.net/api/v1.5/tr.json/translate"
                             :method :post
                             :parameters `(("key" . ,*yandex-api-key*)
                                           ("lang" . ,(format nil "~:[~;~:*~a-~]~a"
                                                              lang-from lang-to))
                                           ("text" . ,text))
                             :external-format-out :utf-8)
      (if (= 200 status)
          (let ((data (cl-json:decode-json-from-string data)))
            (cadr (assoc :text data)))
          (error (format nil "yandex api returned ~a" status))))))

(defun lstack-push (item stack)
  (let ((content-size (length (lstack-content stack))))
    (if (= content-size (lstack-size stack))
        (setf (lstack-content stack)
              (push item (subseq (lstack-content stack) 0 (1- content-size))))
        (push item (lstack-content stack)))))

(defun lstack-pick (stack)
  (car (lstack-content stack)))

(defun hn-top-items (&optional (amount 5))
  (check-type amount integer)
  (with-content-types
    (multiple-value-bind (data status)
        (drakma:http-request "https://hacker-news.firebaseio.com/v0/topstories.json")
      (if (= 200 status)
          (let ((top (json:decode-json-from-string data)))
            (subseq top 0 amount))
          (error (format nil "hn api returned ~a" status))))))

(defun hn-item-info (id)
  (check-type id integer)
  (with-content-types
    (multiple-value-bind (data status)
        (drakma:http-request (format
                              nil
                              "https://hacker-news.firebaseio.com/v0/item/~a.json" id))
      (if (= 200 status)
          (json:decode-json-from-string data)
          (error (format nil "hn api returned ~a" status))))))

(defun format-hn-info (item)
  (format nil "~a [<a href='~a'>урл</a>] [<a href='https://news.ycombinator.com/item?id=~a'>дискач</a>]"
          (cdr (assoc :title item))
          (cdr (assoc :url item))
          (cdr (assoc :id item))))

(defun get-random-advice ()
  (multiple-value-bind (data status)
      (drakma:http-request "http://fucking-great-advice.ru/api/random")
    (if (= 200 status)
        (html-entities:decode-entities (cdr (assoc :text (json:decode-json-from-string data))))
        (error (format nil "great-advice api returned ~a" status)))))

(defun cbr-rates (&key
                    (range2 (local-time:universal-to-timestamp (get-universal-time)))
                    (range1 (local-time:timestamp- range2 1 :month))
                    (code "R01235"))
  "API DOCS: http://www.cbr.ru/scripts/Root.asp?PrtId=SXML"
  (flet ((date-formatter (d) (format nil "~2,'0d/~2,'0d/~d"
                                     (local-time:timestamp-day d)
                                     (local-time:timestamp-month d)
                                     (local-time:timestamp-year d))))
    (let ((date1 (date-formatter range1))
          (date2 (date-formatter range2)))
      (with-content-types
        (multiple-value-bind (data status)
            (drakma:http-request
             (format
              nil
              "http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=~a&date_req2=~a&VAL_NM_RQ=~a"
              date1 date2 code))
          (if (= 200 status)
              (let ((records (cddr (xmls:parse data))))
                (loop for r in records
                   collect (with-input-from-string
                               (in (ppcre:regex-replace-all ","  (third (fourth r)) "."))
                             (read in nil nil))))
              (error (format nil "cbr api returned ~a" status))))))))

(defun spark-data (data)
  (check-type data (or string list))
  (cond
    ((consp data) (run-program (format nil "spark ~{~a~^ ~}" data)))
    ((stringp data) (run-program (format nil "spark ~a" data)))
    (t (error (format nil "unsupported data type for spark-data: ~a" (type-of data))))))

(defun format-spark-data (data)
  (check-type data list)
  (let ((graph (spark-data data))
        (max (apply #'max data))
        (min (apply #'min data)))
    (format nil "~,2f ~a ~,2f (H: ~,2f L: ~,2f)"
            (car data) graph (last1 data)
            max min)))

(defun http-request (url)
  (check-type url string)
  (with-content-types
    (multiple-value-bind (data status headers)
        (drakma:http-request url)
      (if (= 200 status)
          (values data status headers)
          (error (format nil "request to ~s returned ~a" url status))))))

(defun cnn-rss-list (&key topic)
  (check-type topic (or null string))
  (let* ((url (format nil "http://rss.cnn.com/rss/edition~@[_~a~].rss" topic)))
    (rss:rss-site url)))

(defun rss-item-format (item)
  (format nil "~a" (rss:title item)))

(defun format-cnn-list (&key topic (amount 5))
  (let ((data (cnn-rss-list :topic topic)))
    (if data
        (with-output-to-string (*standard-output*)
          (format t "~a" (rss:title data))
          (loop for n in (my-subseq (rss:items data) 0 amount)
             do (format t "~&~a" (rss-item-format n))))
        (error (format nil "no data for topic ~a" topic)))))

(defun draw-line (l &key (empty-char #\Space) (fill-char #\.))
  (let ((fmt-string (format nil "~~:[~a~~;~a~~]" empty-char fill-char)))
    (loop for e in l
       do (format t fmt-string e))))

(defun draw-graph (data &key (empty-char #\Space) (fill-char #\.) max-height (min-step 0.5))
  (let* ((max (apply #'max data))
         (min (apply #'min data))
         (diff (- max min))
         (diff-height (/ diff min-step))
         (max-height (or max-height diff-height))
         (step (if (<= diff-height max-height)
                   min-step
                   (/ diff max-height))))
    (loop for i from max downto min by step
       do (progn
            (draw-line (mapcar #'(lambda (x) (>= x i)) data)
                       :empty-char empty-char
                       :fill-char fill-char)
            (format t "~&")))))

(defun short-url (url)
  (with-content-types
    (multiple-value-bind (data status)
        (drakma:http-request "http://nn.lv"
                             :method :post
                             :parameters `(("url" . ,url))
                             :external-format-out :utf-8)
      (if (= 200 status)
          data
          (error (format nil "request to ~s returned ~a" url status))))))

(defun readability-parse (url &key (token *readability-parser-token*))
  (let ((url (format nil "https://www.readability.com/api/content/v1/parser?token=~a&url=~a" token url)))
    (with-content-types
      (multiple-value-bind (data status)
          (drakma:http-request url)
        (if (= 200 status)
            (json:decode-json-from-string data)
            (error (format nil "readability api call returned ~a: ~a"
                           status (json:decode-json-from-string data))))))))

(defun remove-tags (str)
  (check-type str string)
  (ppcre:regex-replace-all "\\s+" (sanitize:clean str) " "))

(defun random-substr (str &optional (size 100))
  (check-type str string)
  (check-type size integer)
  (let* ((max (length str))
         (start (random (- max size))))
    (subseq str start (+ start size))))
