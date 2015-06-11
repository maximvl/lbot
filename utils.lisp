(in-package #:lbot)

(defun my-subseq (seq start &optional end)
  (check-type seq sequence)
  (check-type start fixnum)
  (check-type end (or fixnum null))
  (if (and end (> end (length seq)))
      (subseq seq start)
      (subseq seq start end)))

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
        (error (format nil "command ~s exited with status ~a: ~s" cmd status err)))))

(defun git-version ()
  (with-system-path (path "lbot")
    (run-program (format nil "cd ~a && git log -1 --format='%cD (%cr)'" path))))

(defun reload ()
  (with-system-path (path "lbot")
    (run-program (format nil "cd ~a && git pull" path))
    (ql:quickload "lbot")))

(defun format-time (&optional (time (get-universal-time)))
  (check-type time integer)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore second day-of-week dst-p tz))
    (format nil "~2,'0d-~2,'0d-~d ~2,'0d:~2,'0d"
            date month year hour minute)))

(defun travis-status (repo &key (branch "master"))
  (check-type repo string)
  (check-type branch string)
  (let ((url (format nil "https://api.travis-ci.org/~a.svg?branch=~a" repo branch)))
    (multiple-value-bind (res code) (drakma:http-request url)
      (if (= code 200)
          (let* ((res (plump:parse (babel:octets-to-string res)))
                 (last-child (alexandria:last-elt
                              (plump:children (aref (plump:children res) 0))))
                 (last-text (plump:text
                             (aref (plump:children (alexandria:last-elt
                                                    (plump:children last-child)))
                                   0))))
            (cond
              ((string-equal "passing" last-text) :passing)
              ((string-equal "failing" last-text) :failing)
              (t :unknown)))
          (error (format nil "http request to ~s returned ~a" url code))))))

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

(defun yandex-translate (text &key (lang "ru"))
  (multiple-value-bind (data status)
      (drakma:http-request "https://translate.yandex.net/api/v1.5/tr.json/translate"
       :method :post :parameters `(("key" . ,*yandex-api-key*)
                                   ("lang" . ,lang)
                                   ("text" . ,text))
       :external-format-out :utf-8)
    (if (= 200 status)
        (let ((data (cl-json:decode-json-from-string
                     (babel:octets-to-string data))))
          (cadr (assoc :text data)))
        (error (format nil "yandex api returned ~a" status)))))

(defun lstack-push (item stack)
  (let ((content-size (length (lstack-content stack))))
    (if (= content-size (lstack-size stack))
        (setf (lstack-content stack)
              (push item (subseq (lstack-content stack) 0 (1- content-size))))
        (push item (lstack-content stack)))))

(defun lstack-pick (stack)
  (car (lstack-content stack)))
