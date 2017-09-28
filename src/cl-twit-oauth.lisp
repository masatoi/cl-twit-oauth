;;; -*- Coding:utf-8; Mode:Lisp; -*-

(in-package :cl-user)

(defpackage cl-twit-oauth
  (:use :cl :cl-who))

(in-package :cl-twit-oauth)

;;; Settings

(defparameter *callback-uri* "http://localhost:5000/")

;;; URL settings
(defparameter *get-request-token-endpoint* "https://api.twitter.com/oauth/request_token")
(defparameter *auth-request-token-endpoint* "https://api.twitter.com/oauth/authorize")
(defparameter *get-access-token-endpoint* "https://api.twitter.com/oauth/access_token")
(defparameter *request-token* nil)
(defparameter *access-token* nil)

;;; insert your credentials and auxiliary information here.
(defvar *consumer-key*)
(defvar *consumer-secret*)

(let ((setting-file (merge-pathnames (user-homedir-pathname) ".cl-twit-oauth")))
  (if (uiop/filesystem:file-exists-p setting-file)
      (load setting-file)
      (error "File not exist: ~~/.cl-twit-oauth")))

;;; Example of .cl-twit-oauth
;; (setf *consumer-key* "xxxxxxxxxxxxxxxxxx")
;; (setf *consumer-secret* "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

(defparameter *consumer-token* (oauth:make-consumer-token :key *consumer-key* :secret *consumer-secret*))

;; set drakma coding system from default LATIN-1 to UTF-8
(setf drakma:*drakma-default-external-format* :utf-8)

;;; for debug Hunchentoot
(setf hunchentoot:*catch-errors-p* t)

;;; Setting application
(defparameter *app* (make-instance 'ningle:<app>))
(defvar *handler*)

;;; Setting for Static
(defparameter *app-root-directory* (asdf:system-source-directory :cl-twit-oauth))
(defparameter *static-directory*   (merge-pathnames #P"static/" *app-root-directory*))

(defun start-server (&key (port 5000))
  (setf *handler*
	(clack:clackup
	 (lack:builder
	  (:static :path
                   (lambda (path)
                     (if (ppcre:scan "^(?:/images/|/css/|/js/|/fonts/|/robot\\.txt$|/favicon.ico$)"
                                     path)
                         path
                         nil))
		   :root *static-directory*)
	  *app*)
         :port port)))

(defun stop-server ()
  (clack:stop *handler*))

;;; Authentication Pages
(defmacro defroute (name (params &rest route-args) &body body)
  "Example:
 (defroute \"/\" (params)
   (html (:p \"Hello,world!\")))

 (defroute \"/login\" (params :method :POST)
   (if (authorize (assoc \"username\" params :test #'string=)
 		 (assoc \"password\" params :test #'string=))
     \"Authorized!\"
     \"Failed...Try again.\"))"
  `(setf (ningle:route *app* ,name ,@route-args)
	 (lambda (,params) ,@body)))

(defmacro html (&body body)
  (let ((s (gensym)))
    `(with-html-output-to-string (,s)
       ,@body)))

(defmacro with-head
    ((&key title css script favicon (content-type "text/html") (encoding "UTF-8"))
     &body body)
  "Example:
 (with-head (:title \"Title\" :css \"foo.css\")
   (html (:p \"Hello,World!\")))

 (with-head (:title \"Title\" :css (\"foo.css\" \"bar.css\"))
   (html (:p \"Hello,World!\")))"
  `(html
     (:head ,@(if title `((:title ,title)))
            ,@(if (consp css)
                  (mapcar (lambda (css)
                            `(:link :rel "stylesheet" :type "text/css" :href ,css :media "all"))
                          css)
                  `((:link :rel "stylesheet" :type "text/css" :href ,css :media "all")))
            ,@(if (consp script)
                  (mapcar (lambda (script) `(:script :src ,script)) script)
                  `((:script :src ,script)))
            ,@(if favicon `((:link :rel "shortcut icon" :type "image/vnd.microsoft.icon" :href ,favicon)
                            (:link :rel "icon" :type "image/vnd.microsoft.icon" :href ,favicon)))
            (:meta :http-equiv "Content-Type"
                   :content ,(format nil "~A; charset=~A" content-type encoding)))
     (str ,@body)))

(defun conv4pre (str)
  (ppcre:regex-replace-all
   ">"
   (ppcre:regex-replace-all "<" str "&lt;")
   "&gt;"))

(defun pprint-str (obj)
  (with-output-to-string (s) (pprint obj s)))

(defroute "/" (params)
  (cond (*access-token* (print "Already authorized!"))
        ((and params
              (assoc "oauth_token" params :test #'equal)
              (string= (oauth:token-key *request-token*)
                       (cdr (assoc "oauth_token" params :test #'equal))))
         ;; update verification-code of request-token
         (setf (oauth:request-token-verification-code *request-token*)
               (cdr (assoc "oauth_verifier" params :test #'equal)))
         ;; check authorize flag
         (oauth:authorize-request-token *request-token*)
         ;; update access-token
         (setf *access-token* (oauth:obtain-access-token *get-access-token-endpoint* *request-token*))
         ;; (redirect *clatwify-uri*)
         ))
  (with-head (:title "cl-twit-oauth")
    (html
      (str (cond (*access-token*
                  (html (:p "Already authorized!")))
                 (t
                  (setf *request-token* (get-request-token))                 
                  (html (:a :href (puri:render-uri
                                   (oauth:make-authorization-uri
                                    *auth-request-token-endpoint*
                                    *request-token*)
                                   nil)
                            "Twitter Authorize URI")))))
      (:h4 "PARAMS:")
      (:pre (str (pprint-str params)))
      (:h4 "REQUEST-TOKEN:")
      (:pre (str (conv4pre (pprint-str *request-token*))))
      (:h4 "ACCESS-TOKEN:")
      (:pre (str (conv4pre (pprint-str *access-token*)))))))

;;; Twitter operations

(defun get-access-token ()
  (oauth:obtain-access-token *get-access-token-endpoint* *request-token*))

(defun get-request-token (&optional (callback-uri *callback-uri*))
  (oauth:obtain-request-token *get-request-token-endpoint*
			      *consumer-token*
			      :callback-uri callback-uri))

;;; for Twitter API 1.1
;; https://dev.twitter.com/docs/api/1.1

(defun search-timeline (access-token keyword &key (count 20) since-id max-id lang result-type)
  (json:decode-json-from-string
   (babel:octets-to-string
    (oauth:access-protected-resource
     "https://api.twitter.com/1.1/search/tweets.json"
     access-token
     :user-parameters
     (remove-if #'null
                (list
                 (cons "q" keyword)
                 (cons "count" (format nil "~D" (truncate count)))
                 (if result-type (cons "result_type" result-type)) ; "popular" "mixed" "recent"
                 (if since-id (cons "since_id" (format nil "~D" (truncate since-id))))
                 (if max-id (cons "max_id" (format nil "~D" (truncate max-id))))
                 (if lang (cons "lang" lang)) ; ja, en, etc.. 
                 ))))))

;; Newer first. To load next timeline, call this with tail tweet id of previous timeline as max-id.
(defun home-timeline (access-token &key (count 200) since-id max-id)
  (json:decode-json-from-string
   (babel:octets-to-string
    (oauth:access-protected-resource
     "https://api.twitter.com/1.1/statuses/home_timeline.json"
     access-token
     :user-parameters
     (remove-if #'null
                (list
                 (cons "count" (format nil "~D" (truncate count)))
                 (if since-id (cons "since_id" (format nil "~D" (truncate since-id))))
                 (if max-id (cons "max_id" (format nil "~D" (truncate max-id))))))))))

;; TODO: Add condition
;; home-timeline returns the following value when encountering an error.
;; ((:ERRORS ((:MESSAGE . "Rate limit exceeded") (:CODE . 88))))

(defun user-timeline (access-token user-id/screen-name &key (count 200) since-id max-id)
  (json:decode-json-from-string
   (babel:octets-to-string
    (oauth:access-protected-resource
     "https://api.twitter.com/1.1/statuses/user_timeline.json"
     access-token
     :user-parameters
     (remove-if #'null
                (list
                 (etypecase user-id/screen-name
                   (integer (cons "user_id" user-id/screen-name))
                   (string (cons "screen_name" user-id/screen-name)))
                 (cons "count" (format nil "~D" (truncate count)))
                 (if since-id (cons "since_id" (format nil "~D" (truncate since-id))))
                 (if max-id (cons "max_id" (format nil "~D" (truncate max-id))))))))))

;; Error code list: https://dev.twitter.com/docs/error-codes-responses

(defun get-rate-limit-status (access-token)
  (json:decode-json-from-string
   (babel:octets-to-string
    (oauth:access-protected-resource
     "https://api.twitter.com/1.1/application/rate_limit_status.json"
     access-token))))

(defun parse-twitter-timestring (timestring)
  (let* ((three-char-month-list
	  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	 (year (subseq timestring 26 30))
	 (month (format nil "~2,'0D"
			(1+ (position (subseq timestring 4 7) three-char-month-list :test #'string=))))
	 (day (subseq timestring 8 10))
	 (hour-min-sec (subseq timestring 11 19))
	 (timezone (subseq timestring 20 25)))
    (local-time:parse-timestring
     (concatenate 'string year "-" month "-" day "T" hour-min-sec timezone))))

;;; unicode char utils

(defun basic-ej-char? (symb)
  (or (eq symb 'CL-UNICODE-NAMES::HIRAGANA)
      (eq symb 'CL-UNICODE-NAMES::KATAKANA)
      (eq symb 'CL-UNICODE-NAMES::CJKUNIFIEDIDEOGRAPHS)
      (eq symb 'CL-UNICODE-NAMES::CJKSYMBOLSANDPUNCTUATION)
      (eq symb 'CL-UNICODE-NAMES::HALFWIDTHANDFULLWIDTHFORMS) 
      (eq symb 'CL-UNICODE-NAMES::BASICLATIN)))

(defun count-basic-ej-char (str)
  (loop for char across str
        count
        (multiple-value-bind (s symb)
            (cl-unicode:code-block char)
          (declare (ignore s))
          (basic-ej-char? symb))))

(defun remove-not-basic-ej-char (str)
  (let ((result (make-string (count-basic-ej-char str)))
        (cnt 0))
    (loop for char across str do
      (multiple-value-bind (s symb)
          (cl-unicode:code-block char)
        (declare (ignore s))
        (when (basic-ej-char? symb)
          (setf (aref result cnt) char)
          (incf cnt))))
    result))

(defstruct (user (:constructor %make-user)
                 (:print-object %print-user))
  id name screen-name description entities
  followers-count friends-count listed-count
  favorites-count statuses-count
  profile-image-url)

(defun %print-user (obj stream)
  (format stream "#S(USER :ID ~A)" (user-id obj)))

(defun make-user (json-user)
  (%make-user :id (cdr (assoc :id json-user))
              :name (remove-not-basic-ej-char (cdr (assoc :name json-user)))
              :screen-name (cdr (assoc :screen--name json-user))
              :description (remove-not-basic-ej-char (cdr (assoc :description json-user)))
              :entities (cdr (assoc :entities json-user))
              :followers-count (cdr (assoc :followers--count json-user))
              :friends-count (cdr (assoc :friends--count json-user))
              :listed-count (cdr (assoc :listed--count json-user))
              :favorites-count (cdr (assoc :favorites--count json-user))
              :statuses-count (cdr (assoc :statuses--count json-user))
              :profile-image-url (cdr (assoc :profile--image--url json-user))))

;;; tweet struct
(defstruct (tweet (:constructor %make-tweet)
                  (:print-object %print-tweet))
  id user retweet timestamp text entities
  retweet-count favorite-count)

(defun %print-tweet (obj stream)
  (format stream "#S(TWEET :ID ~A)" (tweet-id obj)))

(defun make-tweet (json-tweet)
  (let* ((retweeted-status (cdr (assoc :RETWEETED--STATUS json-tweet)))
         (retweet (if retweeted-status (make-tweet retweeted-status)))
         (user (make-user (cdr (assoc :USER json-tweet)))))
    (%make-tweet
     :id (cdr (assoc :ID json-tweet))
     :user user
     :retweet retweet
     :timestamp (parse-twitter-timestring (cdr (assoc :CREATED--AT json-tweet)))
     :text (remove-not-basic-ej-char (cdr (assoc :TEXT json-tweet)))
     :entities (cdr (assoc :entities json-tweet))
     :retweet-count (cdr (assoc :retweet--count json-tweet))
     :favorite-count (cdr (assoc :favorite--count json-tweet))
     )))

;;; timeline struct
(defstruct (timeline (:constructor %make-timeline))
  tweet-list
  access-token
  search-keyword  ; If this is NIL, then is regarded home-timeline.
  user
  )

(defun get-json-tweet-list (access-token &key user search-keyword (count 10) since-id max-id)
  (cond
    (user
     (user-timeline access-token user :count count :since-id since-id :max-id max-id))
    (search-keyword
     (cdr (assoc :statuses (search-timeline access-token search-keyword
                                            :count count :since-id since-id :max-id max-id))))
    (t (home-timeline access-token :count count :since-id since-id :max-id max-id))))

(defun make-timeline (access-token &key user search-keyword (count 10))
  (let ((json-tweet-list (get-json-tweet-list access-token
                                              :user user
                                              :search-keyword search-keyword
                                              :count count)))
    (%make-timeline
     :tweet-list (mapcar #'make-tweet json-tweet-list)
     :access-token access-token
     :search-keyword search-keyword
     :user user)))

(defun timeline-text (timeline)
  (mapcar #'tweet-text (timeline-tweet-list timeline)))

(defun refresh-timeline! (timeline count)
  (let* ((access-token (timeline-access-token timeline))
         (got-list-length
           (let ((json-tweet-list
                   (get-json-tweet-list
                    access-token
                    :user (timeline-user timeline)
                    :search-keyword (timeline-search-keyword timeline)
                    :count count
                    :since-id (tweet-id (car (timeline-tweet-list timeline))))))
             (if json-tweet-list
                 (progn
                   (setf (timeline-tweet-list timeline)
                         (append (mapcar #'make-tweet json-tweet-list)
                                 (timeline-tweet-list timeline)))
                   (length json-tweet-list)) ; returns the length of the updated part
                 0))))
    (values timeline got-list-length)))

(defun more-timeline! (timeline count)
  (let* ((access-token (timeline-access-token timeline))
         (got-list-length
           (let ((json-tweet-list
                   (get-json-tweet-list
                    access-token
                    :user (timeline-user timeline)
                    :search-keyword (timeline-search-keyword timeline)
                    :count (abs count)
                    :max-id (tweet-id (car (last (timeline-tweet-list timeline)))))))
             (if json-tweet-list
                 (progn
                   (setf (timeline-tweet-list timeline)
                         (append (timeline-tweet-list timeline)
                                 (mapcar #'make-tweet (cdr json-tweet-list))))
                   (length json-tweet-list))
                 0))))
    (values timeline got-list-length)))

;;; Stream API

(defun make-sample-stream (access-token &key (lang "ja"))
  (oauth:access-protected-resource
   "https://stream.twitter.com/1.1/statuses/sample.json"
   access-token
   :user-parameters (remove-if #'null (list (if lang (cons "language" lang))))
   :drakma-args '(:want-stream t)))

(defun stream-sample (access-token &key (lang "ja"))
  (let ((stream (make-sample-stream access-token :lang lang)))
    (loop
      (let ((json-data (json:decode-json-from-source stream)))
        (print (remove-not-basic-ej-char (cdr (assoc :text json-data))))))))
