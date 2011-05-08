(in-package :cl-user)

(defpackage :wlamos.blog
  (:use :cl :hunchentoot :html-template :elephant)
  (:export :start-server
	   :stop-server))

(in-package :wlamos.blog)

(require :hunchentoot)
(require :html-template)
(require :elephant)

(use-package :elephant)

(defvar *username* "test")
(defvar *password* "test")
(defvar *server-log-handle* nil)

(defpclass blog-post ()
  ((title :initarg :title
	  :accessor title)
   (body :initarg :body
	 :accessor body)
   (timestamp :initarg :timestamp
	      :accessor timestamp
	      :initform (get-universal-time)
	      :index t)
   (url-part :initarg :url-part
	     :accessor url-part
	     :initform nil
	     :index t)))

(defmethod initialize-instance :after ((obj blog-post) &key)
  "If :url-part wasn't non-nil when making the instance, generate it
automatically."
  (cond ((eq nil (url-part obj))
	 (setf (url-part obj) (make-url-part (title obj))))))

(defun generate-index-page ()
  "Generate the index page showing all the blog posts."
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       #p"index.tmpl"
       (list :blog-posts
	     (loop for blog-post in (nreverse (get-instances-by-range 'blog-post 'timestamp nil nil))
		collect (list :title (title blog-post) :body (body blog-post) :url-part (url-part blog-post))))
       :stream stream))))

(defun make-url-part (title)
  "Generate a url-part from a title. A url-part should only have
alphanumeric characters or dashes (in place of spaces)."
  (string-downcase
   (delete-if #'(lambda (x) (not (or (alphanumericp x) (char= #\- x))))
	      (substitute #\- #\Space title))))

(defun generate-blog-post-page (template)
  (let ((url-part (hunchentoot:query-string*)))
    (format *server-log-handle* "~a : view post2 ~a ~%" (time-log-string) url-part)
    (with-output-to-string (stream) ;create a stream that will give us a string
      (let ((blog-post (get-instance-by-value 'blog-post 'url-part url-part)) ;Get the blog post we're intereseted in
	    (html-template:*string-modifier* #'identity))
	(html-template:fill-and-print-template
	 template
	 (list :title (title blog-post)
	       :body (body blog-post)
	       :url-part (url-part blog-post))
	 :stream stream)))))

(defun view-blog-post-page ()
  "Generate a page for viewing a blog post."
  (format *server-log-handle* "~a : view post1~%" (time-log-string))
  (generate-blog-post-page #P"post.tmpl"))

(defun edit-blog-post ()
  (format *server-log-handle* "edit-blog-post ~a ~%" (hunchentoot:request-method*))
  (with-http-authentication
    (cond ((eq (hunchentoot:request-method*) :GET)
	     (generate-blog-post-page #P"post-form.tmpl"))
	    ((eq (hunchentoot:request-method*) :POST)
	     (save-blog-post)))))

(defun delete-blog-post ()
  (with-http-authentication
      (let ((url (hunchentoot:query-string*))))))
(defun save-blog-post ()
  "Read POST data and modify blog post."
  (let ((blog-post
	 (get-instance-by-value 'blog-post
				'url-part (hunchentoot:query-string*))))
    (setf (title blog-post) (hunchentoot:post-parameter "title"))
    (setf (body blog-post) (hunchentoot:post-parameter "body"))
    (setf (url-part blog-post) (make-url-part (title blog-post)))
    (hunchentoot:redirect (url-part blog-post))))

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *username*) (string= password *password*))
	    ,@body)
	   (t (hunchentoot:require-authorization "my-blog")))))

(defun create-blog-post ()
  (with-http-authentication
      (cond ((eq (hunchentoot:request-method*) :GET)
	     (with-output-to-string (stream)
	       (html-template:fill-and-print-template #P"post-form.tmpl" nil
						      :stream stream)))
	    ((eq (hunchentoot:request-method*) :POST)
	     (save-new-blog-post)))))

(defun save-new-blog-post ()
  (let ((blog-post (make-instance 'blog-post
				  :title (hunchentoot:post-parameter "title")
				  :body (hunchentoot:post-parameter "body"))))
    (hunchentoot:redirect (url-part blog-post))))

(defun update-blog-post ()
  "Read POST data and modify blog post."
  (let ((blog-post 
         (get-instance-by-value 'blog-post
                                'url-part (hunchentoot:query-string*))))
    (setf (title blog-post) (hunchentoot:post-parameter "title"))
    (setf (body blog-post) (hunchentoot:post-parameter "body"))
    (setf (url-part blog-post) (make-url-part (title blog-post)))
    (hunchentoot:redirect (url-part blog-post))))

;; Set the web server dispatch table
(setq hunchentoot:*dispatch-table*
      `(,(hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
	 ,(hunchentoot:create-regex-dispatcher "^/view/$" 'view-blog-post-page)
	 ,(hunchentoot:create-regex-dispatcher "^/edit/$" 'edit-blog-post)
	 ,(hunchentoot:create-regex-dispatcher "^/create/$" 'create-blog-post)))

;; Make sure html-template looks for files in the right directory
(setq html-template:*default-template-pathname* #P"/home/velen/work/github/common-lisp/")

;; Open the store where our data is stored
(defvar *elephant-store* (open-store '(:clsql (:sqlite3 "/home/velen/work/github/common-lisp/blog.db"))))

; Container for all our blog posts
(defvar *blog-posts* (or (get-from-root "blog-posts")
                         (let ((blog-posts (make-pset)))
                           (add-to-root "blog-posts" blog-posts)
                           blog-posts)))

(defvar *blog-server* nil)

(defvar *server-log-file* "/home/velen/work/github/common-lisp/server.log")
(defconstant +day-names+
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defun time-log-string ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d, ~a, ~d/~2,'0d/~d (GMT~@d)"
	    hour
	    minute
	    second
	    (nth day-of-week +day-names+)
	    month
	    date
	    year
	    (- tz))))

(defun start-server ()
  (setq *server-log-handle* (open *server-log-file* :direction :output :if-exists :append))
  (format *server-log-handle* "~a : server started!~%" (time-log-string))
  (setq *blog-server* (make-instance 'hunchentoot:acceptor :port 8080))
  (hunchentoot:start *blog-server*))

(defun stop-server ()
  (when *server-log-handle*
    (format *server-log-handle* "~a : Stop server~%" (time-log-string))
    (close *server-log-handle*)
    (setq *server-log-handle* nil))
  (when *blog-server*
    (hunchentoot:stop *blog-server*)))
