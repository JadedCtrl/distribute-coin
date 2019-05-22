(in-package :distribute-coin)

;; -------------------------------------
;; globals

(defparameter *img-dir* #p"res/img/")
(defparameter *html-dir* #p"res/html/")
(defparameter *css-dir* #p"res/css/")
(defparameter *upload-dir* #p"p/")
(defparameter *alias-dir* #p"u/")
(defparameter *default-env* '(:depth "/"))
(defparameter *hostname* nil)
(defparameter *protocol* nil)
(defparameter *ipfs-hostname* nil)
(defparameter *onion-hostname* nil)
(defparameter *site-name* "distribute-coin ☭")


(defun root-request (&rest ignored)
  (template-request "p" "index"))

;; -------------------------------------
;; file-upload/download etc

;; STRING {STRING} → STRING
(defun p-request (p &optional file)
  "Handle a /p/* request appropriately."
  (if file
    (file-request p file)
    (template-request p)))


;; STRING STRING → STRING/VARYING
(defun file-request (p GET-filename)
  "Handle a /p/ file-request-- I.E., either return a pre-existing uploaded
  file, or upload and create a new file. Or return an error. W/e."
  (let* ((upload-pair    (hunchentoot:post-parameter "file"))
	 (pair-filename  (name-sanitize (ignore-errors (second upload-pair))))
	 (POST-filename  (name-sanitize
			   (hunchentoot:post-parameter "filename")))
	 (filename       (choose-new-file-name pair-filename POST-filename
					       (name-sanitize GET-filename)))
	 (hash           nil)
	 (env           `(:filename ,filename)))

    (cond 
      ((and upload-pair (alias-exists-p filename))
           (setf (hunchentoot:return-code*) hunchentoot:+http-conflict+)
           (template-request "p" "already-exists" env))
      (upload-pair
	(setq hash (file-upload (car upload-pair) filename))
	(setq new-env (concatenate 'list `(:hash ,hash) env))
	(template-request "p" "success" new-env))
      ((alias-exists-p filename)
	(alias-execute filename))
      ('T
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       (template-request "p" "no-data")))))


(defun file-upload (temp-location file-name)
  (let ((hash (ipfs-gno:add temp-location)))
    (alias-create file-name (ipfs-uri hash))
    hash))

(defun ipfs-uri (hash)
  (string+ *ipfs-hostname* "/ipfs/" hash))

(defun ipns-uri (hash)
  (string+ *ipfs-hostname* "/ipns/" hash))

;; -------------------------------------
;; url alias/shortening

;; STRING {STRING} → STRING
(defun u-request (u &optional alias)
  "Handle a /u/* request appropriately."
  (if alias
    (alias-request u alias)
    (template-request u)))

;; STRING STRING → STRING
(defun alias-request (u GET-alias-name)
  "Handle a /u/ alias-request. I.E., either redirect to another URL-- as per
  the alias, or create a new redirect/alias. Or error… tiel la mondo iras."
  (let* ((POST-alias-name (name-sanitize
			    (hunchentoot:post-parameter "alias-name")))
	 (alias-name      (if (zerop (length POST-alias-name))
			    (name-sanitize GET-alias-name) POST-alias-name))
	 (POST-target     (uri-sanitize (hunchentoot:post-parameter "target")))
	 (env             `(:alias-name ,alias-name :target ,POST-target)))

    (cond
      ((and alias-name POST-target (alias-exists-p alias-name))
       (setf (hunchentoot:return-code*) hunchentoot:+http-conflict+)
       (template-request "u" "already-exists" env))
      ((and alias-name POST-target)
       (alias-create alias-name POST-target)
       (template-request "u" "success" env))
      ((alias-exists-p alias-name)
       (alias-execute alias-name))
      ('T
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       (template-request "u" "no-data")))))


;; STRING → NIL 
(defun alias-execute (alias)
  "Actually 'execute' a given alias; I.E., redirect the client to the URL
  written in the pathname respective to the given alias-name."
  (hunchentoot:redirect (uiop:read-file-line (alias-pathname alias))))

;; STRING STRING → NIL 
(defun alias-create (alias target)
  "Create an alias to a given URL."
  (with-open-file (alias-file (alias-pathname alias) :direction :output
			      :if-does-not-exist :create)
    (format alias-file target)))

;; STRING → BOOLEAN
(defun alias-exists-p (alias)
  "Return whether or not an alias of the given name exists."
  (uiop:file-exists-p (alias-pathname alias)))

;; STRING → PATHNAME
(defun alias-pathname (alias)
  "Return the pathname an alias of the given name would use."
  (pathname (string+ *alias-dir* "/" alias)))


;; -------------------------------------

;; STRING STRING → VARYING
(defun res-css-request (r css)
  "Serve a CSS resource file."
  (standard-file-request *css-dir* css))

;; STRING STRING → VARYING
(defun res-img-request (r image)
  "Serve a resource image."
  (standard-file-request *img-dir* image))


;; -------------------------------------


;; :NUMBER :PATHNAME :PATHNAME :PATHNAME :PATHNAME → ACCEPTOR
(defun server (&key (hostname nil)
		    (onion-hostname nil)
		    (ipfs-hostname nil)
		    (ipfs-api nil)
		    (port 80) (upload-dir #p"p/") (alias-dir #p"u/")
		    (html-dir #p"res/html/") (css-dir #p"res/css/")
		    (img-dir #p"res/img/") (site-name "distribute-coin ☭"))
  "Start the `insert-coin` server. :upload-dir, :html-dir,
  and css-dir must contain relative paths to their
  respective directories. The :upload-dir must be
  writable to the server; the rest must only be
  readable."
  (setq html-template:*default-template-pathname* html-dir)
  (setf *html-dir* html-dir)
  (setf *img-dir* img-dir)
  (setf *css-dir* css-dir)

  (setf *site-name* site-name)

  (setf *hostname* (or hostname (string+ "http://127.0.0.1:" port)))
  (setf *onion-hostname* onion-hostname)
  (setf *ipfs-hostname* (or ipfs-hostname "http://127.0.0.1:8080"))
  (setf ipfs-gno:*api-host* (or ipfs-api "http://127.0.0.1:5001"))

  (setf *alias-dir* alias-dir)
  (setf *upload-dir* upload-dir)
  (setf *alias-dir* alias-dir)
  
  (create-regex-dispatcher "/i/.*" #'template-request)
  (create-regex-dispatcher "/r/.*\.png" #'res-img-request)
  (create-regex-dispatcher "/r/.*\.css" #'res-css-request)
  (create-regex-dispatcher "/p/.*" #'p-request)
  (create-regex-dispatcher "/u/.*" #'u-request)
  (create-regex-dispatcher "^/$" #'root-request)

  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
