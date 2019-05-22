(in-package :distribute-coin)

;; -------------------------------------
;; handler-creation

(defmacro create-regex-dispatcher (regex page-function)
  "Just like tbnl:create-regex-dispatcher except it passes the entire
  uri split into a list by slashes.
  I.E., '/d/b/123/4' would pass arguments 'd', 'b', '123', and '4'.
  Based on:
  http://uint32t.blogspot.com/2007/12/restful-handlers-with-hunchentoot.html"
  `(push (let ((scanner (cl-ppcre:create-scanner ,regex)))
           (lambda (request)
             (multiple-value-bind (whole-match matched-registers)
               (cl-ppcre:scan-to-strings scanner (tbnl:script-name request))
               (when whole-match
                 (lambda ()
                   (apply ,page-function
                     (cdr (cl-ppcre:split "/" (tbnl:script-name request)))))))))
     hunchentoot:*dispatch-table*))


;; -------------------------------------

;; LIST-OF-PATHS (LIST) → STRING
(defun template (templates &optional (env '()))
  "Return the output of several (or one) templates as a simple string."
;;  (if env (setq env (concatenate 'list env *default-env*))
;;    (setq env *default-env*))
  (let ((output (make-string-output-stream))
	(new-env (concatenate 'list env *default-env*)))
    (mapcar (lambda (template)
              (html-template:fill-and-print-template
                template new-env
		:stream output))
      templates)
    (get-output-stream-string output)))

;; -------------------------------------

;; STRING (STRING) → STRING
(defun template-request (prefix &optional file (env '()))
  "Returns the templated-version of a given request.
  When given, for example, 'i' and 'what', it returns the template-
  processed version of 'res/html/i-what.html', merged with the
  'head.html' and 'foot.html' templates."
  (when (not file) (setq file "index"))

  (let* ((template (pathname (string+ prefix "-" file ".html")))
         (template-path (string+ *html-dir* template))
	 (new-env (concatenate
		    'list env `(:site-name ,*site-name*
					   :hostname ,*hostname*
					  ,(if *onion-hostname* :onion-hostname)
					  ,(if *onion-hostname* *onion-hostname*)
					  :ipfs-hostname ,*ipfs-hostname*))))

    (cond ((uiop:file-exists-p template-path)
           (template (list #p"head.html" template #p"foot.html") new-env))
          ('T
            (template (list #p"head.html" template #p"foot.html") new-env)))))


;; STRING STRING → NIL
(defmacro standard-file-request (prefix file)
  "Return a simple, static file to the client."
  `(hunchentoot:handle-static-file (string+ ,prefix ,file)))
