(in-package :distribute-coin)

;; STRING → BOOLEAN
(defun onion-url (url)
  "Return whether or not a given URL is for an onion site."
  (cl-ppcre:scan "\.onion" url))


;; STRING PATTERN-PAIR … PATTERN-PAIR → STRING
(defun replace-all (string &rest replace-pairs)
  "Apply several sets of regex replacements to a string,
  with each replacement in the form of a 'replacement-pair':
  '(TARGET REPLACEMENT)
  I.E., to replace 'a', you'd use
  '('a' 'replacement-of-a')"

  (mapcar (lambda (pair)
	    (setq string
		  (cl-ppcre:regex-replace-all
		    (first pair) string (second pair))))
	  replace-pairs)
  string)


;; STRING → STRING
(defun file-type (filename)
  "Return the file extension of a given filename."
  (setq type-string (cl-ppcre:regex-replace-all ".*\\." filename "\."))
  (if (or (zerop (length type-string))
	  (not (cl-ppcre:scan "\\." filename)))
    nil
    type-string))


(defun choose-new-file-name (original new-a new-b)
  (setq new-name
	(cond ((or (and (zerop (length new-a)) (zerop (length new-b)))
		    (and (not new-a) (not new-b)))
		original)
	       ((zerop (length new-a))
		new-b)
	       ('T new-a)))
  (if (equal new-name "nil") (setq new-name original))
  (if (not (file-type new-name))
    (string+ new-name (file-type original))
    new-name))


;; STRING-A STRING-B … STRING-N → STRING
(defun string+ (&rest strings)
    "Combine an arbitrary amount of strings into a single string."
      (reduce (lambda (a b) (format nil "~A~A" a b)) strings))
