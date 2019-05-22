(in-package :distribute-coin)

(defun valid-name-p (string)
  (not (cl-ppcre:scan "^null$" string)))

(defun name-sanitize (string)
  (if (valid-name-p string)
    (replace-all string
		 '("\.html" "\.html\.txt")
		 '("\\!" "_")  '("\\?" "_")
		 '("\\" "_") '("/" "_")
		 '("\\~" "_")  '("\\*" "_")
		 '("\\$" "_")  '("\&" "_")
		 '("\\=" "_")  '("\\+" "_")
		 '("{" "_")  '("}" "_")
		 '("\\[" "_")  '("\\]" "_")
		 '("\\(" "_")  '("\\)" "_")
		 '("<" "_")  '(">" "_")
		 '(" " "_"))))

(defun valid-uri-p (string)
  (not (zerop (length string))))

(defun uri-sanitize (string)
  (if (valid-uri-p string)
    (if (not (cl-ppcre:scan "://" string))
	 (string+ "https://" string)
	 string)))
