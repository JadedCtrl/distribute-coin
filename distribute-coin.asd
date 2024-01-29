(defsystem "distribute-coin"
	   :version "0.11"
	   :author "Jaidyn Ann <jadedctrl@posteo.at>"
           :license "GNU AGPLv3"
	   :depends-on ("hunchentoot" "translate" "html-template" "cl-ipfs-api2")
	   :components ((:module "src"
			   :components
			   ((:file "package")
			     (:file "util")
			     (:file "sanitize")
			     (:file "handle-requests")
			     (:file "main"))))

           :description
	   "An IPFS-backed file-uploading & URL-shortening website.")
