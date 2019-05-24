(defsystem "distribute-coin"
	   :version "0.1"
	   :author "Jaidyn Ann <jadedctrl@teknik.io>"
           :license "Cooperative Software License"
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
