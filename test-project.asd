;;;; test-project.asd

(asdf:defsystem #:test-project
  :description "This is a test project to see if asdf is used correctly"
  :author "A Jivani <ajivani@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "test-project")))

