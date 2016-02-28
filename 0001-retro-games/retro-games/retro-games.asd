;;;; retro-games.asd

(asdf:defsystem #:retro-games
  :description "Describe retro-games here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-who
               #:hunchentoot
               #:parenscript)
  :serial t
  :components ((:file "package")
               (:file "retro-games")))
