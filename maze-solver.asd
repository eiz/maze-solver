(asdf:defsystem #:maze-solver
  :serial t
  :description "Solves 2D mazes using A*"
  :author "Mackenzie Straight"
  :license "GPLv3+"
  :components ((:file "package")
               (:file "map")
               (:file "prioqueue")
               (:file "astar")
               (:file "solver")))
