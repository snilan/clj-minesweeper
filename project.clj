(defproject clj-minesweeper "1.0.0-SNAPSHOT"
  :description "An attempt at a functional minesweeper"
  :dependencies [[org.clojure/clojure "1.3.0"]
  				[org.clojure/clojure-contrib "1.2.0"]
  		]
  :aot [clj-minesweeper.gui.swing]
  :main clj-minesweeper.core
)
