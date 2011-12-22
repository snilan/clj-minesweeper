

(ns minesweeper.MButton
	(:gen-class		
			:prefix MButton-
			;;:import (javax.swing JButton Icon ImageIcon)
			:extends javax.swing.JButton
			:state state
			:init my-init
			:methods [[getx [] int]
			          [gety [] int]]
			:constructors {[int int] [String]))


(defn MButton-my-init 
	[x y] 
	[[""] [x y]])

(defn MButton-getx
	[this] 
	(first (.state this)))

(defn MButton-gety
	[this] 
	(second (.state this)))
	

