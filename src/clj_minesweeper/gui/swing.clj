
(ns clj-minesweeper.gui.swing
	(:import (java.awt Color Font GridLayout Dimension Image)
           (java.awt.event MouseEvent MouseAdapter)
           (javax.swing JFrame Timer JPanel JOptionPane JButton JLabel Icon ImageIcon)
           )
       (:use 
	       [clojure.contrib.swing-utils :only (do-swing do-swing*)])
	   (:gen-class))

(def button-size 40) ; 20 by 20 pixels per button

(def image-size (int (* 0.95 button-size)))

(def colors {
		1 Color/black
		2 Color/red
		3 Color/green
		4 Color/orange
		5 Color/gray
})


(defn scaled-icon [icon width height]
	(ImageIcon.
		(.getScaledInstance (.getImage icon) width height Image/SCALE_DEFAULT)))
	

(def images {
		:flag (ImageIcon. "images/minesweeper_flag.png")
		:bomb (ImageIcon. "images/bomb.png")
		:bomb-hit (ImageIcon. "")
		:blank (ImageIcon. "")
})

;;(def font [bn]
;;	(Font. "temp" Font/PLAIN (int (+ 15 (* 1.5 bn)))))

(defn alert [message]
  (JOptionPane/showMessageDialog nil message message 
	  JOptionPane/INFORMATION_MESSAGE))


(defn click2
	[this bn]
	(do
		(if (not= 0 bn)
			(do 
				(.setForeground this (colors bn))
				(.setText this (str bn))))))
		;;(.setEnabled this false)))


(defn update-gui-button [gb lb]
	(let [icon 
			(cond
				(and (:clicked lb) (:bomb lb)) (:bomb images) ; bomb image
				(:clicked lb) nil						; no image (text)
				(:flag lb) (:flag images) ; flag image
				:else (:blank images))]
		(do-swing
			(if icon
				(.setIcon gb (scaled-icon icon image-size image-size))
				(let [bn (:bomb-neighbors lb)]
					(if (= 0 bn)
						(.setText gb (str ""))
						(.setText gb (str bn)))))
			(if (:clicked lb) (.setEnabled gb false)))))

(defn update-gui-board [gui-board logb]
	(let [bh (count logb) bw (count (first logb))]
		(doseq [y (range bh) x (range bw)]
			(update-gui-button (get-in gui-board [y x]) (get-in logb [y x])))))

(defn update-gui-bombs [gui-cnt bombs] 
	(do-swing
		(.setText gui-cnt (str "Bombs: " bombs))))

(defn update-gui-timer [gui-timer t] 
	(do-swing
		(.setText gui-timer (str "Time: " t))))

(defmacro on-mouse-click [component event & body]
    `(. ~component addMouseListener 
	    (proxy [MouseAdapter] []
		    (mouseClicked [~event] ~@body))))

(defn init-gui-buttons [board click-cb]
	"Returns a 2D vector of buttons"
	(let [bh (count board) bw (count (first board))
		  left-click? #(= (.getButton %) MouseEvent/BUTTON1)]
		(vec
			(for [y (range bh)]
		  	(vec 
			  	(for [x (range bw)] 
		 	  		(doto (JButton. "")
			 	  		(.setPreferredSize (Dimension. 50 50))
			 	  		(on-mouse-click event 
			 	  			  (if (left-click? event)
								  (click-cb y x false)
								  (click-cb y x true))))))))))			


(defn init-gui-board [board click-cb board-change]
	(let  [panel (JPanel.) bh (count board) bw (count (first board))
		   btns (init-gui-buttons board click-cb)]
	(do	
		(board-change btns) ;; set the watcher to update btns
		(.setLayout panel (GridLayout. bh bw))
		(.setSize panel 900 800)
		(doseq [row btns]
			(doseq [b row]
				(.add panel b))))
		panel))


(defn init-gui-bombs [bomb-cnt bomb-change]
	(doto
		(JLabel. (str "Bombs left:\n", bomb-cnt))
		bomb-change)) ;; set the watcher to update the JLabel

(defn init-gui-timer [tme timer-change]
	(doto
		(JLabel. (str "Time left:\n", tme))
		timer-change)) ;; set the watcher to update the JLabel

(defn init-gui-container [gui-board gui-timer gui-bombs]
	(let [frame (JFrame. "Minesweeper")
		  panel (doto (JPanel.)
				(.add gui-timer)
				(.add gui-bombs)
				(.add gui-board))]
	(doto frame
		(.setContentPane panel)
		.pack .show)))


;; lots of higher order closures being passed around
(defn init-gui 
	[board timer bomb-cnt click-cb board-change timer-change bomb-change]
	(let [
		gui-board 		(init-gui-board board click-cb board-change)
		gui-timer 		(init-gui-timer timer timer-change)
		gui-bombs 		(init-gui-bombs bomb-cnt bomb-change)]
	 (init-gui-container gui-board gui-timer gui-bombs)))


(comment
	(def *target-gui* :swing)
	(case *target-gui*
		:swing (require [minesweeper.swing :as g])
		:js    (require [minesweeper.js :as g])))


