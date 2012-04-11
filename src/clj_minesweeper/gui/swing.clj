
(ns clj-minesweeper.gui.swing
  (:import (java.awt Color Font GridLayout Dimension Image)
           (java.awt.event MouseEvent MouseAdapter)
           (javax.swing JFrame Timer JPanel JOptionPane JButton JLabel Icon ImageIcon)
           )
  (:use 
    (clojure.contrib
      [swing-utils :only (do-swing do-swing*)]
      [miglayout :only (miglayout components)]))
  (:require 
    [clojure.contrib.miglayout :as mig]
    [clj-minesweeper.dispatch :as dispatch])
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
  (JOptionPane/showMessageDialog 
    nil message message JOptionPane/INFORMATION_MESSAGE))


(defn click2 [this bn]
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
        (do
          (.setText gb "")
          (.setIcon gb (scaled-icon icon image-size image-size)))
        (let [bn (:bomb-neighbors lb)]
          (.setIcon gb (:blank images))
          (if (= 0 bn)
            (.setText gb (str ""))
            (.setText gb (str bn)))))
      (if (:clicked lb) (.setEnabled gb false) (.setEnabled gb true)))))

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

(defn init-buttons [board]
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
                    (dispatch/fire :click {:y y :x x :flag-click false})
                    (dispatch/fire :click {:y y :x x :flag-click true}))))))))))

(defn init-board [gui-buttons]
  (let  [panel (JPanel.) bh (count gui-buttons) bw (count (first gui-buttons))]
    (do	
      (.setLayout panel (GridLayout. bh bw))
      (.setSize panel 900 800)
      (doseq [row gui-buttons]
        (doseq [btn row]
          (.add panel btn))))
    panel))


(defn init-bombs [bomb-cnt]
  (doto
    (JLabel. (str "Bombs left:\n", bomb-cnt))))

(defn init-timer [tme]
  (doto
    (JLabel. (str "Time left:\n", tme))))

(defn init-reset []
	(doto (JButton. "Reset")
		  (on-mouse-click event
		  		(dispatch/fire :reset))))

(defn init-container [gui-board gui-timer gui-bombs gui-reset]
  (let [frame (JFrame. "Minesweeper")
        panel (doto (JPanel.)
                (.add gui-timer)
                (.add gui-bombs)
                (.add gui-board))]
    (doto frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (-> .getContentPane
        (.add (miglayout (JPanel.)
                         gui-board [:wrap]
                         gui-timer [:align "left"]
                         gui-reset [:align "center"]
                         gui-bombs [:align "right"]
                         )))
      .pack .show)))


(defn init-gui 
	[board timer bomb-cnt]
	(let [
       gui-buttons      (init-buttons board)  
       gui-board 		(init-board gui-buttons)
       gui-timer 		(init-timer timer)
       gui-bombs 		(init-bombs bomb-cnt) 
       gui-reset 		(init-reset)]
   (dispatch/react-to #{:bomb-change}
                      (fn [_ bcnt]
                        (update-gui-bombs gui-bombs bcnt)))
   (dispatch/react-to #{:timer-change}
                      (fn [_ t]
                        (update-gui-timer gui-timer t)))
   (dispatch/react-to #{:cell-change}
                      (fn [_ sq]
                        (let [guib (get-in gui-buttons [(:y sq) (:x sq)])]
                          (update-gui-button guib sq))))
   (init-container gui-board gui-timer gui-bombs gui-reset)))


(comment
	(def *target-gui* :swing)
	(case *target-gui*
		:swing (require [minesweeper.swing :as g])
		:js    (require [minesweeper.js :as g])))


