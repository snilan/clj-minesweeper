
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

(defn scaled-icon [image width height]
  (icon
    (.getScaledInstance (.getImage image) width height Image/SCALE_DEFAULT)))
	
(def images {
		:flag (icon "images/minesweeper_flag.png")
		:bomb (icon "images/bomb.png")
		:bomb-hit (icon "")
		:blank (icon "")
})

(defn update-gui-button [gb lb]
  (let [icon 
        (cond
          (and (:clicked lb) (:bomb lb)) (:bomb images) ; bomb image
          (:clicked lb) nil						; no image (text)
          (:flag lb) (:flag images) ; flag image
          :else (:blank images))]
    (do-swing
      (if icon
        (config! gb 
                 :text ""
                 :icon (scaled-icon icon image-size image-size))
        (let [bn (:bomb-neighbors lb)]
          (config! gb
                   :icon (:blank images)
                   :text (if (zero? bn)
                          (str "")
                          (str bn)))))
      (config! gb :enabled? (not (:clicked lb))))))

(defn update-gui-bombs [gui-cnt bombs] 
  (do-swing
    (config! gui-cnt :text (str "Bombs: " bombs))))

(defn update-gui-timer [gui-timer t] 
  (do-swing
    (config! gui-timer :text (str "Time: " t))))

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
            (let [btn 
                  (button :text ""
                          :preferred-size [50 :by 50]
                          :class :game-button
                          :id (keyword (str "button" x "," y)))]
                  (on-mouse-click btn event 
                     (if (left-click? event)
                       (dispatch/fire :click {:y y :x x :flag-click false})
                       (dispatch/fire :click {:y y :x x :flag-click true})))
              btn)))))))

(defn init-board [gui-buttons]
  (let  [panel (JPanel.) bh (count gui-buttons) bw (count (first gui-buttons))]
    (do	
      (.setLayout panel (GridLayout. bh bw))
    panel))


(defn init-bombs [bomb-cnt]
  (label :text (str "Bombs left:\n", bomb-cnt)))

(defn init-timer [tme]
  (label :text (str "Time left:\n", tme)))

(def gui-reset
  (let [btn (button
              :text "Reset"
              :id :reset-button)]
    (on-mouse-click btn event
		  		(dispatch/fire :reset))
    btn))

(defn init-container [gui-board gui-timer gui-bombs gui-reset]
  (let [frame (JFrame. "Minesweeper")
        panel (doto (JPanel.)
                (.add gui-timer)
                (.add gui-bombs)
                (.add gui-board))]
    (frame
      :on-exit :close

        (.add (miglayout (JPanel.)
                         gui-board [:wrap]
                         gui-timer [:align "left"]
                         gui-reset [:align "center"]
                         gui-bombs [:align "right"]
                         )))
      pack! show!)))


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


