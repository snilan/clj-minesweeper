
(ns clj-minesweeper.gui.swing
  (:import (java.awt Image)
           (java.awt.event MouseEvent MouseAdapter)
           (javax.swing Icon ImageIcon))
  (:use 
    (clojure.contrib
      [swing-utils :only (do-swing do-swing*)]
      [miglayout :only (miglayout components)]))
  (:use [seesaw.core])
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
      :flag (ImageIcon. "images/minesweeper_flag.png")
      :bomb (ImageIcon. "images/bomb.png")
      :bomb-hit (ImageIcon. "")
      :blank (ImageIcon. "")
})

(defmacro on-mouse-click [component event & body]
  `(. ~component addMouseListener 
      (proxy [MouseAdapter] []
        (mouseClicked [~event] ~@body))))

(defn make-buttons [height width]
  "Returns a vector of buttons"
  (let [left-click? #(= (.getButton %) MouseEvent/BUTTON1)]
    (vec
      (for [y (range height) x (range width)]
        (let [btn 
              (button :text ""
                      :preferred-size [button-size :by button-size]
                      :class :game-button
                      :id (keyword
                            (str "button" x "and" y)))]
          (on-mouse-click btn event 
                          (if (left-click? event)
                            (dispatch/fire :click {:y y :x x :flag-click false})
                            (dispatch/fire :click {:y y :x x :flag-click true})))
          btn)))))


(defn make-board [height width]
  (let [buttons (make-buttons height width)]
    (grid-panel
      :id :board
      :rows height
      :columns width
      :items buttons)))


(defn make-bomb-label [bomb-cnt]
  (label
    :id :bomb-label 
    :text (str "Bombs: ", bomb-cnt)))

(defn make-timer [tme]
  (label 
    :id :timer 
    :text (str "Time: ", tme)))

(defn make-reset []
  (let [btn (button
              :text "Reset"
              :id :reset-button)]
    (on-mouse-click btn event
          (dispatch/fire :reset-game))
    btn))


(defn make-top-panel []
  (flow-panel
    :items [(make-bomb-label 0)
            (make-timer 0)
            (make-reset)]
    :hgap 10))

(defn make-frame []
  (frame
    :title "Minesweeper"
    :size [900 :by 800]
    :on-close :exit))

(defn update-gui-button [gui-button button]
  (let [icon 
        (cond
          (and (:clicked button) (:bomb button)) (:bomb images)
          (:clicked button) nil
          (:flag button) (:flag images)
          :else (:blank images))]
    (if icon
      (config! gui-button 
               :text ""
               :icon (scaled-icon icon image-size image-size))
      (let [bn (:bomb-neighbors button)]
        (config! gui-button
                 :icon (:blank images)
                 :text (if (zero? bn)
                         (str "")
                         (str bn)))))
    (config! gui-button 
             :enabled? (not (:clicked button)))))

(defn update-gui-bombs [gui-cnt bombs] 
  (config! gui-cnt :text (str "Bombs: " bombs)))

(defn update-gui-timer [gui-timer t]
  (config! gui-timer :text (str "Time: " t)))


(defn add-behavior [pred fun]
  (dispatch/react-to
    pred
    (fn [id data]
      (invoke-later 
        (fun id data)))))

(defn add-behaviors [root]
  (add-behavior #{:new-game}
                (fn [_ {height :height width :width bombs :bomb-cnt}]
                  (config! root
                           :content (border-panel
                                      :border 5
                                      :hgap 5
                                      :vgap 5
                                      :north (make-top-panel)
                                      :center (make-board height width)
                                      :south (label :h-text-position :center :text "Ready to play!")))
                  (config! (select root [:#bomb-label])
                                   :text (str "Bombs: " bombs))
                  (config! (select root [:#timer])
                                   :text (str "Time: " 0))
                  (-> root pack! show!)))

  (add-behavior #{:bomb-change}
                (fn [_ bcnt]
                  (let [bomb-label (select root [:#bomb-label])]
                    (config! bomb-label
                             :text (str "Bombs: " bcnt)))))

  (add-behavior #{:timer-change}
                (fn [_ t]
                  (let [timer (select root [:#timer])]
                    (config! timer
                             :text (str "Time: " t)))))

  (add-behavior #{:cell-change}
                (fn [_ sq]
                  (let [id (keyword
                             (str "#button" (:x sq) "and" (:y sq)))
                        guib (select root [id])]
                    (update-gui-button guib sq))))

  (add-behavior #{:bomb-click}
                (fn [_ _]
                  (alert "You clicked a bomb! Game over.")))

  (add-behavior #{:game-won}
                (fn [_ _]
                  (alert "Congrats! You won!"))))

(def root (make-frame))

(add-behaviors root)

