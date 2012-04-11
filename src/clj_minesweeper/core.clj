
(ns clj-minesweeper.core
  (:use [clj-minesweeper.gui.swing]
        [clojure.set :only (union)])
  (:require [clj-minesweeper.dispatch :as dispatch])
  (:gen-class))


;	TODO

;   - Implement end of game logic (check for bomb hit / all bombs flagged)
;		- either in board-change function or click-cb

; 	fix clear-path to work with flags
; 	add unit tests

;;(defn gui-bind [ident gui display]
;;	(add-watch ident
;;		(keyword (gensym))
;;		(fn [_ _ _ val]
;;			(display gui val))))

;; could pass (partial gui-bind) to gui
;; (let [gui (JLabel.)
;;		display (fn [g val]
;;					(.setText g (str val)))]
;;	(on-atom-change gui change))

;
;   - Make the GUI pretty
;   - Port to Javascript



(defrecord Square [y x bomb flag clicked bomb-neighbors])

;; factory function
(defn new-board [h w]
  (vec 
    (for [y (range h)] 
      (vec 
        (for [x (range w)]
          (Square. y x false false false 0))))))


(def levels {
   :easy {
      :height 16
      :width 16 
      :bomb-count 20 
    }
   :medium {
      :height 16
      :width 16
	  :bomb-count 40
    }
   :hard {
      :height 16
	  :width 30
      :bomb-count 99
	}
})

(defn print-board [b f]
  (doseq [row b]
    (println (map (fn [x] (if (f x) 1 0)) row))))

(defn map-board [b f]
  (vec 
    (for [row b]
      (vec 
        (for [cell row]
          (f cell))))))


(def running (atom false))
(def current-level (atom :medium))

(def board (ref nil))
(def bombs-left (ref 0))

(def timer (agent 0))

(defn valid? [b [y x]]
  (let [bh (count b) bw (count (first b))]
    (and (< -1 y bh) (< -1 x bw))))

(defn bomb? [b y x]
  (:bomb (get-in b [y x])))

(defn clicked? [b y x]
  (:clicked (get-in b [y x])))

(defn flag? [b y x]
  (:flag (get-in b [y x])))

(defn bn? [b y x]
  (:bomb-neighbors (get-in b [y x])))

(defn change-level [level] {
	:pre (contains? levels level)
  }
  (reset! current-level level)
  (reset! running false)
  (reset! board 
    (new-board 
	  (get-in levels [level :height]) 
      (get-in levels [level :width]))))
	;update GUI

(defn neighbors [b y x] 
  (filter 
    (partial valid?	b)			
    (map #(map + % [y x]) 
         [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]])))

(defn bomb-neighbors [b y x]
  "Returns an integer representing the number of bordering bombs"
  (count 
    (filter 
      (fn [[yn xn]]
        (bomb? b yn xn))
      (neighbors b y x))))


(defn flag [b y x]
  "Mark or unmark (toggle) a position as a bomb"
    (update-in b [y x :flag] not))

(defn click [b y x]
  "Mark a position as clicked. Cannot be unclicked"
  (update-in b [y x :clicked] (constantly true)))

(defn bomb [b y x]
  "Place a bomb at a certain position on the board. Cannot be removed
  Also updates neighbors"
  (let [
        nbrs 	(neighbors b y x)
		brd		(update-in b [y x :bomb] 
                  (constantly true))]
    (reduce
      (fn [bx [i j]]
        (update-in bx [i j :bomb-neighbors] inc))
      brd nbrs)))


(defn place-bombs [b n]
  (let [bh (count b) bw (count (first b))]
    (loop [bnew b num n]
      (let [y (rand-int bh) x (rand-int bw)]
        (if (= 0 num) bnew
          (if (bomb? bnew y x) 
            (recur bnew num) 
            (recur (bomb bnew y x) (dec num))))))))

(defn clear-path [b y-start x-start]
  (let [
        to-click
        (loop [[[y x] & more :as all] [[y-start x-start]] seen #{[y-start x-start]}]
          (if (seq all)
            (let [
                  not-ok #(or (seen %) (apply clicked? b %) (apply flag? b %))
                  n (remove not-ok (neighbors b y x)) bn (bomb-neighbors b y x)]
              (if (= 0 bn)
                (recur (concat more n) (union seen (set n)))
                (recur more seen)))
            seen))]
    (println "y = " y-start "x = " x-start "to-click = " to-click)
    (reduce #(apply click % %2) b to-click)))
		
(defn game-won? [b]
  "Success if every bomb is flagged and every square with flag has a bomb"
  (let [squares (flatten b)]
    (= 
      (set (filter :bomb squares)) 
      (set (filter :flag squares)))))

(defn fail? []
  (some #(and (:bomb %) (:clicked %)) (flatten @board)))

(defn game-over []
  (println "Oops! You clicked a bomb! Game over"))

(defn update-timer [x]
  (do
    (. Thread (sleep 1000))
    (send-off *agent* #'update-timer)
    (inc x)))


(defn new-game [level]
  (dosync
    (let [bh (get-in levels [level :height])
          bw (get-in levels [level :width])
          bombs (get-in levels [level :bomb-count])]
      (ref-set bombs-left bombs)
      (ref-set board (new-board bh bw))
      (alter board place-bombs bombs))))



(add-watch
  board
  ::update-board
  (fn [_ _ old-board new-board]
    (let [cells-changed
          (map second
            (filter (fn [[x y]]
                        (not= x y)) 
              (map vector
                (flatten old-board) (flatten new-board))))]
      (doseq [cell cells-changed]
        (println "cell = " cell)
        (dispatch/fire :cell-change cell)))))


(add-watch 
  timer 
  ::update-timer
  (fn [_ _ _ new-time]
    (dispatch/fire :timer-change new-time)))

(add-watch 
  bombs-left
  ::update-bomb-count
  (fn [_ _ _ new-bombs]
    (dispatch/fire :bomb-change new-bombs)))

(dispatch/react-to
  #{:click}
    (fn [_ {y :y x :x flag-click :flag-click}]
      (dosync
        (let [b @board is-flag (flag? b y x) is-bomb (bomb? b y x) is-clicked (clicked? b y x)]
          (cond 
            is-clicked nil ;; already been clicked
            flag-click ;; toggled a flag
            (do
              (alter board flag y x) ;; this will toggle the flag-marker
              (if is-flag
                (alter bombs-left inc)
                (alter bombs-left dec)))
            is-bomb 
            (if is-flag nil ; clicking on a flagged box does nothing
              (do
                (alter board click y x)
                (game-over)))
            :else 	;; regular old click
            (alter board clear-path y x))))))

(dispatch/react-to 
  #{:reset-game}
  (fn [_ _]
    (new-game @current-level)))


(defn -main []
  (new-game @current-level)
  (print-board @board :bomb)
  (init-gui @board @timer @bombs-left)
  (send-off timer update-timer))



