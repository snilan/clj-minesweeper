
(ns clj-minesweeper.core
  (:use [clj-minesweeper.gui.swing]
        [clojure.set :only (union)])
  (:require [clj-minesweeper.dispatch :as dispatch])
  (:gen-class))


;	TODO
; 	fix clear-path to work with flags
; 	add unit tests
;   - Make the GUI pretty
;   - Port to Javascript


(defrecord Square [y x bomb flag clicked bomb-neighbors])

(def running (atom false))
(def game-ready (atom false))
(def current-level (atom :medium))

(def board (ref nil))
(def bombs-left (ref 0))

(def timer (agent 0))

;; factory function
(defn new-board [h w]
  (vec 
    (for [y (range h)] 
      (vec 
        (for [x (range w)]
          (Square. y x false false false 0))))))

(def levels {
   :easy {
      :height 8
      :width 8 
      :bomb-count 10 
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

(defn valid? [b [y x]]
  (let [bh (count b) bw (count (first b))]
    (and (< -1 y bh) (< -1 x bw))))

(defn bomb? [b y x]
  (:bomb (get-in b [y x])))

(defn clicked? [b y x]
  (:clicked (get-in b [y x])))

(defn flag? [b y x]
  (:flag (get-in b [y x])))

(defn neighbors [b y x] 
  (filter 
    (partial valid?	b)			
    (map #(map + % [y x]) 
         [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]])))

(defn bomb-neighbors [b y x]
  (:bomb-neighbors (get-in b [y x])))

(defn flag-neighbors [b y x]
  "Returns an integer representing the number of borderingi flags"
  (count 
    (filter 
      #(apply flag? b %)
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
            (let [not-ok #(or (seen %) (apply clicked? b %) (apply flag? b %))
                  n (remove not-ok (neighbors b y x)) 
                  flagn (flag-neighbors b y x)
                  bombn (bomb-neighbors b y x)]
              (if (and (zero? flagn) (zero? bombn))
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

(defn game-over []
  (println "Oops! You clicked a bomb! Game over")
  (assert (= @running true))
  (swap! running not)
  (dispatch/fire :bomb-click))

(defn update-timer [x & args]
  (let [x (if (seq args) 0 x)]
    (. Thread (sleep 1000))
    (if @running
      (do
        (send-off *agent* #'update-timer)
        (inc x))
      x)))

(defn new-game [level]
  (let [height (get-in levels [level :height])
        width (get-in levels [level :width])
        bombs (get-in levels [level :bomb-count])]
    (dosync
      (ref-set bombs-left bombs)
      (ref-set board (new-board height width))
      (reset! running false)
      (reset! game-ready true)
      (alter board place-bombs bombs))
    (print-board @board :bomb)
    (dispatch/fire :new-game 
                   {:height height
                    :width width
                    :levels {:levels [:easy :medium :hard]
                             :default @current-level}
                    :bomb-cnt bombs})))

(defn click-cb [y x flag-click]
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
        (alter board clear-path y x))))
  (if (game-won? @board)
    (do
      (assert @running)
      (swap! running not)
      (dispatch/fire :game-won))))

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

(add-watch
  running
  ::game-change
  (fn [_ _ _ now-running]
    (println "running: " now-running)
    (if now-running
      (send-off timer update-timer :restart))))

(dispatch/react-to
  #{:click}
  (fn [_ {y :y x :x flag-click :flag-click}]
    ;; starts the timer on first click
    (cond 
      (and @game-ready (not @running))
      (do
        (swap! running not)
        (swap! game-ready not)
        (click-cb y x flag-click))
      @running
      (click-cb y x flag-click)))) 

(dispatch/react-to
  #{:level-change}
  (fn [_ level]
    (assert (contains? levels level))
    (reset! current-level level)))

(dispatch/react-to 
  #{:reset-game}
  (fn [_ _]
    (new-game @current-level)))

(defn -main []
  (new-game @current-level))

