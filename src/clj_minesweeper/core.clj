(comment
   TODO:
   1. Fix clear-path to work for flags

   2. Wrap the relevant Swing parts in (do-swing*) since
      Swing is not thread safe

   3. Change atomic reference to board to
      a 2D vector of atomic references to squares

   4. Make the GUI pretty
  
   ∞. Port to Javascript
)


(ns clj-minesweeper.core
	(:use [clj-minesweeper.gui.swing]
		  [clojure.set])
	(:gen-class))

(defrecord Square [y x bomb flag clicked bomb-neighbors])

;; factory function
(defn new-board [h w]
   (vec (for [y (range h)] 
   	    (vec (for [x (range w)]
   	    	(Square. y x false false false 0))))))


(def levels {
	:easy {
		:height 9
		:width 9
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
 	(vec (for [row b]
 		(vec (for [cell row]
 			(f cell))))))


(def in-progress (atom false))

(def board (atom nil)) ;; new-game to initialize

(def bombs-left (atom 0))

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
	(let [sq (get-in b [y x])]
		(assoc-in b [y x] 
			(assoc sq :flag (not (:flag sq))))))

(defn click [b y x]
	"Mark a position as clicked. Cannot be unclicked"
	(assoc-in b [y x] 
		(assoc (get-in b [y x]) :clicked true)))

(defn bomb [b y x]
	"Place a bomb at a certain position on the board. Cannot be removed
	Also updates neighbors"
	(let [
		nbrs 	(neighbors b y x)
		brd		(assoc-in b [y x] 
					(assoc (get-in b [y x]) :bomb true))]
		(reduce 
			(fn [bx [i j]]
				(let [sq (get-in bx [i j]) bn (:bomb-neighbors sq)]
					(assoc-in bx [i j]
						(assoc sq :bomb-neighbors (inc bn)))))
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
	(let [to-click
		(loop [[[y x] & more :as all] [[y-start x-start]] seen #{}]
			(if (seq all)
				(let [n (remove seen (neighbors b y x)) bn (bomb-neighbors b y x)]
					(if (= 0 bn)
						(recur (concat more n) (clojure.set/union seen (set n)))
				 		(recur more (conj seen [y x]))))
					seen))]
		(reduce #(apply click % %2) b to-click)))
		
(defn success? []
	"Success if every bomb is flagged and every square with flag has a bomb"
	(let [squares (flatten @board)]
		(= (set (filter :bomb squares)) (set (filter :flag squares)))))

(defn fail? []
	(some #(and (:bomb %) (:clicked %)) (flatten @board)))

(defn game-over []
	nil)


(defn click-cb [y x flag-click]
	(dosync
		;;(println "x = " x "y = " y)
		(let [
			b @board is-flag (flag? b y x) is-bomb (bomb? b y x) is-clicked (clicked? b y x)]
			(cond 
				is-clicked nil ;; already been clicked
				flag-click ;; toggled a flag
					(do
						(swap! board flag y x) ;; this will toggle the flag-marker
						(if is-flag
							(swap! bombs-left inc)
							(swap! bombs-left dec)))
				is-bomb ;; just clicked a bomb
					(do
						(swap! board click y x)
						(game-over))
				:else 	;; regular old click
					(swap! board clear-path y x)))))

(defn board-change [gui-board]
	(add-watch 
		board 
		::update-board
		(fn [_ _ _ new-board]
			(update-gui-board gui-board new-board))))

(defn timer-change [gui-timer]
	(add-watch 
		timer 
		::update-timer
		(fn [_ _ _ new-time]
			(update-gui-timer gui-timer new-time))))

(defn bombs-change [gui-bomb-cnt]
	(add-watch 
		bombs-left
		::update-bomb-count
		(fn [_ _ _ new-bombs]
			(update-gui-bombs gui-bomb-cnt new-bombs))))

(defn new-game [level]
	(dosync
		(let [bh (get-in levels [level :height])
			  bw (get-in levels [level :width])
			  bombs (get-in levels [level :bomb-count])]
				(reset! bombs-left bombs)
				(reset! board (new-board bh bw))
				(swap! board place-bombs bombs))))

(defn update-timer [x]
	(do
		(println x)
		(. Thread (sleep 1000))
		(send-off *agent* #'update-timer)
		(inc x)))

(defn -main []
	(do
		(new-game :hard)
		(print-board @board :bomb)
		(init-gui @board @timer @bombs-left click-cb 
			board-change timer-change bombs-change)))
		;;(send-off timer update-timer)))



