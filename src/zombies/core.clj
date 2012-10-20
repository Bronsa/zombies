(ns zombies.core
  (:import jline.Terminal)
  (:gen-class))

(def max-x 79)
(def max-y 22)

(def screen (atom (vec (repeat max-y
                               (vec (repeat max-x '(0)))))))

(defrecord Player [x y hp gold])
(def player (atom (map->Player {:hp 20 :gold 0})))

(defrecord Zombie [x y])
(def zombies (atom []))

(def level (atom 0))
(def messages (atom []))

(defn read-char []
  (char (.readCharacter (Terminal/getTerminal) System/in)))

(defn die [msg]
  (printf "\n\n%s Score: %s\n\n" msg 
          (+ (* (:gold @player) @level) (* 10 (dec @level))))
  (flush)
  (System/exit 1))

(defn rand-x [] (rand-int max-x))
(defn rand-y [] (rand-int max-y))

(defn randly-update-screen-with [c]
  (swap! screen update-in [(rand-y) (rand-x)] conj c))

(defn new-level []
  (dotimes [i (* 2 max-x)]
    (println))
  (swap! level inc)
  (dotimes [i max-x]
    (dotimes [j max-y]
      (swap! screen assoc-in [j i] '(\.))))
  (dotimes [_ (rand-int 100)]
    (randly-update-screen-with \$))
  (dotimes [_ (rand-int 10)]
    (randly-update-screen-with \^))
  (dotimes [_ (rand-int 80)]
    (randly-update-screen-with \#))
  (reset! zombies [])
  (dotimes [_ (+ 8 (* 2 @level))]
    (let [x (rand-x)
          y (rand-y)]
      (swap! zombies conj (->Zombie x y))
      (swap! screen update-in [y x] conj \Z)))
  (randly-update-screen-with \>)
  (let [x (rand-x)
        y (rand-y)]
    (swap! player assoc :x x :y y)
    (swap! screen update-in [y x] conj \@))
  (swap! messages conj (str "Welcome to Level " @level ".")))

(defn print-screen []
  (dotimes [i 2]
    (print \return)
    (dotimes [_ (inc max-x)]
      (print " "))
    (when (zero? i)
      (print (str (char 27) "[23A"))))
  (print \return)
  (print  "Level:" @level "HP:" (:hp @player) "Gold:" (:gold @player))
  (dotimes [j max-y]
    (println)
    (dotimes [i max-x]
      (print (first ((@screen j) i)))))
  (println)
  (doseq [i @messages]
    (print i " " ))
  (flush))

(defn move-zombie [z x y]
  (let [zombie (@zombies z)]
    (swap! screen update-in [(:y zombie) (:x zombie)] rest)
    (swap! zombies update-in [z] assoc :x x :y y)
    (swap! screen update-in [y x] conj \Z)))

(defn move-player [x y]
  (swap! screen update-in [(:y @player) (:x @player)] rest)
  (swap! player assoc :x x :y y)
  (swap! screen update-in [y x] conj \@))

(declare process-zombie-smart
         process-zombie-rand)

(defn process-zombies []
  (dotimes [z (count @zombies)]
    (if (or (zero? (rand-int 7))
            (not (process-zombie-smart z)))
      (process-zombie-rand z))))

(defn process-zombie-smart [z]
  (let [z-x (:x (@zombies z))
        z-y (:y (@zombies z))
        p-x (:x @player)
        p-y (:y @player)
        new-x ((if (> z-x p-x) dec
                   (if (< z-x p-x) inc identity)) z-x)
        new-y ((if (> z-y p-y) dec
                   (if (< z-y p-y) inc identity)) z-y)]
    (case (first ((@screen new-y) new-x))

      \@
      (do (swap! player update-in [:hp] dec)
          (swap! messages conj "Ouch!")
          true)

      (\Z \#)
      nil
      
      \^
      (loop [new-x (rand-int max-x)
             new-y (rand-int max-y)]
        (if (= (first ((@screen new-y) new-x)) \.)
          (do (move-zombie z new-x new-y)
              true)
          (recur (rand-int max-x)
                 (rand-int max-y))))
      ;; else
      (do (move-zombie z new-x new-y)
          true))))

(defn process-zombie-rand [z]
  (let
      [z-x (:x (@zombies z))
       z-y (:y (@zombies z))
       rand-x (rand-int 3)
       rand-y (rand-int 3)
       new-x ((if (zero? rand-x) inc
                  (if (= 1 rand-x) dec identity)) z-x)
       new-y ((if (zero? rand-y) inc
                  (if (= 1 rand-y) dec identity)) z-y)]
    (when-not (or (neg? new-x) (>= new-x max-x)
                  (neg? new-y) (>= new-y max-y))
      (case (first ((@screen new-y) new-x))
        
        (\Z \#)
        nil
        
        \^
        (loop [new-x (rand-int max-x)
               new-y (rand-int max-y)]
          (if (= (first ((@screen new-y) new-x)) \.)
            (move-zombie z new-x new-y)
            (recur (rand-int max-x)
                   (rand-int max-y))))
        ;; else
        (move-zombie z new-x new-y)))))


(defn match [new-x new-y]
  (case (first ((@screen new-y) new-x))
    \>
    (do (new-level)
        true)
    \$
    (loop [gold 0]
      (if (= (first ((@screen new-y) new-x)) \$)
        (do (swap! player update-in [:gold] inc)
            (swap! screen update-in [new-y new-x] rest)
            (recur (inc gold)))
        (do
          (swap! messages conj (str "Found " gold " gold."))
          (match new-x new-y))))
    
    (\Z \#)
    nil
    
    \^
    (do
      (swap! screen update-in [new-y new-x] rest)
      (loop [new-x (rand-int max-x)
             new-y (rand-int max-y)]
        (if (= (first ((@screen new-y) new-x)) \.)
          (move-player new-x new-y)
          (recur (rand-int max-x)
                 (rand-int max-y)))))
    ;; else
    (move-player new-x new-y)))

(declare print-help)

(defn process-player []
  (let [input (read-char)
        action-map '{\h [-1 0] \j [0 1] \k [0 -1] \l [1 0] \y [-1 -1]
                     \u [1 -1] \b [-1 1] \n [1 1] \. [0 0]}]
    (if (= input \q)
      (die "Goodbye.")
      (if (= input \?)
        (print-help)
        (when-let [[x y] (action-map input)]
          (let [new-x (+ x (:x @player))
                new-y (+ y (:y @player))]
            (when-not (or (neg? new-x) (>= new-x max-x)
                          (neg? new-y) (>= new-y max-y))
              (match new-x new-y))))))))

(defn recover-hp []
  (if (and (zero? (rand-int 10))
           (< (:hp @player) 20))
    (swap! player update-in [:hp] inc)))

(defn print-help []
  (dotimes [i (+ 3 max-x)]
    (println))
  (println "Welcome to Zombies!\n")
  (print "The world has been overrun by zombies.  ")
  (println "You have no weapons or other means of")
  (print "self-defense.  All you can do is run for ")
  (println "the exit!  That, and collect gold.\n")
  (println "Objects:\n@ - You\nZ - Zombies!\n$ - Gold")
  (println "\n# - Trees\n^ - Teleporters")
  (println "> - Level Exit\n\nControls:\ny k u\nh @ l")
  (println "b j n\n\n. - Wait\nq - Quit\n\n")
  (println "Press any key to continue.\n")
  (read-char)
  (print-screen))

(defn -main
  [& args]
  (dotimes [i (inc max-y)]
    (println))
  (new-level)
  (reset! messages ["Zombies!  Press ? for help."])
  (print-screen)
  (loop []
    (reset! messages [])
    (when (process-player)
      (recover-hp)
      (process-zombies)
      (if (< (:hp @player) 1)
            (die "You've been killed by zombies."))
      (print-screen))
    (recur)))
