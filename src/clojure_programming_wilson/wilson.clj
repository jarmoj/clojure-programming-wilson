(ns clojure-programming-wilson.wilson)

(def ^:dynamic use-wilson-version true)

(defn grid
  [w h]
  (set (concat
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn step
  ([walls paths start-loc unvisited]
   (step walls paths unvisited)) 
  ([walls paths unvisited]
   (cons walls
         (lazy-seq
          (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
            (let [walk (iterate (comp rand-nth paths) loc)
                  steps (zipmap (take-while unvisited walk) (next walk))
                  walk (if use-wilson-version
                         (take-while identity (iterate steps loc)) walk)
                  steps (if use-wilson-version 
                          (zipmap walk (next walk)) steps)
                  new-walls (reduce disj walls (map set steps))
                  new-unvisited (reduce disj unvisited (keys steps))]
              (step new-walls paths new-unvisited))
            nil)))))

(defn maze 
  "Returns a random maze carved out of walls; walls is a set of
   2-item sets #{a b} where a and b are locations.
   The returned maze is a set of the remaining walls."
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                {} (map seq walls))
        start-loc (rand-nth (keys paths))
        unvisited (disj (set (keys paths)) start-loc)]
    (step walls paths start-loc unvisited)))
    
(defn draw
  [w h maze-seq]
  (let [panel (doto (proxy [java.awt.Canvas] []
                      (getPreferredSize [] (java.awt.Dimension.
                                            (* 10 (inc w)) (* 10 (inc h)))))
                (.setIgnoreRepaint true))
        frame (doto (javax.swing.JFrame. "Maze")
                (.add panel)
                (.setDefaultCloseOperation javax.swing.JFrame/EXIT_ON_CLOSE)
                .pack                
                (.setVisible true))]
    (.createBufferStrategy panel 2)
    (doseq [maze maze-seq]
      (let [graphics (.getDrawGraphics (.getBufferStrategy panel))]
        (if (not (= maze (first maze-seq))) (Thread/sleep 2000) (Thread/sleep 1000))
        (doto graphics
          (.translate 15 15)
          (.scale 10 10)
          (.setStroke (java.awt.BasicStroke. 0.4))
          (.setColor java.awt.Color/RED)
          (.fillRect -1 -1 w h)
          (.setColor java.awt.Color/BLACK)
          (.drawRect -1 -1 w h)
          )
        (doseq [[[xa ya] [xb yb]] (map sort maze)]
          (let [[xc yc] (if (= xa xb) 
                          [(dec xa) ya]
                          [xa (dec ya)])]
            (doto graphics
              (.drawLine xa ya xc yc))))
        (.dispose graphics)
        (.show (.getBufferStrategy panel))
        (.sync (java.awt.Toolkit/getDefaultToolkit))))))    
        
(defn run
  [arg-use-wilson-version]  
  (binding [use-wilson-version arg-use-wilson-version]
    (draw 50 50 (maze (grid 50 50)))))

