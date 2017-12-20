(ns conway)
(use '[clojure.string :only (join)])
(use '[clojure.pprint :only (pprint)])

(defn in? 
  "true if coll contains elm"
  [coll elm]  
    (some #(= elm %) coll))

(defprotocol CellProtocol
  (living-neighbors [self world] "A collection of living cells around self.")
  (overpopulated? [self living-count] "Whether the cell is overpopulated.")
  (underpopulated? [self living-count] "Whether the cell is underpopulated."))

(defprotocol Displayable
  (display [self] "The string representation of the object"))

(defn neighboring-points [[x y]]
  [ [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]
    [(- x 1) y      ]             [(+ x 1) y      ]
    [(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)] ])

(defrecord Cell [alive? point]
  Displayable
  (display [self] (if (:alive? self) " ◼︎" " ∙"))

  CellProtocol
  (overpopulated? [self living-count]
    (> living-count 3))

  (underpopulated? [self living-count]
    (if (:alive? self)
      (< living-count 2)
      (< living-count 3)))

  (living-neighbors [self world]
    ;world -> flatten >- :alive? => :point >- (in? (self -> :point -> neighboring-points))

    (->> world flatten (filter :alive?) (map :point)
      (filter (partial in? (->> self :point neighboring-points))))))

(defn random-cell [x y]
  (map->Cell { :alive? (not (zero? (rand-int 2)))
               :point [x y]}))

;(map vals (group-by first list-of-cells))
(->> list-of-cells (group-by first) vals)
(defn create-world [size]
  (for [y (range size) y (rangesize)]
    (if (zero? (rand-int 2)) [x y] nil)))
  ;(->> (range size) (map (fn [x] 
    ;(->> (range size) (map (partial random-cell x)))
  ;)))
;)

(defn next-cell [world cell]
  (let [living-count (-> cell (living-neighbors world) count)]
    (-> cell (assoc
      :alive? (not (or (overpopulated? cell living-count)
                       (underpopulated? cell living-count)))))))

(defn display-layer [layer]
  ;#(->> % (map display) (join ""))
  (->> layer (map display) (join "")))

(defn display-world [world]
  (->> world (map display-layer) (join "\n")))

(defn -main []
  (loop [world (create-world 20)]
    (->> world display-world (str "\n") println)
    (Thread/sleep 500) 
    (recur (->> world (map (partial map (partial next-cell world)))))))
