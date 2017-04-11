(ns busy-bus.core)

;; assumptions
;; bus is infinite
;; all passengers want to get off somewhere other than where they got on

(def world
  {:bus {:location 5
         :passengers [1 3 4 4]}
   :stops [[1 2 3] [0 5] [] [] [] []]})

(defn drop-and-pickup [world location]
  (let [new-passenger (get (:stops world) location)]
    ;; update bus location
    (-> world
        (update-in [:bus :location] (constantly location))
        ;; drop off passengers
        (update-in [:bus :passengers] #(filter (fn [l] (not (= l location))) %))
        ;; pick up all new passengers
        (update-in [:bus :passengers] #(vec (concat % new-passenger)))
        (update-in [:stops location] (constantly []))
        )))

(defn create-new-passengers [world]
  (let [num-stops (count (:stops world))
        passenger-location (rand-int num-stops)
        passenger-destination (rand-nth
                               (filter #(not (= passenger-location %))
                                        (range num-stops)))]
    (update-in world [:stops passenger-location] #(vec (conj % passenger-destination)))))

(defn update-world [world location]
  (-> world
      (drop-and-pickup location)
      (create-new-passengers)))

(defn move-right [world]
  (mod (inc (:location (:bus world)))
       (count (:stops world))))

(defn move-left [world]
  (mod (dec (:location (:bus world)))
       (count (:stops world))))

(defn move-stay [world]
  (:location (:bus world)))

(defn strategy-random [world]
  (let [rand-num (rand-int 3)]
    (cond
      (= rand-num 0) (move-right world)
      (= rand-num 1) (move-left world)
      :else (move-stay world)
      )))

(defn main [world strategy num-steps]
  (loop [world world
         num-steps num-steps]
    (if (> num-steps 0)
      (let [new-location (strategy world)]
        (recur (update-world world new-location) (dec num-steps)))
      world)))


#_(main world strategy-random 4)
