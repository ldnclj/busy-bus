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
  world)

(defn update-world [world location]
  (-> world
      (drop-and-pickup location)
      (create-new-passengers)))

(defn strategy [world]
  (mod (inc (:location (:bus world)))
       (count (:stops world))))

(defn main [world strategy num-steps]
  (loop [world world
         num-steps num-steps]
    (if (> num-steps 0)
      (let [new-location (strategy world)]
        (recur (update-world world new-location) (dec num-steps)))
      world)))
