(ns ml-in-action.ch02)

(def data0 {[1 1.1] :A [1 1] :A [0 0] :B [0 0.1] :B})

(def distances
  {:euclidian (fn [[a b] [c d]]
                    (Math/pow (+ (Math/pow (- a b) 2)
                                 (Math/pow (- c d) 2))
                              0.5))
   })

(defn classify0
  "Returns the most frequent class in k Nearest Neighbors of x based on data"
  [data x k]
  (let [euclidian (:euclidian distances)
        knn (take k
                  (sort-by :dist
                           (map (fn [[pos class]]
                                  {:dist (euclidian x pos) :class class})
                                data)))
        ]
    (->> knn
         (map :class)
         frequencies ;; count the occurences of each class
         (sort-by second (comp - compare)) ;; sort by frequencies in desc order
         first
         first)))
