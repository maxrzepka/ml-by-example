(ns ml-in-action.ch02)

(def data0 {[1 1.1] :A [1 1] :A [0 0] :B [0 0.1] :B})

(def distances
  {:euclidian (fn [a b]
                    (Math/pow (reduce + (map #(Math/pow (- %1 %2) 2) a b))
                              0.5))
   })

(defn classify0
  "Returns the most frequent class in k Nearest Neighbors of x based on data"
  [data k x]
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

(defn build-knn-classifier [data k]
  (partial classify0 data k))


;;
;; dataset can be found here :
;;
(defn grab-imagesets
  "load in memory data files under folder as map (data => class).
Each file representing a number is converted into to a vector"
  [folder]
  (reduce (fn [dataset data-file]
                  (conj dataset
                        [(keep {\0 0 \1 1} (slurp (str folder "/" data-file))) ;;data
                         (first (.split data-file "_")) ;;class
                    ]))
          {} (.list (clojure.java.io/file folder))))

(defn check-classifier
  "Runs classifier across data in test and print out the error-rate"
  [classifier test]
  (let [checks (map (fn [[data class]]
                      (let [res (classifier data) ]
                        {:test (= res class) :expected class :guess res}
                        ))
                    test)
        failed-count (count (filter (comp not :test) checks))
        error-rate (/ failed-count (count test))]
    (println "Among a population " (count test) " elements, "
             failed-count " failed : " (format "%.4f" (double error-rate)))))