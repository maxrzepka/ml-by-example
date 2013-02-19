(ns ml-in-action.ch03)


;; # Decision tree based on shannon entropy
;;
;; The choosen datastructure is a plain table where by convention :
;;    - last column is the answers of our question.
;;    - the other columns (features) are used to guess the answer.
;;
;;  The algorithm used to build the decision tree is the ID3
;;  http://en.wikipedia.org/wiki/ID3_algorithm
;;
(def d [[:feat1 :feat2 :class] [1 1 :yes] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])
(def d1 [[:feat1 :feat2 :class] [1 1 :maybe] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])

;; 2 Features : No surfacing , flippers?
(def fish [[:no-surfacing :flippers :fish?]
           [:yes :yes :yes]
            [:yes :yes :yes]
            [:yes :no  :no]
            [:no  :yes :no]
            [:no  :yes :no]])

(defn lenses [filepath]
  (vec
   (list* [:age :prescript :astigmatic :tear-rate :lense]
          (mapv #(vec (seq (.split % "\t")))
                   (clojure.string/split-lines (slurp filepath))))))


(defn shannon
  "Computes the shannon entropy of a dataset.
The values should be nominal or discrete.
 "
  [info]
  (let [len (count info)]
    (reduce (fn [s [_ t]]
              (let [p (/ t len)]
                (- s (* p (/ (Math/log p) (Math/log 2))))))
            0 (frequencies info))))

(defn wshannon
  "Computes the weigthed shannon entropy of multiple datasets"
  [datasets]
  (let [total (reduce (fn [s d] (+ s (count d))) 0 datasets)]
    (assert (> total 0) (str "wshannon empty datasets : " datasets))
    (reduce (fn [s d]
              (let [p (/ (count d) total)]
                (+ s (* p (shannon d)))))
            0 datasets)))

(defn extract-class
  "By convention data contains a header and class is the last column"
  [data]
  (mapv last (next data)))

(defn extract-by
  "Extracts the records of data where the feature equals val
and remove this feature.
"
  [data position val]
  (vec (keep (fn [record]
               (when (= (get record position) val) ;;retains
                 ;; exclude pos-th column
                 (vec (keep-indexed
                       (fn [i e]
                         (when (not (= position i)) e))
                       record))))
             data)))

(defn split-by
  "Returns the map : value of feature => data filtered and feature column excluded"
  [data feature]
  ;;(println "split-by " data " " feature)
  (let [header (first data)
        position ((zipmap header (range)) feature)
        split-header (filterv #(not= feature %) header)]
    (into {}
          (map #(vector %
                        (list* split-header
                               (extract-by (rest data) position %)))
               (set (map #(nth % position) (rest data)))))))


(defn choose-best-feature
  "Find the best decision given data.
Returns a map with the following keys : feature data shannon "
  [data]
  (first
   (sort-by :shannon
            (map (fn [feat]
                   (let [datasets (split-by data feat)
                         shan (wshannon (map extract-class
                                             (vals datasets)))]
                     {:feature feat :data datasets :shannon shan}))
                 (-> data first butlast)))))

(defn decide
  "Build decision tree"
  ([header data]
     (decide (vec (list* header data))))
  ([data]
     (cond (= 1 (-> data first count)) ;; no more feature
           (distinct (extract-class data)) ;;remaining undecidable values
           (= 1 (-> data extract-class distinct count)) ;;decided
           (-> data next first last)
           :else ;;go deeper
           (let [t (choose-best-feature data)
                 data (into {} (map (fn subdec [[k v]] (vector k (decide v))) (:data t)))]
             (assoc t :data data)))))