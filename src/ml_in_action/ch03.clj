(ns ml-in-action.ch03)



;; # Decision tree based on shannon entropy
;;
;; The choosen datastructure is a plain table where by convention :
;;    - last column is the answers of our question.
;;    - the other columns (features) are used to guess the answer.
;;
;;  The algorithm to build the decision tree is the ID3
;;
;;TODO make the datastructure more explicit
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
                (+ (* p (shannon d)))))
            0 datasets)))

(defn extract-by
  "Extracts the records of data where the feature equals val
and remove this feature.
Warning : return vector of vectors because feature is an index
"
  [data feature val]
  (vec (keep (fn [line]
               (when (= (get line feature) val) ;;retains
                 ;; exclude pos-th column
                 (vec (keep-indexed
                       (fn [i e]
                         (when (not (= feature i)) e))
                       line))))
             data)))

(defn split-by
  "Returns the map : value of feature => data filtered and feature excluded"
  [data feature]
  ;;(println "split-by " data " " feature)
  (into {}
        (map #(vector % (extract-by data feature %))
             (set (map #(nth % feature) data)))))

(defn split-shannon
  "Computes the shannon entropy after splitting the dataset by feature pos :
The formula is the pondered sum of subsets entropy.
"
  [data pos]
  (let [len (count data)
        ;;the distinct values of the feature
        values (set (map #(nth % pos) data))]
    (reduce
     (fn [s v]
       (let [sub-set (extract-by data pos v)]
         (+ s (* (/ (count sub-set) len) ;;proportion of v
                 (shannon (map last sub-set))))))
     0 values)))


(defn choose-best-feature
  "Find the best decision given data.
Returns a map with the following keys : feature data shannon "
  [data]
  (first
   (sort-by :shannon
            (map (fn [feat]
                   (let [datasets (split-by data feat)
                         shan (wshannon (vals datasets))]

                     {:feature feat :data datasets :shannon shan}))
                 (-> data first count dec range)))))

;;TODO correct feature label
(defn decide
  "Build decision tree"
  [data]
  (cond (= 1 (-> data first count)) ;; no more feature
    (distinct (map first data)) ;;remaining undecidable values
    (= 1 (->> data (map last) distinct count)) ;;decided
    (-> data first last)
    :else ;;go deeper
    (let [t (choose-best-feature data)
          data (into {} (map (fn subdec [[k v]] (vector k (decide v))) (:data t)))]
      (assoc t :data data))))

(defn choose-best-feature1
  "Returns the feature having the lowest entropy after split."
  [data]
  (let [total-features (-> data first count dec)]
    (first ;; feature
     (first ;; lowest entropy
      ;; order by entropy
      (sort-by second <
               ;; compute the entropy for each feature
               (map #(vector % (split-shannon data %))
                    (range total-features)))))))