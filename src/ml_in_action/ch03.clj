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
(def d [[1 1 :yes] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])
(def d1 [[1 1 :maybe] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])

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

(defn extract-by
  "Extracts the records of data where the feature pos equals val
and remove this feature.
"
  [data pos val]
  (keep (fn [line]
          (when (= (get line pos) val) ;;retains
            ;; exclude pos-th column
            (keep-indexed
             (fn [i e]
               (when (not (= pos i)) e))
             line)))
        data))

(defn split-shannon
  "Computes the shannon entropy after splitting the dataset by feature pos :
The formula is the pondered sum of subsets entropy.
"
  [data pos]
  (let [len (count data)
        ;;the distinc values of the feature
        values (set (map #(nth % pos) data))]
    (reduce
     (fn [s v]
       (let [sub-set (extract-by data pos v)]
         (+ s (* (/ (count sub-set) len) ;;proportion of v
                 (shannon (map last sub-set))))))
     0 values)))


(defn choose-best-feature
  "Returns the feature having the lowest entropy after split."
  [data]
  (let [len (count data)
        total-features (-> data first count dec)]
    (first ;; feature
     (first ;; lowest entropy
      ;; order by entropy
      (sort-by second <
               ;; compute the entropy for each feature
               (map #(vector % (split-shannon data %))
                    (range total-features)))))))