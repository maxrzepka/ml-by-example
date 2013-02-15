(ns ml-in-action.ch03)


(def d [[1 1 :yes] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])
(def d1 [[1 1 :maybe] [1 1 :yes] [1 0 :no] [0 1 :no] [0 1 :no]])

(defn shannon [data]
  (let [len (count data)]
    (reduce (fn [s [_ t]]
              (let [p (/ t len)]
                (- s (* p (/ (Math/log p) (Math/log 2))))))
            0 (frequencies (map last data)))))

(defn extract-by [data pos val]
  (keep (fn [line]
          (when (= (get line pos) val) ;;filter line
            ;; exclude pos-th column
            (keep-indexed
             (fn [i e]
               (when (not (= pos i)) e))
             line)))
        data))

(defn split-shannon [data pos]
  (let [len (count data)
        values (set (map #(nth % pos) data))]
    (reduce
     (fn [s v]
       (let [sub-set (extract-by data pos v)]
         (+ s (* (/ (count sub-set) len)
                 (shannon sub-set)))))
     0 values)))


(defn choose-feature [data]
  (let [len (count data)
        nb-features (-> data first count dec)]
    (first
     (first
      (sort-by second <
               (map #(vector % (split-shannon data %))
                    (range nb-features)))))))