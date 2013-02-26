(ns ml-in-action.ch04)

(defn vmap [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

;; Data
(def posting-list
  (map (fn [[k v]] {:class v :feature k})
   {["my", "dog", "has", "flea", "problems", "help", "please"] :normal
    ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"] :abusive
    ["my", "dalmation", "is", "so", "cute", "I", "love", "him"] :normal
    ["stop", "posting", "stupid", "worthless", "garbage"] :abusive
    ["mr", "licks", "ate", "my", "steak", "how", "to", "stop","him"] :normal
    ["quit", "buying", "worthless", "dog", "food", "stupid"] :abusive}))

;;
;; training set each has a class , a list of features
;;

(defn counting
  "Given a training set (ie a collection of maps with keys :class :feature)
returns for each class a map with the following entries :
occs : occurences of each feature present for the class.
total : total of features present for the class.
prior : proportion of items for this class.
"
  [training]
  (let [doc-totals (frequencies (map :class training))
        total (count training)
        priors (vmap #(/ % total) doc-totals)]
    (reduce
     (fn [m doc]
       (let [clas (:class doc)
             prev-occ (-> m clas :occs)
             prev-total (get (m clas) :total 0)
             occs (merge-with + prev-occ (frequencies (:feature doc)))
             total (+ prev-total (-> doc :feature count))]
         (assoc m clas {:occs occs :total total :prior (get priors clas)})))
     {} training)))

(defn bayes-rule [prior total cts]
  (double
   (* prior
      (reduce (fn [s a]
                (* s (max 0.01 (/ a total))))
              1 cts))))

(defn log-bayes-rule [prior total cts]
  (+ (Math/log prior)
     (reduce (fn [s a]
                (+ s (- (Math/log (inc a)) (Math/log (+ total 2)))))
              0 cts)))

(defn bayes-inference [training & {rule :rule :or {rule bayes-rule}}]
  (let [counts (counting training)
        totals-by-class (vmap :total counts)
        priors (vmap :prior counts)
        classes (keys priors)]
    (fn infer [words]
      (let [counts-by-class
            (map (fn [w]
                   (vmap (fn [c] (get (c :occs) w 0)) counts))
                 (set words))
            ]
        (sort-by second
                 (map (fn [clas]
                        [clas
                         (rule
                          (priors clas)
                          (totals-by-class clas)
                          (map #(get % clas) counts-by-class))])
                      classes))))))

(comment
  (def b (c/bayes-inference c/posting-list))
(def lb (c/bayes-inference c/posting-list c/log-bayes-rule))
user> (b ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"])
([:normal 7.233796296296299E-15] [:abusive 1.766413792416646E-10])
user> (lb ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"])
([:normal -24.2730128349438] [:abusive -18.405536949199654])
  )