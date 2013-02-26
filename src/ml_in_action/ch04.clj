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

;;TODO improve tokenizer to accept words with unicode
;;TODO use stopwords
(defn tokenize [^String text]
  (keep #(when (> (.length %) 2) (.toLowerCase %))
        (clojure.string/split text #"\W+")))

(defn email-tokenize
  "tokenizes text from raw email.
The email text is everything after the first blank line."
  [^String text]
  (mapcat tokenize
          (next (drop-while #(not (.isEmpty (.trim %)))
                            (clojure.string/split-lines text)))))

(defn assoc-map [k f coll] (map (fn [m] (assoc m k (f m))) coll))
;; (c/assoc-map :feature (comp c/tokenize :raw) d)

(defn load-data
  "Load data from files and apply the tokenizer to extract features
"
  [tokenizer & class-files]
  (mapcat
   (fn [[clas dir]]
     (let [dir (clojure.java.io/file dir)
           files (if (.isFile dir) [dir]
                     (map (partial clojure.java.io/file dir) (.list dir)))]
       (map (fn [f] (let [raw (slurp (.getAbsolutePath f))]
                      {:class (keyword clas)
                       :raw raw
                       :feature (tokenizer raw)
                       }))
            files)))
   (partition 2 class-files)))

(defn sample-set
  "Splits the sample set into 2 groups train, test "
  [coll]
  (group-by (fn [x] ({0 :train 1 :test} (rand-int 2))) coll))

;;
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

(defn check-model
  "Checks model against a list of test condidates "
  [model test]
  (let [model (comp last model) ;; because model returns sorted classes
        ]
    (map (fn [m]
           (merge m (zipmap [:guess :prob] (model (:feature m)))))
         test)))

(defn stats
  "Returns total and a map of failed counts {guess => correct => count}"
  [res]
  (reduce (fn [stats {:keys [guess class]}]
            (as-> stats x
                (update-in x [:total] inc)
                (if (= guess class) x
                    (update-in x [:failed guess class] (fnil inc 0)))))
          {:total 0 :failed {}} res))


(comment
  (def b (c/bayes-inference c/posting-list))
(def lb (c/bayes-inference c/posting-list c/log-bayes-rule))
user> (b ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"])
([:normal 7.233796296296299E-15] [:abusive 1.766413792416646E-10])
user> (lb ["maybe", "not", "take", "him", "to", "dog", "park", "stupid"])
([:normal -24.2730128349438] [:abusive -18.405536949199654])
user> (c/stats (c/check-model lb s))
{:count 27, :failed {:spam {:ham 5}}}

user> (def d2 (c/load-data c/email-tokenize :ham  ".../ML_for_Hackers/03-Classification/data/easy_ham_2" :spam ".../ML_for_Hackers/03-Classification/data/spam_2"))
#'user/d2
user> (count d2)
2799
user> (def lb (c/bayes-inference d :rule c/log-bayes-rule))
#'user/lb
user> (def b (c/bayes-inference d ))
#'user/b
user> (c/stats (c/check-model lb d2))
{:total 2799, :failed {:ham {:spam 297}, :spam {:ham 8}}}
user> (c/stats (c/check-model b d2))
{:total 2799, :failed {:ham {:spam 1353}}}


)