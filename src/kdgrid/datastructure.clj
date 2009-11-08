(ns kdgrid-datastructure
  (:use clojure.set
        clojure.contrib.test-is 
        [clojure.contrib.duck-streams :only (read-lines)]))

(defn chunk-size [k max-int] 
  (inc (quot max-int k)))
      
(deftest test-chunk-size
  (is (= 42 (chunk-size 1 41)))
  (is (= 2 (chunk-size 2 3)))
  (is (= 3 (chunk-size 3 8)))
  (is (= 4 (chunk-size 3 9)))
  (is (= 4 (chunk-size 3 10)))
  (is (= 4 (chunk-size 3 11))))

(defn chunk-bin [k max-int bit-number]
  (quot bit-number (chunk-size k max-int)))
  
(deftest test-chunk-bin
  (is (= 0 (chunk-bin 2 3 0)))
  (is (= 0 (chunk-bin 2 3 1)))
  (is (= 1 (chunk-bin 2 3 2)))
  (is (= 1 (chunk-bin 2 3 3)))
  (is (= 0 (chunk-bin 3 9 0)))
  (is (= 0 (chunk-bin 3 9 1)))
  (is (= 0 (chunk-bin 3 9 2)))
  (is (= 0 (chunk-bin 3 9 3)))
  (is (= 1 (chunk-bin 3 9 4)))
  (is (= 1 (chunk-bin 3 9 5)))
  (is (= 1 (chunk-bin 3 9 6)))
  (is (= 1 (chunk-bin 3 9 7)))
  (is (= 2 (chunk-bin 3 9 8)))
  (is (= 2 (chunk-bin 3 9 9))))

(defn create-chunk-summary [k max-int fingerprint]
  (let [chunk-indexes (map (partial chunk-bin k max-int) fingerprint)]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} chunk-indexes)))

(deftest test-create-chunk-summary
  (is (= {0 2, 2 1} (create-chunk-summary 3 9 [0, 8, 1]))))

;; When using this function, create a new one using partial k max-int and use that
;; so we don't have to remember k or max-int more in the rest of the program
(defn create-chunk-vector [k max-int fingerprint]
  (let [summary (create-chunk-summary k max-int fingerprint)]
    (for [chunk (range k)] (get summary chunk 0))))
    
(deftest test-create-chunk-vector
  (is (= [2 0 1] (create-chunk-vector 3 9 (set [0, 8, 1]))))
  (is (= [4 4 2] (create-chunk-vector 3 9 (range 10)))))
  
;; This smells wrong! Would be better if this didn't call create-chunk-vector!
(defn create-kD-grid [k fingerprints]
  (let [max-int (reduce max (map #(reduce max %) fingerprints))]
    (reduce 
      (fn [grid fingerprint] 
        ; might want to pull fingerprint out of structure
        (let [chunk-vector (create-chunk-vector k max-int fingerprint)]  
          (assoc grid chunk-vector 
            (conj (get grid chunk-vector []) fingerprint))))
    {} fingerprints)))
    
(deftest test-create-kD-grid
  (is (= 
    {'(4 4 2) [(range 10)], '(1 0 1) [[0 9] [1 8]]}
     (create-kD-grid 3 [[0, 9], (range 10), [1, 8]]))))
     
; (defn s-max-grid [a-chunk-counts b-chunk-counts]
;   (/ 
;     (reduce + (map min a-chunk-counts b-chunk-counts)) 
;     (reduce + (map max a-chunk-counts b-chunk-counts))))
;   
; (deftest test-s-max-grid
;   (is (= 1 (s-max-grid [1 2] [1 2])))
;   (is (= 0 (s-max-grid [0 0] [1 2])))
;   (is (= (/ 1 3) (s-max-grid [1 0] [1 2]))))

(defn add-leaf-to-tree [tree [chunk-vector leaf]]
    (cond 
      (= chunk-vector []) 
        leaf
      :else 
        (assoc tree (first chunk-vector)
          (add-leaf-to-tree 
            (get tree (first chunk-vector) {}) [(rest chunk-vector) leaf]))))    

(deftest test-add-leaf-to-tree 
  (is (= {1 {2 :leaf}} (add-leaf-to-tree {} [[1, 2] :leaf])))
  (is (= {1 {2 :leaf1 3 :leaf2}} 
    (add-leaf-to-tree (add-leaf-to-tree {} [[1, 2] :leaf1]) [[1, 3] :leaf2]))))
    
(defn build-grid-tree [grid-map]
  (reduce add-leaf-to-tree {} grid-map))
  
(deftest test-build-grid-tree
  (is (= 
    {1 {2 {3 :leaf1 4 :leaf2}} 2 {1 {1 :leaf3}}} 
    (build-grid-tree {[1,2,3] :leaf1 [1,2,4] :leaf2 [2,1,1] :leaf3}))))

(def empty-results [])
(defn add-fingerprint-to-results [results fingerprint]
  (conj results fingerprint))

;; Memorize this?
(defn sum [l] (reduce + l))

(defn inner-max-tanimoto [min-sum max-sum query-chunks]
  (/ (+ min-sum (sum query-chunks)) (+ max-sum (sum query-chunks))))

(defn inner-search-grid-tree 
  [tree min-tanimoto query-chunks min-sum max-sum]
  (cond 
    (= query-chunks [])
      tree
    :else
      (if (>= (inner-max-tanimoto min-sum max-sum query-chunks) min-tanimoto)
        (mapcat
            (fn [[chunk child]] 
                (inner-search-grid-tree 
                  child min-tanimoto (rest query-chunks) 
                  (+ min-sum (min (first query-chunks) chunk)) 
                  (+ max-sum (max (first query-chunks) chunk)) ))
            tree))))

(defn search-grid-tree [tree min-tanimoto query-chunks]
  (inner-search-grid-tree tree min-tanimoto query-chunks 0 0))
  
(defn sparse-fingerprint-from-string [s]
  (map #(Integer/parseInt %) (.split s ",")))

(deftest test-sparse-fingerprint-from-string
  (is (= [10,42,101] (sparse-fingerprint-from-string "10,42,101"))))

(defn sparse-fingerprints-from-file [file-name]
  (map sparse-fingerprint-from-string (read-lines file-name)))

(def example-grid-tree 
  (build-grid-tree 
    {[1,2,3] [:leaf1] 
    [1,2,4] [:leaf2 :leaf4] 
    [2,1,1] [:leaf3]}))

(println (search-grid-tree example-grid-tree 0.9 [1,2,3]))

(defn tanimoto-on-sets [a b] 
  (/ 
    (count (intersection a b))
    (count (union a b))))

(println (tanimoto-on-sets #{1 2} #{1 3}))

(defn perform-query [tree query min-tanimoto query-chunks]
  (filter 
    #(>= (tanimoto-on-sets query %) min-tanimoto)
    (search-grid-tree tree min-tanimoto query-chunks)))

(def chemdb-file-name 
  "/Users/tgk/Documents/workspace/Tanimoto/data/chemdb/CHEMDB_SAMPLE_UNCOMP_10K.csv")

(defn kD-grid [fingerprints k]
  (let [max-int (reduce max (map #(reduce max %) fingerprints))
        chunk-vector (partial create-chunk-vector k max-int)
        grid-set (create-kD-grid k fingerprints)
        grid-tree (build-grid-tree grid-set)]
    (fn [query min-tanimoto] 
      (perform-query grid-tree query min-tanimoto (chunk-vector query)))))
  
(def fingerprints (map set (sparse-fingerprints-from-file chemdb-file-name)))

(time (def lookup (kD-grid fingerprints 1)))

(time (doall (map #(lookup % 0.9) fingerprints)))

(run-tests)