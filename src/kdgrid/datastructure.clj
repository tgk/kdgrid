(ns kdgrid.datastructure
  (:use clojure.set
        clojure.contrib.test-is 
        [clojure.contrib.duck-streams :only (read-lines)]))

(defn chunk-size [k max-int] 
  (inc (quot max-int k)))
      
(defn chunk-bin [k max-int bit-number]
  (quot bit-number (chunk-size k max-int)))
  
(defn create-chunk-summary [k max-int fingerprint]
  (let [chunk-indexes (map (partial chunk-bin k max-int) fingerprint)]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} chunk-indexes)))

;; When using this function, create a new one using partial k max-int and 
;; use that so we don't have to remember k or max-int more in the rest of 
;; the program
(defn create-chunk-vector [k max-int fingerprint]
  (let [summary (create-chunk-summary k max-int fingerprint)]
    (for [chunk (range k)] (get summary chunk 0))))
    
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
     
(defn add-leaf-to-tree [tree [chunk-vector leaf]]
    (cond 
      (= chunk-vector []) 
        leaf
      :else 
        (assoc tree (first chunk-vector)
          (add-leaf-to-tree 
            (get tree (first chunk-vector) {}) [(rest chunk-vector) leaf]))))    

(defn build-grid-tree [grid-map]
  (reduce add-leaf-to-tree {} grid-map))

;; Memorize this?
(defn sum [l] (reduce + l))

;; Ugly thing to do, calculating the sum again and again and again...
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

(defn sparse-fingerprints-from-file [file-name]
  (map sparse-fingerprint-from-string (read-lines file-name)))

(defn tanimoto-on-sets [a b] 
  (/ 
    (count (intersection a b))
    (count (union a b))))

(defn perform-query [tree query min-tanimoto query-chunks]
  (filter 
    #(>= (tanimoto-on-sets query %) min-tanimoto)
    (search-grid-tree tree min-tanimoto query-chunks)))

(defn kD-grid [fingerprints k]
  (let [max-int (reduce max (map #(reduce max %) fingerprints))
        chunk-vector (partial create-chunk-vector k max-int)
        grid-set (create-kD-grid k fingerprints)
        grid-tree (build-grid-tree grid-set)]
    (fn [query min-tanimoto] 
      (perform-query grid-tree query min-tanimoto (chunk-vector query)))))
      
(defn linear-searcher [fingerprints]
  (fn [query min-tanimoto]
    (filter 
      #(>= (tanimoto-on-sets query %) min-tanimoto)
      fingerprints)))
