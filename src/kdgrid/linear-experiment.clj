(ns kdgrid.experiment
   (:use [kdgrid.datastructure :only (tanimoto-on-sets sparse-fingerprints-from-file)]))
  
(defn run-experiment [file-name lookups]
  (time
    (let [fingerprints (map set (sparse-fingerprints-from-file file-name))]
      (doall 
        (for [i (range lookups)] 
          (tanimoto-on-sets (first fingerprints) (second fingerprints)))))))
  
(if (= 2 (count *command-line-args*))
  (let [file-name (first *command-line-args*)
        lookups (Integer/parseInt (second *command-line-args*))]
    (run-experiment file-name lookups))
  (println "wrong number of arguments"))