(ns kdgrid.experiment
   (:use [kdgrid.datastructure :only (linear-searcher sparse-fingerprints-from-file)]))
  
(defn run-experiment [file-name min-tanimoto]
  (let [fingerprints (map set (sparse-fingerprints-from-file file-name))
        query (linear-searcher fingerprints)]
    (time (doall (map #(query % min-tanimoto) fingerprints)))))
  
(if (= 2 (count *command-line-args*))
  (let [file-name (first *command-line-args*)
        min-tanimoto (Double/parseDouble (second *command-line-args*))]
    (run-experiment file-name min-tanimoto))
  (println "wrong number of arguments"))