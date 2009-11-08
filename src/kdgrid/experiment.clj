(ns kdgrid.experiment
   (:use [kdgrid.datastructure :only (kD-grid sparse-fingerprints-from-file)]))
  
(defn run-experiment [file-name k min-tanimoto]
  (let [fingerprints (map set (sparse-fingerprints-from-file file-name))
        query (kD-grid fingerprints k)]
    (time (doall (map #(query % min-tanimoto) fingerprints)))))
  
(if (= 3 (count *command-line-args*))
  (let [file-name (first *command-line-args*)
        k (Integer/parseInt (second *command-line-args*))
        min-tanimoto (Double/parseDouble (nth *command-line-args* 2))]
    (run-experiment file-name k min-tanimoto))
  (println "wrong number of arguments"))