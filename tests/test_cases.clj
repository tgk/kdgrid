(ns tests.test-cases
  (:use (kdgrid datastructure)
        clojure.set
        clojure.contrib.test-is 
        [clojure.contrib.duck-streams :only (read-lines)]))

(deftest test-chunk-size
  (is (= 42 (chunk-size 1 41)))
  (is (= 2 (chunk-size 2 3)))
  (is (= 3 (chunk-size 3 8)))
  (is (= 4 (chunk-size 3 9)))
  (is (= 4 (chunk-size 3 10)))
  (is (= 4 (chunk-size 3 11))))

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

(deftest test-create-chunk-summary
  (is (= {0 2, 2 1} (create-chunk-summary 3 9 [0, 8, 1]))))

(deftest test-create-chunk-vector
  (is (= [2 0 1] (create-chunk-vector 3 9 (set [0, 8, 1]))))
  (is (= [4 4 2] (create-chunk-vector 3 9 (range 10)))))
  
(deftest test-create-kD-grid
  (is (= 
    {'(4 4 2) [(range 10)], '(1 0 1) [[0 9] [1 8]]}
     (create-kD-grid 3 [[0, 9], (range 10), [1, 8]]))))

; (deftest test-add-leaf-to-tree 
;   (is (= {1 {2 :leaf}} (add-leaf-to-tree {} [[1, 2] :leaf])))
;   (is (= {1 {2 :leaf1 3 :leaf2}} 
;     (add-leaf-to-tree (add-leaf-to-tree {} [[1, 2] :leaf1]) [[1, 3] :leaf2]))))
    
(deftest test-build-grid-tree
  (is (= 
    {1 {2 {3 :leaf1 4 :leaf2}} 2 {1 {1 :leaf3}}} 
    (build-grid-tree {[1,2,3] :leaf1 [1,2,4] :leaf2 [2,1,1] :leaf3}))))

(deftest test-sparse-fingerprint-from-string
  (is (= [10,42,101] (sparse-fingerprint-from-string "10,42,101"))))

(deftest test-search-grid-tree 
  (def example-grid-tree 
    (build-grid-tree 
      {[1,2,3] [:leaf1] 
      [1,2,4] [:leaf2 :leaf4] 
      [2,1,1] [:leaf3]}))
      
  (is (= 
    #{:leaf1 :leaf2 :leaf4} 
    (set (search-grid-tree example-grid-tree 0.9 [1,2,3])))))