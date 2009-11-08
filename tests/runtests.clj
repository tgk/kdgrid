(ns tests.runtests
  (:use clojure.contrib.test-is 
        (tests test-cases)
        (kdgrid datastructure)))

(run-tests 'tests.test-cases)