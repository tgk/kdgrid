java \
  -Xmx1G \
  -cp src:lib/clojure.jar:lib/clojure-contrib.jar:.:classes \
  clojure.lang.Script \
  tests/test_cases.clj