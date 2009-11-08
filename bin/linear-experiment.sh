java \
  -Xmx1G \
  -cp .:src:lib/clojure.jar:lib/clojure-contrib.jar:.:classes \
  clojure.main \
  src/kdgrid/linear-experiment.clj \
  $1 $2