java \
  -Xmx1G \
  -cp .:src:lib/clojure.jar:lib/clojure-contrib.jar:.:classes \
  clojure.main \
  src/kdgrid/experiment.clj \
  $1 $2 $3