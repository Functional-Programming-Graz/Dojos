(defproject primesum "0.1.0-SNAPSHOT"
  :description "Computes the longest consecutive sequence of primes < 1000000 that, when summed up, yields another prime."
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot primesum.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
