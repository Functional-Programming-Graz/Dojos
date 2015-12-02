(ns primesum.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(defn prime? [curr primes]
  (not  (some #(= 0 (mod curr %)) (take-while #(<= (* %1 %1) curr) primes))))

(defn primes-to [primes curr max]
  (if (< max curr)
    primes
    (recur
     (if
         (prime? curr primes)
       (conj primes curr)
       primes)
     (+ 2 curr)
     max)))

(def primemo? (memoize prime?))

(defn longest-prime-sum [length primes]
  (let [skippable (- (count primes) length)]
    (for [front-skip (range skippable)]
      (let [current-primes (->> primes (drop front-skip) (take length))
            current-sum (apply + current-primes)]
            (if
                (and (< current-sum 1000000) (primemo? current-sum primes))
              {:sum current-sum :count (count current-primes) :primes current-primes}
              nil)))))

(defn collect-sums [primes]
  (mapcat identity
          (for [i (->> primes count range rest reverse)]
            (longest-prime-sum i primes))))

(defn sum-prime? [numbers primes]
  (prime? (apply + numbers) primes))
