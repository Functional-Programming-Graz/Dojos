(ns primesum.core
  (:gen-class))

(defn prime?
  "A predicate yielding whether curr is a prime using a sequence of
  primes that must extend to at least the ceiling of the square root
  of curr."
  [curr primes]
  (not (some #(= 0 (mod curr %)) (take-while #(<= (* %1 %1) curr) primes))))

(defn prime-sum
  "Computes a sum of subvec of the given primes vector. Yields a map
  of the resulting sum and the used input primes if the sum is prime
  and less than under, or nil if either condition is not fulfilled."
  [[primes [start len _] under]]
  (let [inputs (subvec primes start (+ start len))
        sum (apply + inputs)]
    (if
        (and (< sum under) (prime? sum primes))
      {:sum sum :inputs inputs}
      nil)))

(defn str-result
  "Represent a result readably, by presenting a short sentence of the
  result length, the first and last prime in the result and the prime
  sum it results in."
  [result]
  (str "Sequence length is " (count (:inputs result))
       " ([" (first (:inputs result)) ".." (last (:inputs result)) "])"
       " resulting in prime " (:sum result)))

(defn primes-extend
  "Given a sorted sequence of primes, a value, and a maximum value,
  add the primes from curr to max the sequence of primes. Note that
  the algorithm assumes that all primes less than curr are already
  present in primes."
   [primes curr max]
  (if (< max curr) primes
      (recur (if (prime? curr primes) (conj primes curr) primes) (+ 2 curr) max)))

(defn primes-fn
  "Yields a function that, when called with a to paramater, yields a
  vector of primes extending from 2 to at least the largest prime <=
  to. Multiple invocations reuse the same vector, so that computation
  to larger primes will somewhat amortise the earlier computation of
  smaller primes earlier. The computed function will always return the
  full vector of primes computed so far so that more primes may be
  served than have been ordered by the invocation."
  []
  (let [primes-cache (atom [2 3])]
    (fn [to]
      (if
          (>= (last @primes-cache) to)
        @primes-cache
        (swap! primes-cache primes-extend (+ 2 (last @primes-cache)) to)))))

(defn next-sum-spec
  "Given a 3-sequence describing the previous summation specification,
  compute the next one, consisting of: How many elements in the
  overall sequence of summands to skip, how long the summed
  subsequence after that shoule be, and the total length of the
  summand sequence."
  [[front current length]]
  (let [at-end (= (+ front current) length)]
    (if  (and (= 1 current) at-end)
      nil
      (conj (if at-end [0 (dec current)] [(inc front) current]) length))))

(defn sum-specs
  "Generate a lazy sequence of summation specifications."
  [length]
  (iterate next-sum-spec [0 length length]))

(def !nil? (complement nil?))

(defn longest-prime-sum
  "Finds the longest sequence of primes whose sum is another prime less than under."
  [under]
  (let [primes-to (primes-fn)
        primes (primes-to under)
        number-primes-under (count primes)
        specs (sum-specs number-primes-under)]
    (->>
     (map vector (repeat primes) specs (repeat under))
     (map prime-sum)
     (filter !nil?)
     first)))

(defn -main
  "Calculate longest consecutive sequence of primes that, if summed
  up, yield a prime less than or equal to first positional argument."
  [& argv]
  (->> argv first Long/parseLong longest-prime-sum str-result print))
