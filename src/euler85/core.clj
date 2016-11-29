(ns euler85.core
  (:gen-class))

(defn nChoose2
  [n]
  (quot (* n (inc n)) 2))

(defn pow
  [b n]
  (nth (iterate (partial * b) 1) n))

(defn square
  [b]
  (pow b 2))

(defn tuplize
  [n xs]
  (->>
    (range n)
    (map #(drop %  xs))
    (apply map vector)))

(defn analyze-square-size
  [max-total]
  (->>
    (range)
    (reductions #(-> (* %2 %2 %2) (+ %1)))
    (take-while #(< % max-total))
    (count)))

(defn calc-columns
  [i]
  (let [start-num (square (nChoose2 i))]
    (->>
      (range)
      (drop start-num)
      (reductions (fn [prev j] (+ (nChoose2 j) prev))))))

(defn last-2-while-less-than
  [max-total xs]
  (->>
    (tuplize 2 xs)
    (drop-while #(< (first %) max-total))
    first))

(defn distance-from
  [a b]
  (Math/abs (- a b)))

(defn min-column-fn
  [max-total xs]
  (->>
    xs
    (partial last-2-while-less-than max-total)
    (map (partial distance-from max-total))
    (apply min)))

(defn calc-euler-85
  [max-total]
  (let [max-square-size  (analyze-square-size max-total)
        indexed-columns (->>
                          (range max-square-size)
                          (map calc-columns)
                          (map-indexed #(cons %1 %2)))
        column-j (min-key #(min-column-fn max-total %) indexed-columns)]
    [(first column-j) (count column-j)]))

(defn -main
  "Euler 85"
  [& args]
  (let [max-total 5]
    (println (calc-euler-85 max-total))))
