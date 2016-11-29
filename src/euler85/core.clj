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
    (map #(drop % xs))
    (apply map vector)))

(defn analyze-square-size
  [max-total]
  (->>
    (range)
    (reductions #(-> (* %2 %2 %2) (+ %1)))
    (take-while #(< % max-total))
    (count)))

(defn calc-column
  [i]
  (let [iC2 (nChoose2 i)
        start-num (square (nChoose2 (dec i)))]
    (->>
      (iterate inc start-num)
      (reductions (fn [prev j] (+ iC2 prev))))))

(defn last-2-while-less-than
  [max-total xs]
  (->>
    xs
    (tuplize 2)
    (drop-while #(< (first %) max-total))
    first))

(defn distance-from
  [a b]
  (Math/abs (- a b)))

(defn min-column-fn
  [max-total xs]
  (->>
    xs
    (last-2-while-less-than max-total)
    (map (partial distance-from max-total))
    (apply min)))

(defn calc-euler-85
  [max-total]
  (let [max-square-size (analyze-square-size max-total)
        indexed-columns (->>
                          (range 1 (inc max-square-size))
                          (map calc-column)
                          (map-indexed cons))
        column-j        (apply min-key (partial min-column-fn max-total) indexed-columns)]
    [(first column-j) (count column-j)]))

(defn -main
  "Euler 85"
  [& args]
  (let [max-total 5]
    (println (calc-euler-85 max-total))))
