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
        start-num (square (nChoose2 i))]
    (->>
      (iterate inc i)
      (reductions (fn [prev j] (+ prev (* (inc j) iC2))) start-num))))

(defn last-2-while-less-than
  [max-total xs]
  (->>
    xs
    (tuplize 2)
    (drop-while #(< (second %) max-total))
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
                          (map-indexed #(cons (inc %1) %2)))
        column-j        (apply min-key (partial min-column-fn max-total) indexed-columns)
        trimmed-column  (take-while #(< % max-total) column-j)
        min-val         (apply min-key (partial distance-from max-total) trimmed-column)]
    [ (first column-j)
      (->>
        column-j
        (drop 1)
        (take-while #(< % min-val))
        count
        (+ (first column-j)))]))

(defn -main
  "Euler 85"
  [& args]
  (let [max-total 2000000]
    (println (calc-euler-85 max-total))))
