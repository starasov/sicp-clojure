(ns com.intelliarts.starasov.sicp.e112)

(defn pascal-triangle-value [row el]
  (if (or (< row 2) (< el 1) (> el (dec row)))
    1
    (+ (pascal-triangle-value (dec row) (dec el)) (pascal-triangle-value (dec row) el))))

(defn print-row [row]
  (do
    (println (map #(pascal-triangle-value row %1) (range (inc row))))))

(defn print-triangle [rows]
  (loop [row 0]
    (if (< row rows)
      (do
        (print-row row)
        (recur (inc row))))))

(print-triangle 10)
