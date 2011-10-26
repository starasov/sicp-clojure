(ns clojure.com.intelliarts.starasov.sicp.generic.common)

(def -registry (ref {}))

(defn attach-tag [tag contents]
  (cons tag contents))

(defn type-tag [data]
  (cond
    (number? data) :scheme-number
    (and (seq? data) (keyword? (first data))) (first data)
    :else (throw (IllegalStateException. (str "[type-tag] Data - " data " is not tagged.")))))

(defn contents [data]
  (cond
    (number? data) data
    (and (seq? data) (keyword? (first data))) (rest data)
    :else (throw (IllegalStateException. (str "[contents] Data - " data " is not tagged.")))))

(defn register [op type item]
  (let [entry (@-registry op)]
    (if (nil? entry)
      (dosync
        (alter -registry assoc op (ref {}))
        (register op type item))
      (dosync (alter entry assoc type item)))))

(defn lookup [op type]
  (let [entry (@-registry op)]
    (if (not (nil? entry))
      (let [op-fn (entry type)]
        (if (not (nil? op-fn))
          op-fn
          (throw (IllegalStateException. (str "Unsupported operation - " op " for " (vec type))))))
      (throw (IllegalStateException. (str "Unsupported operation - " op))))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args) proc (lookup op type-tags)]
    (if (not (nil? proc))
      (apply proc (map contents args))
      (throw (IllegalStateException. (str "Unsupported operation: " op "-" args))))))