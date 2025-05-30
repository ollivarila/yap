(ns yap.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn const [x] (fn [& _] x))

(defn tag [what]
  #(when (str/starts-with? % what)
     [what (.substring % (.length what))]))

(defn p-map
  "Apply function f to the output of parser"
  [f parser]
  (fn [input]
    (some-> input
            parser
            ((fn [[parsed rem]] (vector (f parsed) rem))))))

(defn alt
  ([] (const nil))
  ([& parsers]
   #(when-let [parser (first parsers)]
      (if-let [res (parser %)]
        res
        ((apply alt (rest parsers)) %)))))

(defn p-take-while [pred]
  (fn [input]
    (let [consumed (take-while pred input)
          parsed (apply str consumed)
          remaining (.substring input (.length parsed))]
      [parsed remaining])))

(defn p-take-while-1 [pred]
  #(let [result ((p-take-while pred) %)]
     (when (not-empty (first result))
       result)))

(defn delimited [lhs parser rhs]
  (fn [input]
    (when-let [[_ rem] (lhs input)]
      (when-let [[parsed rem] (parser rem)]
        (when-let [[_ rem] (rhs rem)]
          [parsed rem])))))

(defn run [parser input]
  (when-let [[parsed _] (parser input)]
    parsed))

(defn run-all-consuming [parser input]
  (when-let [[parsed r] (parser input)]
    (when (empty? r) parsed)))

(def ws (p-take-while #(Character/isWhitespace %)))

(defn pseq
  ([] (const nil))
  ([& parsers]
   (fn [input]
     (let [f (fn [acc parser]
               (when-let [[so-far remaining] acc]
                 ((p-map #(conj so-far %) parser) remaining)))
           init ((p-map vector (first parsers)) input)]
       (reduce f init (rest parsers))))))

(defn terminated [first second]
  #(when-let [[parsed rem] (first %)]
     (when-let [[_ rem] (second rem)]
       [parsed rem])))

(defn many-till
  ([many till]
   #(many-till many till [[] %]))
  ([many till result]
   (when-let [[parsed-items rem] result]
     (if-let [_ (till rem)]
       [parsed-items rem]

       (when-let [[parsed rem] (many rem)]
         (many-till many till [(concat parsed-items [parsed]) rem]))))))

(defn separated
  ([sep item terminator]
   #(if-let [[parsed rem] (item %)]
      (separated sep item terminator [[parsed] rem])
      (when-let [res (terminator %)] [[] (last res)])))

  ([sep item terminator acc]
   (when-let [[parsed rem] acc]
     (if-let [[_ rem] (terminator rem)]
       [parsed rem]

       (when-let [[_ rem] (sep rem)]
         (when-let [[parsed-item rem] (item rem)]
           (separated sep item terminator [(concat parsed [parsed-item]) rem])))))))

(defn many-0
  ([parser] (fn [input] (many-0 parser [[] input])))
  ([parser [parsed-items rem]]
   (if-let [[parsed rem] (parser rem)]
     (let [acc (vector (conj parsed-items parsed) rem)]
       (recur parser acc))
     [parsed-items rem])))

