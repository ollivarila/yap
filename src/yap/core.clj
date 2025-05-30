(ns yap.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn const [what]
  (fn [& _don't-care]
    what))

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

(defn delimited [lhs parser rhs]
  (fn [input]
    (when-let [[_ rem] (lhs input)]
      (when-let [[parsed rem] (parser rem)]
        (when-let [[_ rem] (rhs rem)]
          [parsed rem])))))

(defn run [parser input]
  (when-let [[parsed _] (parser input)]
    parsed))

(def ws (p-take-while #(Character/isWhitespace %)))

(defn pseq [& parsers]
  (fn [input]
    (let [f (fn [acc parser]
              (when-let [[so-far remaining] acc]
                ((p-map #(conj so-far %) parser) remaining)))
          init ((p-map vector (first parsers)) input)]
      (reduce f init (rest parsers)))))

(defn terminated [first second]
  #(when-let [[parsed rem] (first %)]
     (when-let [[_ rem] (second rem)]
       [parsed rem])))

(defn many-till
  ([many till]
   #(many-till many till [[] %]))
  ([many till result]
   (when-let [[parsed-items rem] result]
     (if-let [[parsed rem] (till rem)]
       [(concat parsed-items [parsed]), rem]

       (when-let [[parsed rem] (many rem)]
         (many-till many till [(concat parsed-items [parsed]) rem]))))))

(defn separated
  ([sep item terminator]
   #(when-let [[parsed rem] (item %)]
      (separated sep item terminator [[parsed] rem])))
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

(def json-null
  (p-map (const :null) (tag "null")))

(def json-true
  (p-map (const true) (tag "true")))

(def json-false
  (p-map (const false) (tag "false")))

(def numeric? #(Character/isDigit %))

(def json-int (p-take-while numeric?))
(def json-double
  (p-take-while
   #(or (numeric? %) (= \. %))))

(def json-number (p-map parse-double (alt json-int json-double)))

(def quote (tag "\""))
; TODO: handle escaped quotes
(def json-string (delimited quote (p-take-while #(not= % \")) quote))

(declare json-value) ; for recursive parsing

(def comma (pseq ws (tag ",") ws))
(def open-b (pseq (tag "[") ws))
(def close-b (pseq ws (tag "]")))

(def json-array (p-map last (pseq open-b (separated comma json-value close-b))))

(def open-curly (pseq (tag "{") ws))
(def close-curly (pseq (tag "}") ws))
(def empty-object (p-map (const {}) (pseq open-curly ws close-curly)))

(def colon (tag ":"))
(def json-key (p-map keyword json-string))

(def key-value-pair (p-map #(vector (first %) (last %)) (pseq json-key ws colon ws json-value)))
(def keys-separated-by-comma (p-map #(nth % 1) (pseq ws key-value-pair comma)))
(def non-empty-object (p-map #(into {} (conj (nth % 1) (nth % 2))) (pseq open-curly (many-0 keys-separated-by-comma) key-value-pair ws close-curly)))

(def json-object (alt empty-object non-empty-object))

(def json-value (alt json-null json-true json-false json-number json-string json-array json-object))

(def json (delimited ws json-value ws))

(run json "{ \"foo\": 123, \"bar\": 3210 }")
