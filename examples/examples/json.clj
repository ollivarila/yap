(ns examples.json
  (:require [yap.core :refer [p-map tag const p-take-while-1 p-take-while delimited ws pseq alt many-0 run separated]]
            [examples.json :as ex]))

(declare json-value)

(def json-null
  (p-map (const :null) (tag "null")))

(def json-true
  (p-map (const true) (tag "true")))

(def json-false
  (p-map (const false) (tag "false")))

(def numeric? #(Character/isDigit %))

(def json-int (p-take-while-1 numeric?))
(def json-double
  (p-take-while-1
   #(or (numeric? %) (= \. %))))

(def json-number (p-map #(if (some? %) (parse-double %) nil) (alt json-double json-int)))

(def quote (tag "\""))
; TODO: handle escaped quotes
(def json-string (delimited quote (p-take-while #(not= % \")) quote))

(def comma (pseq ws (tag ",") ws))
(def open-b (pseq (tag "[") ws))
(def close-b (pseq ws (tag "]")))

(def json-array (p-map last (pseq open-b (separated comma #(json-value %) close-b))))

(def open-curly (pseq (tag "{") ws))
(def close-curly (pseq (tag "}") ws))
(def empty-object (p-map (const {}) (pseq open-curly ws close-curly)))

(def colon (tag ":"))
(def json-key (p-map keyword json-string))

(def key-value-pair (p-map #(vector (first %) (last %)) (pseq json-key ws colon ws #(json-value %))))
(def keys-separated-by-comma (p-map #(nth % 1) (pseq ws key-value-pair comma)))
(def non-empty-object (p-map #(into {} (conj (nth % 1) (nth % 2))) (pseq open-curly (many-0 keys-separated-by-comma) key-value-pair ws close-curly)))

(def json-object (alt empty-object non-empty-object))

(def json-value (alt json-null json-true json-false json-number json-string json-array json-object))

(def json (delimited ws json-value ws))

(defn -main []
  (println
   (run json "{ \"foo\": 123, \"bar\": 3210 }")))

