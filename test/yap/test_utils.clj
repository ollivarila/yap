(ns yap.test-utils
  (:require [clojure.test :refer [is]]
            [yap.core :as yap]))

(defn test-parser [parser input expected]
  (is (= (yap/run-all-consuming parser input) expected)))

(defn test-parser-output [parser input parsed rem]
  (let [output (parser input)]
    (is (some? output))
    (let [[o r] output]
      (is (= o parsed))
      (is (= r rem)))

    nil))

