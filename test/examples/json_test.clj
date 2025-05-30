(ns examples.json-test
  (:require [clojure.test :refer [is deftest testing]]
            [yap.test-utils :refer [test-parser]]
            [examples.json :as ex]))

(deftest test-parse-json-true
  (testing "Parse json true"
    (test-parser ex/json-true "true" true)))

(deftest test-parse-json-false
  (testing "Parse json false"
    (test-parser ex/json-false "false" false)))

(deftest test-parse-json-null
  (testing "Parse json null"
    (test-parser ex/json-null "null" :null)))

(deftest test-parse-json-number
  (testing "Parse json number"
    (test-parser ex/json-number "123" 123.0)
    (test-parser ex/json-number "123.456" 123.456)
    (test-parser ex/json-number "\"foo\"" nil)))

(deftest test-parse-json-string
  (testing "Parse json string"
    (test-parser ex/json-string "\"hello\"" "hello")
    (test-parser ex/json-string "\"world\"" "world")))

(deftest test-parse-json-array
  (testing "Parse json array"
    (testing "of empty"
      (test-parser ex/json-array "[]" []))
    (testing "of numbers"
      (test-parser ex/json-array "[1, 2, 3]" '(1.0 2.0 3.0)))
    (testing "of strings"
      (test-parser ex/json-array "[\"a\", \"b\", \"c\"]" (vector "a" "b" "c")))))

(deftest test-parse-json-object
  (testing "Parse json object"
    (test-parser ex/json-object "{\"key\": \"value\"}" {:key "value"})
    (test-parser ex/json-object "{\"foo\": 123, \"bar\": 3210}" {:foo 123.0, :bar 3210.0})
    (test-parser ex/json-object "{}" {})))

