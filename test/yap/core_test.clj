(ns yap.core-test
  (:require [clojure.test :refer [is deftest testing]]
            [yap.core :as core]
            [yap.test-utils :refer [test-parser test-parser-output]]
            [examples.json :as ex]))

(deftest test-const
  (testing "Returns constant value"
    (let [const-fn (core/const 42)]
      (is (= (const-fn) 42))
      (is (= (const-fn 1 2 3) 42)))))

(deftest test-tag
  (testing "Parse tag"
    (test-parser (core/tag "tag") "tag" "tag")
    (test-parser (core/tag "tag") "foo" nil)
    (test-parser-output (core/tag "tag") "tag1" "tag" "1")))

(deftest test-p-map
  (testing "Apply function to parser output"
    (let [parser (core/p-map #(Integer/parseInt %) (core/tag "1"))]
      (test-parser parser "1" 1)
      (test-parser parser "foo" nil))))

(deftest test-alt
  (testing "alt"

    (testing "with 0 parsers"
      (test-parser (core/alt) "foo" nil))

    (testing "with 1 parser"
      (let [parser (core/alt (core/tag "foo"))]
        (test-parser parser "foo" "foo")
        (test-parser parser "bar" nil)
        (test-parser-output parser "foo123" "foo" "123")))

    (testing "with more than 1 parser"
      (let [p1 (core/tag "foo")
            p2 (core/tag "bar")
            parser (core/alt p1 p2)]
        (test-parser parser "foo" "foo")
        (test-parser parser "bar" "bar")
        (test-parser parser "dadfad" nil)))
    (testing "with partially matching input"
      (let [p1 (core/tag "foo")
            p2 (core/tag "bar")
            parser (core/alt p1 p2)]
        (test-parser-output parser "foobar" "foo" "bar")
        (test-parser-output parser "barfoo" "bar" "foo")))))

(deftest test-p-take-while
  (testing "take while pred is true"
    (test-parser-output (core/p-take-while #(= % \a)) "aabb" "aa" "bb"))
  (testing "take 0"
    (test-parser-output (core/p-take-while #(= % \a)) "bb" "" "bb"))

  (testing "take all"
    (test-parser-output (core/p-take-while #(= % \a)) "aa" "aa" "")))

(def a (core/tag "a"))
(def b (core/tag "b"))

(deftest test-delimited
  (testing "parse delimited value"
    (test-parser-output (core/delimited a b a) "aba" "b" ""))
  (testing "fail on missing delims"
    (test-parser (core/delimited a b a) "ba" nil)
    (test-parser (core/delimited a b a) "abb" nil))
  (testing "fail on missing inner value")
  (test-parser (core/delimited a b a) "aaa" nil))

(deftest test-ws
  (testing "parse whitespace"
    (test-parser-output core/ws " \n a" " \n " "a")
    (test-parser-output core/ws "a" "" "a")))

(deftest test-pseq
  (testing "parse sequence of parsers"
    (testing "empty sequence"
      (test-parser (core/pseq) "a" nil))
    (testing "one parser"
      (test-parser-output (core/pseq a) "ab" (vector "a") "b"))
    (testing "multiple parsers"
      (test-parser-output (core/pseq a b) "ab" (vector "a" "b") "")
      (test-parser (core/pseq a b a) "abb" nil))))

(deftest test-terminated
  (testing "parse terminated value"
    (test-parser-output (core/terminated a b) "ab" "a" ""))
  (testing "fail on missing terminator"
    (test-parser (core/terminated a b) "aa" nil)
    (test-parser (core/terminated a b) "a" nil)))

(deftest test-many-till
  (testing "parse many till"
    (test-parser-output (core/many-till a b) "aabb" (list "a" "a") "bb"))
  (testing "fail on missing till"
    (test-parser (core/many-till a b) "aaa" nil))
  (testing "fail on missing inner value"
    (test-parser (core/many-till a b) "ca" nil)))

(deftest test-separated
  (testing "parse separated values"
    (test-parser-output (core/separated a b (core/tag "c")) "babc" (vector "b" "b") ""))
  (testing "fail on missing separator"
    (test-parser (core/separated a b (core/tag "c")) "aac" nil))
  (testing "fail on missing item"
    (test-parser (core/separated a b (core/tag "c")) "bbc" nil))
  (testing "fail on missing terminator"
    (test-parser (core/separated a b (core/tag "c")) "abab" nil)))

(deftest many-0
  (testing "parses 0"
    (test-parser-output (core/many-0 a) "bb" (vector) "bb"))
  (testing "parses 1"
    (test-parser-output (core/many-0 a) "abb" (vector "a") "bb"))
  (testing "parses multiple"
    (test-parser-output (core/many-0 a) "aaabbb" (vector "a" "a" "a") "bbb")))
