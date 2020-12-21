(ns day01
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn read-numbers [filename]
  (->> filename
       slurp
       s/split-lines
       (map #(Integer/parseInt %))))

(let [numbers (read-numbers "input.txt")]
  (for [a numbers
        b numbers
        :when (< a b)
        :when (= 2020 (+ a b))]
   (print (* a b))))

(let [numbers (read-numbers "input.txt")]
  (for [a numbers
        b numbers
        c numbers
        :when (< a b c)
        :when (= 2020 (+ a b c))]
    (print (* a b c))))