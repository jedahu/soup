(ns me.panzoo.soup.test
  (:require
    [me.panzoo.soup :as s])
  (:use-macros
    [me.panzoo.soup.macros.phantom :only (assert)]))

(assert
  (= {:a 2} (s/rmerge {:a 1} {:a 2})))

(assert
  (= {:a 2 :c 3} (s/rmerge {:a {:b 1} :c 3} {:a 2})))

(assert
  (= {:a {:b 2} :c 3} (s/rmerge {:a {:b 1} :c 3} {:a {:b 2}})))

(assert
  (= [3] (s/rmerge {:a 1} {:a 2} [1] [3])))

(assert
  (= {:x 0} (s/rmerge {:a 1} {:b 2} "x" {:x 1} {:x 0})))

(.exit js/phantom 0)
