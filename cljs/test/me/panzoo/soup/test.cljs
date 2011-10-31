(ns me.panzoo.soup.test
  (:require
    [me.panzoo.soup :as s])
  (:use-macros
    [me.panzoo.soup.macros.phantom :only (passert)]))

(passert
  (= {:a 2} (s/rmerge {:a 1} {:a 2})))

(passert
  (= {:a 2 :c 3} (s/rmerge {:a {:b 1} :c 3} {:a 2})))

(passert
  (= {:a {:b 2} :c 3} (s/rmerge {:a {:b 1} :c 3} {:a {:b 2}})))

(passert
  (= [3] (s/rmerge {:a 1} {:a 2} [1] [3])))

(passert
  (= {:x 0} (s/rmerge {:a 1} {:b 2} "x" {:x 1} {:x 0})))
