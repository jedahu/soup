(ns me.panzoo.soup.test
  (:require
    [me.panzoo.soup :as s])
  (:require-macros
    [me.panzoo.soup.macros.phantom :as p]))

(p/assert
  (= {:a 2} (s/rmerge {:a 1} {:a 2})))

(p/assert
  (= {:a 2 :c 3} (s/rmerge {:a {:b 1} :c 3} {:a 2})))

(p/assert
  (= {:a {:b 2} :c 3} (s/rmerge {:a {:b 1} :c 3} {:a {:b 2}})))

(p/assert
  (= [3] (s/rmerge {:a 1} {:a 2} [1] [3])))

(p/assert
  (= {:x 0} (s/rmerge {:a 1} {:b 2} "x" {:x 1} {:x 0})))
