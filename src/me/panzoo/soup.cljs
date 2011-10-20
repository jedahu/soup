(ns me.panzoo.soup)

(defn rmerge-with
  "Merge maps recursively. Apply `f` to a level when it contains a non-map.

  `f` should take a single argument which is a list of objects which may be maps."
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? #(or (map? %) (not %)) maps)
        (apply merge-with m maps)
        (f maps)))
    maps))

(defn rmerge
  "Merge maps recursively. If a level contains a non-map, the value for that
  level will be the result of merging the maps at the end of the sequence or it
  will be the last item in the sequence."
  [& maps]
  (letfn [(f [& maps]
            (let [maps* (reverse
                          (take-while
                            (or (map? x) (not x))
                            (reverse maps)))]
              (if (seq maps*)
                (apply rmerge-with f maps*)
                (last maps))))]
    (apply rmerge-with f maps)))
