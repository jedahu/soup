(ns me.panzoo.soup.css
  (:use
    [clojure.string :only (join)]))

(defn- wrap [n s]
  (loop [n n acc s]
    (if (= 0 n)
      acc
      (recur (dec n) [acc]))))

(defn- normalize-selector
  ([s n]
  (if (vector? s)
    (map #(normalize-selector % (dec n))  s)
    (wrap n (name s))))
  ([s]
   (normalize-selector s 2)))

(defn rules [& rs]
  (join ";" (for [[k v] (partition 2 rs)]
              (str (name k) ":" v))))

(defn css
  ([[selector & rule-list]]
   (str
     (join "," (map #(join " " %)
                    (normalize-selector selector)))
     "{"
     (apply rules rule-list)
     "}"))
  ([& decls]
   (join (map css decls))))
