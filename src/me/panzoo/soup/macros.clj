(ns me.panzoo.soup.macros)

(defmacro defslurp
  "`(def name (slurp uri))` but at compile time."
  [name uri]
  `(def ~name ~(slurp uri)))

(defmacro timeout
  "Execute `forms` in order with `t` millisecond delay between each one.
  Requires goog.Timer.

  Modified from http://brianmckenna.org/blog/cps_transform_js."
  [t & forms]
  (reduce
   (fn [x y]
     `(.callOnce goog.Timer (fn [] ~x ~y) ~t))
   `(fn [])
   (reverse forms)))
