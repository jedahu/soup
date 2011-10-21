(ns me.panzoo.soup.svg
  (:require
    [goog.style :as style]
    [goog.dom :as dom]))

(defn set-matrix
  "Set the transformation matrix of `node` to SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node transform baseVal)
        t (. ts (createSVGTransformFromMatrix mx))]
    (. ts (initialize t))))

(defn get-matrix
  "Get the transformation matrix of `node`. Returns an SVGMatrix."
  [node]
  (let [baseVal (.. node transform baseVal)
        trans (. baseVal (consolidate))]
    (if trans
      (. trans matrix)
      (do
        (.setAttribute node "transform" "scale(1)")
        (.. baseVal (consolidate) matrix)))))

(defn *-matrix
  "Multiply transform of `node` with SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node transform baseVal)
        curr (get-matrix node)
        t (.. ts (createSVGTransformFromMatrix (.multiply curr mx)))]
    (.initialize ts t)))

(defn clear-matrix
  "Set transform of `node` to the identity matrix. Returns a reference to the
  SVGMatrix."
  [node]
  (let [mx (get-matrix node)]
    (set! (. mx a) 1)
    (set! (. mx b) 0)
    (set! (. mx c) 0)
    (set! (. mx d) 1)
    (set! (. mx e) 0)
    (set! (. mx f) 0)
    (set-matrix node mx)
    mx))

(defn point
  "Create an SVGPoint with svg root element `svg` and numbers `x` and `y`."
  [svg x y]
  (let [p (. svg (createSVGPoint))]
    (set! (. p x) x)
    (set! (. p y) y)
    p))

(defn evt-client-point
  "Create an SVGPoint with svg root element `svg` and client coordinates from
  browser event `evt`."
  [svg evt]
  (point svg (. evt clientX) (. evt clientY)))

(defn evt-offset-point
  "Create an SVGPoint with svg root element `svg` and offset coordinates from
  browser event `evt`."
  [svg evt]
  (point svg (. evt offsetX) (. evt offsetY)))

(defn point->pair
  "Convert an SVGPoint to a two-element vector."
  [p]
  [(. p x) (. p y)])

(defn pair->point
  "Convert a two-element vector to an SVGPoint."
  [svg [x y]]
  (point svg x y))

(defn pointwise
  "Apply `op` pointwise to the SVGPoints `pts`, using svg root element `svg` to
  construct the resulting SVGPoint."
  [svg op & pts]
  (point svg
         (apply op (map #(. % x) pts))
         (apply op (map #(. % y) pts))))

(defn matrix
  "Create an SVGMatrix with svg root element `svg` and optional values `a`
  through `f`. If no values are supplied the identity matrix is returned."
  [svg & [a b c d e f]]
  (let [mx (. svg (createSVGMatrix))]
    (when a
      (set! (. mx a) a)
      (set! (. mx b) b)
      (set! (. mx c) c)
      (set! (. mx d) d)
      (set! (. mx e) e)
      (set! (. mx f) f))
    mx))

(defn matrix->vector
  "Convert an SVGMatrix to a six-element vector."
  [mx]
  [(. mx a) (. mx b) (. mx c) (. mx d) (. mx e) (. mx f)])

(defn distance
  "Calculate the distance between two SVGPoints."
  [p1 p2]
  (let [[x1 y1] (point->pair p1)
        [x2 y2] (point->pair p2)
        dx (- x1 x2)
        dy (- y1 y2)]
    (.sqrt js/Math (+ (* dx dx) (* dy dy)))))

(defn classes [node]
  (set (.. node className baseVal (split #"\s+"))))

(defn rect-center [r]
  (let [x (. r x)
        y (. r y)
        w (. r width)
        h (. r height)]
    [(+ x (/ w 2)) (+ y (/ h 2))]))

(defn bbox-center [node]
  (rect-center (. node (getBBox))))
