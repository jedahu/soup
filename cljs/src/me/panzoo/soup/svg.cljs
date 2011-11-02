(ns me.panzoo.soup.svg
  (:use
    [clojure.string :only (join)])
  (:require
    [me.panzoo.soup.dom :as dom]
    [goog.style :as style]
    [goog.events :as events]
    [goog.events.EventType :as event-type]))

(defn attr [node k]
  (aget node (name k)))

(defn attr-val [node k]
  (when-let [a (attr node k)]
    (when-let [b (. a baseVal)]
      (. a value))))

(defn seq<-
  [s]
  (for [i (range (. s numberOfItems))]
    (.getItem s i)))

(extend-protocol ISeqable
  js/SVGLengthList
  (-seq [coll] (seq<- coll))

  js/SVGPathSegList
  (-seq [coll] (seq<- coll)))

(defn owner-svg
  "Return the SVGSVGElement for `node`, or `nil`. If `node` is an SVGSVGElement,
  return it."
  [node]
  (or (. node ownerSVGElement)
      (and (= (. node tagName) "svg")
           (= (. node namespaceURI) dom/svgns)
           node)))

(defn matrix
  "Create an SVGMatrix with svg root element `svg` and optional values `a`
  through `f`. If no values are supplied the identity matrix is returned."
  ([svg]
   (. svg (createSVGMatrix)))
  ([svg a b c d e f]
   (let [mx (matrix svg)]
     (set! (. mx a) a)
     (set! (. mx b) b)
     (set! (. mx c) c)
     (set! (. mx d) d)
     (set! (. mx e) e)
     (set! (. mx f) f)
     mx)))

(defn vector<-matrix
  "Convert an SVGMatrix to a six-element vector."
  [mx]
  [(. mx a) (. mx b) (. mx c) (. mx d) (. mx e) (. mx f)])

(defn matrix-components
  "Return the translation, rotation, and scale components of the transformation
  matrix of `node` in a map:

  :translation [x y]
  :rotation radians
  :scale [sx sy]"
  [node]
  (let [[a b c d e f] (vector<-matrix (get-matrix node))]
    {:translation [e f]
     :rotation (Math/atan2 b a)
     :scale [(Math/sqrt (+ (* a a) (* b b)))
             (Math/sqrt (+ (* c c) (* d d)))]}))

(defn set-matrix
  "Set the transformation matrix of `node` to SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node transform baseVal)
        t (. ts (createSVGTransformFromMatrix mx))]
    (. ts (initialize t))))

(defn get-matrix
  "Get the transformation matrix of `node`. Returns an SVGMatrix."
  [node]
  (when-not (.getAttribute node "transform")
    (.setAttribute node "transform" "scale(1)"))
  (.. node transform baseVal (consolidate) matrix))

(defn *-matrix
  "Multiply transform of `node` with SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node transform baseVal)
        curr (get-matrix node)
        t (.. ts (createSVGTransformFromMatrix (.multiply curr mx)))]
    (.initialize ts t)))

(defn matrix-*
  "Multiply SVGMatrix `mx` with transform of `node`."
  [mx node]
  (let [ts (.. node transform baseVal)
        curr (get-matrix node)
        t (.. ts (createSVGTransformFromMatrix (.multiply mx curr)))]
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

(defn matrix-between
  "An alias for the DOM method `getTransformToElement`."
  [from to]
  (.getTransformToElement from to))

(defn transform-point
  "Transform `point` using an SVGMatrix `mx` or the transform between the nodes
  `from` and `to`."
  ([point mx]
   (.matrixTransform point mx))
  ([point from to]
   (transform-point point (matrix-between from to))))

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
  [evt]
  (point (owner-svg (.. evt target)) (. evt clientX) (. evt clientY)))

(defn evt-offset-point
  "Create an SVGPoint with svg root element `svg` and offset coordinates from
  browser event `evt`."
  [evt]
  (point (owner-svg (.. evt target)) (. evt offsetX) (. evt offsetY)))

(defn pair<-point
  "Convert an SVGPoint to a two-element vector."
  [p]
  [(. p x) (. p y)])

(defn point<-pair
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

(defn distance
  "Calculate the distance between two SVGPoints."
  [p1 p2]
  (let [[x1 y1] (pair<-point p1)
        [x2 y2] (pair<-point p2)
        dx (- x1 x2)
        dy (- y1 y2)]
    (.sqrt js/Math (+ (* dx dx) (* dy dy)))))

(defn classes
  "Return the classes of `node` as a set."
  [node]
  (if (and (. node className) (.. node className baseVal))
    (set (.. node className baseVal (split #"\s+")))
    #{}))

(defn del-classes
  "Remove `class-names` from `node`."
  [node & class-names]
  (set! (.. node className baseVal)
        (join " " (remove (set class-names) (classes node)))))

(defn clear-classes
  "Remove all classes from `node`."
  [node]
  (set! (.. node className) ""))

(defn rect
  "Create a new SVGRect using the SVGSVGElement `svg`, with or without
  dimensions."
  ([svg]
   (. svg (createSVGRect)))
  ([svg x y w h]
   (let [r (rect svg)]
     (set! (. r x) x)
     (set! (. r y) y)
     (set! (. r width) w)
     (set! (. r height) h)
     r)))

(defn vector<-rect
  "Convert an SVGRect to a vector `[x y width height]`."
  [r]
  [(. r x) (. r y) (. r width) (. r height)])

(defn rect-center
  "Return the center of SVGRect `r` as a vector pair."
  [r]
  (let [x (. r x)
        y (. r y)
        w (. r width)
        h (. r height)]
    [(+ x (/ w 2)) (+ y (/ h 2))]))

(defn bbox-center
  "Return the center of the BBox of `node` as a vector pair."
  ([node]
   (rect-center (. node (getBBox))))
  ([node svg]
   (apply point svg (bbox-center node))))

(defn bbox-for-target
  "Transform the BBox of `node` to the coordinate space of `target`.

  Returns a vector of vector pairs, which when used in the data attribute of
  a polygon will draw a transformed rectangle."
  [node target]
  (let [svg (. node ownerSVGElement)
        [x y w h] (vector<-rect (. node (getBBox)))
        mx (.getTransformToElement node target)
        p1 (point svg x y)
        p2 (point svg (+ x w) y)
        p3 (point svg (+ x w) (+ y h))
        p4 (point svg x (+ y h))]
    (for [p [p1 p2 p3 p4]]
      (pair<-point (.matrixTransform p mx)))))

(defn node-from-point
  "Get the topmost node under the point `[x y]` under the svg root `svg`."
  ([svg x y]
   (if (.. svg ownerDocument elementFromPoint)
     (.. svg ownerDocument (elementFromPoint x y))
     (last (seq<- (. svg (getIntersectionList (rect svg x y 1 1) nil))))))
  ([svg point]
   (apply node-from-point svg (pair<-point point))))

(defn ancestor-by-class
  "Ported from me.panzoo.soup.dom to work with the SVG class attribute."
  [node class]
  (loop [node node]
    (cond
      ((classes node) class) node
      (. node parentNode) (recur (. node parentNode))
      :else nil)))

(defprotocol CenterOrigin
  (-center-origin [node]))

(defn- center-origin-xywh [node]
  (let [[x y w h] (map js/parseFloat (dom/attrs node :x :y :width :height))
        cx (/ w 2)
        cy (/ h 2)]
    (dom/set-attrs node :x (- cx) :y (- cy))
    (set-matrix
      node (. (get-matrix node)
              (translate (+ x cx) (+ y cy))))))

(defn- center-origin-xybb [node]
  (let [[x y _ _] (vector<-rect (. node (getBBox)))
        [cx cy] (bbox-center node)]
    (dom/set-attrs node :x (- cx) :y (- cy))
    (set-matrix
      node (. (get-matrix node)
              (translate (+ x cx) (+ y cy))))))

(extend-protocol CenterOrigin

  js/SVGRectElement
  (-center-origin [node]
    (center-origin-xybb node))

  js/SVGEllipseElement
  (-center-origin [node]
    (let [[cx cy] (dom/attrs node :cx :cy)]
      (dom/set-attrs node :cx 0 :cy 0)
      (set-matrix
        node (. (get-matrix node)
                (translate cx cy)))))

  js/SVGGElement
  (-center-origin [node]
    (let [[cx cy] (bbox-center node)]
      (doseq [n (dom/seq<- (. node childNodes))]
        (when (instance? js/Element n)
          (set-matrix
            n (. (get-matrix n)
                 (translate (- cx) (- cy))))))
      (set-matrix
        node (. (get-matrix node)
                (translate cx cy)))))

  js/SVGTextElement
  (-center-origin [node]
    ; Why is bbox.y incorrect?
    (let [x (.. node x baseVal (getItem 0) value)
          y (.. node y baseVal (getItem 0) value)
          [bx by _ _] (vector<-rect (. node (getBBox)))
          [cx cy] (bbox-center node)]
      (set! (.. node x baseVal (getItem 0) value) (- cx))
      (set! (.. node y baseVal (getItem 0) value) (- cy))
      (set-matrix
        node (. (get-matrix node)
                (translate (+ x cx) (+ y cy)))))))

(defn center-origin
  "Modify and transform `node` so its origin of transformation is the center of
  its BBox."
  [node]
  (-center-origin node))

(defn img<-
  "Render `svg` to a new HTML `img` element. If an :error option is provided
  its value must be a function of one argument (the image); it will be called
  once if the error event fires."
  [svg & opts]
  (let [opts (apply hash-map opts)
        xml (.serializeToString (js/XMLSerializer.) svg)
        img (js/Image.)]
    (when-let [error (:error opts)]
      (events/listenOnce img event-type/ERROR #(error img)))
    (set! (. img src) (str "data:image/svg+xml;base64," (js/btoa xml)))
    img))
