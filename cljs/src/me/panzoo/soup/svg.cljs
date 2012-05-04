(ns me.panzoo.soup.svg
  (:use
    [clojure.string :only (join)])
  (:require
    [me.panzoo.soup.dom :as dom]
    [goog.style :as style]
    [goog.events :as events]
    [goog.events.EventType :as event-type]))

(defn seq<-
  [s]
  (when s
    (for [i (range (. s -numberOfItems))]
      (.getItem s i))))

(defn attr [node k & [anim?]]
  (when-let [a (aget node (name k))]
    (cond
      (. a -baseVal) (let [v (if anim? (. a -animVal) (. a -baseVal))]
                      (if (. v -numberOfItems)
                        (seq<- v)
                        (. v -value)))
      (. a -numberOfItems) (seq<- a)
      (. a -value) (. a -value)
      :else a)))

(defn owner-svg
  "Return the SVGSVGElement for `node`, or `nil`. If `node` is an SVGSVGElement,
  return it."
  [node]
  (or (. node -ownerSVGElement)
      (and (= (. node -tagName) "svg")
           (= (. node -namespaceURI) dom/svgns)
           node)))

(defn matrix
  "Create an SVGMatrix with svg root element `svg` and optional values `a`
  through `f`. If no values are supplied the identity matrix is returned."
  ([svg]
   (.createSVGMatrix svg))
  ([svg a b c d e f]
   (let [mx (matrix svg)]
     (set! (. mx -a) a)
     (set! (. mx -b) b)
     (set! (. mx -c) c)
     (set! (. mx -d) d)
     (set! (. mx -e) e)
     (set! (. mx -f) f)
     mx)))

(defn vector<-matrix
  "Convert an SVGMatrix to a six-element vector."
  [mx]
  [(. mx -a) (. mx -b) (. mx -c) (. mx -d) (. mx -e) (. mx -f)])

(defn get-matrix
  "Get the transformation matrix of `node`. Returns an SVGMatrix."
  [node]
  (when-not (.getAttribute node "transform")
    (.setAttribute node "transform" "scale(1)"))
  (.. node -transform -baseVal (consolidate) -matrix))

(defn matrix-components
  "Return the translation, rotation, and scale components of the transformation
  matrix of `node` in a map:

  :translation [x y]
  :rotation radians
  :scale [sx sy]"
  [node|mx]
  (let [[a b c d e f] (vector<-matrix
                        (if (instance? js/SVGMatrix node|mx)
                          node|mx
                          (get-matrix node|mx)))]
    {:translation [e f]
     :rotation (Math/atan2 b a)
     :scale [(Math/sqrt (+ (* a a) (* b b)))
             (Math/sqrt (+ (* c c) (* d d)))]}))

(defn set-matrix
  "Set the transformation matrix of `node` to SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node -transform -baseVal)
        t (.createSVGTransformFromMatrix ts mx)]
    (.initialize ts t)))

(defn *-matrix
  "Multiply transform of `node` with SVGMatrix `mx`."
  [node mx]
  (let [ts (.. node -transform -baseVal)
        curr (get-matrix node)
        t (.createSVGTransformFromMatrix ts (.multiply curr mx))]
    (.initialize ts t)))

(defn matrix-*
  "Multiply SVGMatrix `mx` with transform of `node`."
  [mx node]
  (let [ts (.. node -transform -baseVal)
        curr (get-matrix node)
        t (.createSVGTransformFromMatrix ts (.multiply mx curr))]
    (.initialize ts t)))

(defn clear-matrix
  "Set transform of `node` to the identity matrix. Returns a reference to the
  SVGMatrix."
  [node]
  (let [mx (get-matrix node)]
    (set! (. mx -a) 1)
    (set! (. mx -b) 0)
    (set! (. mx -c) 0)
    (set! (. mx -d) 1)
    (set! (. mx -e) 0)
    (set! (. mx -f) 0)
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
  (let [p (.createSVGPoint svg)]
    (set! (. p -x) x)
    (set! (. p -y) y)
    p))

(defn evt-client-point
  "Create an SVGPoint with svg root element `svg` and client coordinates from
  browser event `evt`."
  [evt & [svg]]
  (point (or svg (owner-svg (. evt -target)))
         (. evt -clientX) (. evt -clientY)))

(defn evt-offset-point
  "Create an SVGPoint with svg root element `svg` and offset coordinates from
  browser event `evt`."
  [evt]
  (point (owner-svg (. evt -target)) (. evt -offsetX) (. evt -offsetY)))

(defn pair<-point
  "Convert an SVGPoint to a two-element vector."
  [p]
  [(. p -x) (. p -y)])

(defn point<-pair
  "Convert a two-element vector to an SVGPoint."
  [svg [x y]]
  (point svg x y))

(defn pointwise
  "Apply `op` pointwise to the SVGPoints `pts`, using svg root element `svg` to
  construct the resulting SVGPoint."
  [svg op & pts]
  (point svg
         (apply op (map #(. % -x) pts))
         (apply op (map #(. % -y) pts))))

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
  (if (and (. node -className)
           (.. node -className -baseVal))
    (set (.. node -className -baseVal (split #"\s+")))
    #{}))

(defn del-classes
  "Remove `class-names` from `node`."
  [node & class-names]
  (set! (.. node -className -baseVal)
        (join " " (remove (set class-names) (classes node)))))

(defn clear-classes
  "Remove all classes from `node`."
  [node]
  (set! (. node -className) ""))

(defn rect
  "Create a new SVGRect using the SVGSVGElement `svg`, with or without
  dimensions."
  ([svg]
   (.createSVGRect svg))
  ([svg x y w h]
   (let [r (rect svg)]
     (set! (. r -x) x)
     (set! (. r -y) y)
     (set! (. r -width) w)
     (set! (. r -height) h)
     r)))

(defn vector<-rect
  "Convert an SVGRect to a vector `[x y width height]`."
  [r]
  [(. r -x) (. r -y) (. r -width) (. r -height)])

(defn rect-center
  "Return the center of SVGRect `r` as a vector pair."
  [r]
  (let [x (. r -x)
        y (. r -y)
        w (. r -width)
        h (. r -height)]
    [(+ x (/ w 2)) (+ y (/ h 2))]))

(defn bbox-center
  "Return the center of the BBox of `node` as a vector pair."
  ([node]
   (rect-center (.getBBox node)))
  ([node svg]
   (apply point svg (bbox-center node))))

(defn bbox-for-target
  "Transform the BBox of `node` to the coordinate space of `target`.

  Returns a vector of vector pairs, which when used in the data attribute of
  a polygon will draw a transformed rectangle."
  [target node]
  (let [svg (. node -ownerSVGElement)
        [x y w h] (vector<-rect (.getBBox node))
        mx (.getTransformToElement node target)
        p1 (point svg x y)
        p2 (point svg (+ x w) y)
        p3 (point svg (+ x w) (+ y h))
        p4 (point svg x (+ y h))]
    (for [p [p1 p2 p3 p4]]
      (pair<-point (.matrixTransform p mx)))))

(defn bboxes-for-target
  [target & nodes]
  (vec (map (partial bbox-for-target target) nodes)))

(defn node-from-point
  "Get the topmost node under the point `[x y]` under the svg root `svg`."
  ([svg x y]
   (if (.. svg -ownerDocument -elementFromPoint)
     (.. svg -ownerDocument (elementFromPoint x y))
     (last (seq<- (.getIntersectionList svg (rect svg x y 1 1) nil)))))
  ([svg point]
   (apply node-from-point svg (pair<-point point))))

(defn ancestor-by-class
  "Ported from me.panzoo.soup.dom to work with the SVG class attribute."
  [node class]
  (loop [node node]
    (cond
      ((classes node) class) node
      (. node -parentNode) (recur (. node -parentNode))
      :else nil)))

(defprotocol CenterOrigin
  (-center-origin [node xy]))

(defn- center-origin-xywh [node]
  (let [[x y w h] (map js/parseFloat (dom/attrs node :x :y :width :height))
        cx (/ w 2)
        cy (/ h 2)]
    (dom/set-attrs node :x (- cx) :y (- cy))
    (set-matrix
      node (. (get-matrix node)
              (translate (+ x cx) (+ y cy))))))

(defn- center-origin-xybb [node xy]
  (let [[x y _ _] (vector<-rect (.getBBox node))
        [cx cy] (or xy (bbox-center node))]
    (dom/set-attrs node :x (- x cx) :y (- x cy))
    (set-matrix
      node (. (get-matrix node)
              (translate (+ cx) (+ cy))))))

(extend-protocol CenterOrigin

  js/SVGRectElement
  (-center-origin [node xy]
    (center-origin-xybb node xy))

  js/SVGEllipseElement
  (-center-origin [node xy]
    (let [[cx cy] (dom/attrs node :cx :cy)
          [x y] (or xy [0 0])]
      (dom/set-attrs node :cx (- cx x) :cy (- cy y))
      (set-matrix
        node (. (get-matrix node)
                (translate (+ cx x) (+ cy y))))))

  js/SVGGElement
  (-center-origin [node xy]
    (let [[cx cy] (or xy (bbox-center node))]
      (doseq [n (dom/seq<- (. node -childNodes))]
        (when (instance? js/Element n)
          (set-matrix
            n (. (get-matrix n)
                 (translate (- cx) (- cy))))))
      (set-matrix
        node (. (get-matrix node)
                (translate cx cy)))))

  js/SVGPolygonElement
  (-center-origin [node xy]
    (let [svg (owner-svg node)
          [x y _ _] (vector<-rect (.getBBox node))
          [cx cy] (or xy (bbox-center node))
          pts (. node -points)
          pts* (map pair<-point (vec (seq<- pts)))]
      (.clear pts)
      (doseq [[x y] pts*]
        (.appendItem pts (point svg (- x cx) (- y cy))))
      (set-matrix
        node (. (get-matrix node)
                (translate cx cy)))))

  js/SVGTextElement
  (-center-origin [node xy]
    ; Why is bbox.y incorrect?
    (let [x (.. node -x -baseVal (getItem 0) -value)
          y (.. node -y -baseVal (getItem 0) -value)
          [bx by _ _] (vector<-rect (.getBBox node))
          [cx cy] (or xy (bbox-center node))]
      (set! (.. node -x -baseVal (getItem 0) -value) (- x cx))
      (set! (.. node -y -baseVal (getItem 0) -value) (- y cy))
      (set-matrix
        node (. (get-matrix node)
                (translate (+ x cx) (+ y cy)))))))

(defn center-origin
  "Modify and transform `node` so its origin of transformation is the center of
  its BBox."
  [node & [xy]]
  (-center-origin node xy))

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
    (set! (. img -src) (str "data:image/svg+xml;base64," (js/btoa xml)))
    img))

(defn with-g-wrap [node f]
  (let [g (dom/node [dom/svgns "g"])
        p (. node -parentNode)]
    (. p insertBefore g node)
    (. g appendChild node)
    (let [ret (f g)]
      (. p insertBefore node g)
      (. p removeChild g)
      ret)))

(defn rects-intersect?
  [r1 r2]
  (and (< (. r2 -x) (+ (. r1 -x) (. r1 -width)))
       (> (+ (. r2 -x) (. r2 -width)) (. r1 -x))
       (< (. r2 -y) (+ (. r1 -y) (. r1 -height)))
       (> (+ (. r2 -y) (. r2 -height)) (. r1 -y))))

;; Get all elements that have a BBox (excludes <defs>, <title>, etc).
;; Note that 0-opacity, off-screen etc elements are still considered "visible"
;; for this function
;;
;; Parameters:
;; parent - The parent DOM element to search within
;;
;; Returns:
;; An array with objects that include:
;; * elem - The element
;; * bbox - The element's BBox as retrieved from getStrokedBBox
(defn visible-nodes+bboxes
  [parent]
  (for [node (dom/seq<- (. parent -children))
        :when (. node -getBBox)]
    {:node node :bbox (with-g-wrap node #(. % getBBox))}))

;; Since the only browser that supports the SVG DOM getIntersectionList is Opera,
;; we need to provide an implementation here.  We brute-force it for now.
;;
;; Reference:
;; Firefox does not implement getIntersectionList(), see https://bugzilla.mozilla.org/show_bug.cgi?id=501421
;; Webkit does not implement getIntersectionList(), see https://bugs.webkit.org/show_bug.cgi?id=11274
(defn intersection-list
  [svg rect parent]
  (if (. svg -getIntersectionList)
    (. svg getIntersectionList rect parent)
    (let [bboxes (visible-nodes+bboxes parent)]
      (filter #(rects-intersect? rect %) bboxes))))
