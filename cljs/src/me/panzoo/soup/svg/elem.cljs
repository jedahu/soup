(ns me.panzoo.soup.svg.elem
  (:require
    [clojure.string :as s]
    [me.panzoo.soup.svg :as svg]
    [me.panzoo.soup.dom :as dom]))

(defn node [tag attrs & children]
  (apply dom/node [dom/svgns tag] attrs children))

(defn read-d [node]
  (for [s (svg/seq<- (. node -pathSegList))]
    (let [t (. s -pathSegTypeAsLetter)
          k (keyword t)]
      (.log js/console t)
      (condp #(some %2 %1) #{(first (s/lower-case t))}
        "mlt" [k (. s -x) (. s -y)]
        "z" [k]
        "h" [k (. s -x)]
        "v" [k (. s -y)]
        "c" [k (. s -x1) (. s -y1) (. s -x2) (. s -y2) (. s -x) (. s -y)]
        "s" [k (. s -x2) (. s -y2) (. s -x) (. s -y)]
        "q" [k (. s -x1) (. s -y1) (. s -x) (. s -y)]
        "a" [k (. s -r1) (. s -r2) (. s -angle)
             (. s -largeArcFlag) (. s -sweepFlag)
             (. s -x) (. s -y)]))))

(defn- node-attrs [node]
  (into
    {}
    (for [a (. node -attributes)
          k [(if-let [ns (. a -namespaceURI)]
               [ns (keyword (. a -name))]
               (keyword (. a -name)))]]
      [k
       (cond
         (= :transform k) (svg/vector<-matrix (svg/get-matrix node))
         (= :class k) (svg/classes node)
         (= :d k) (read-d node)
         :else (if-let [b (. (or (svg/attr node k)
                                 (dom/attr node k))
                             baseVal)]
                 (cond
                   (. b -getItem) (vec (map #(. % -value) (svg/seq<- b)))
                   (. b -value) (. b -value))
                 (cond
                   (string? v) v)))])))

(defn- rect1
  ([x y w h attrs children]
   (let [r (apply node "rect" attrs children)]
     (set! (.. r -x -baseVal -value) x)
     (set! (.. r -y -baseVal -value) y)
     (set! (.. r -width -baseVal -value) w)
     (set! (.. r -height -baseVal -value) h)
     r))
  ([xy wh attrs children]
   (let [[x y] (svg/pair<-point xy)
         [w h] (svg/pair<-point wh)]
     (rect1 x y w h attrs children)))
  ([r attrs children]
   (let [[x y w h] (svg/vector<-rect r)]
     (rect1 x y w h attrs children))))

(defn rect
  [xywh attrs & children]
  (apply rect1
         (concat
           (if (vector? xywh) xywh [xywh])
           [attrs children])))

(defn- circle1
  ([cx cy r attrs children]
   (let [c (apply node "circle" attrs children)]
     (set! (.. c -cx -baseVal -value) cx)
     (set! (.. c -cy -baseVal -value) cy)
     (set! (.. c -r -baseVal -value) r)
     c))
  ([cxy r attrs children]
   (let [[cx cy] (svg/pair<-point cxy)]
     (circle1 cx cy r attrs children))))

(defn circle
  [cxy r attrs & children]
  (apply circle1
         (concat
           (if (vector? cxy) cxy [cxy])
           [r attrs children])))

(defn path
  [d attrs & children]
  (apply node "path"
         (assoc attrs
                :d (apply str
                          (for [x d]
                            (if (vector? x)
                              (apply str
                                     (name (first x)) " "
                                     (s/join " " (rest x)))
                              (name x)))))
         children))
