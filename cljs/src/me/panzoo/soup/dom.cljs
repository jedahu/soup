(ns me.panzoo.soup.dom
  (:require
    [goog.dom :as dom]
    [goog.dom.classes :as classes]
    [goog.style :as style]
    [goog.dom.ViewportSizeMonitor :as viewport]))

(def ^:dynamic *document* nil)

(def xhtmlns
  "The XHTML namespace string."
  "http://www.w3.org/1999/xhtml")

(def svgns
  "The SVG namespace string."
  "http://www.w3.org/2000/svg")

(def xlinkns
  "The XLINK namespace string."
  "http://www.w3.org/1999/xlink")

(def default-ns-prefixes
  "A map of namespace prefixes (keywords) to namespaces (strings). This is the
  default map used by `node` &co."
  {:html xhtmlns
   :svg svgns
   :xlink xlinkns})

(defn text
  "Create a text node with text `s`."
  [s]
  (dom/createTextNode s))

(defn del-attr
  "Remove attribute `k` with namespace `ns`. See `add-attr` for more details."
  [node k]
  (if-let [[ns k] (and (vector? k) k)]
    (.removeAttributeNS node ns (name k))
    (.removeAttribute node (name k))))

(defn set-attr
  "Set attribute `k` to `v` on `node`. `(name k)` is the attribute name, unless
  `k` is a vector, in which case the first item is the namespace and the `name`
  of the second is the attribute name."
  [node k v]
  (if-let [[ns k] (and (vector? k) k)]
    (.setAttributeNS node ns (name k) v)
    (.setAttribute node (name k) v)))

(defn set-attrs
  "Set attributes on `node`. `attrs` must be a map of attribute keywords to
  values. If a key is a seq, the first item must be a namespace and the second
  a keyword. See `set-attr` and `set-attr-ns` for more details."
  [node & {:as attrs}]
  (doseq [[k v] attrs] (set-attr node k v)))

(defn attr
  "Get attribute of `node`. See `set-attr` for details about `k`."
  [node k]
  (if-let [[ns k] (and (vector? k) k)]
    (.getAttributeNS node ns (name k))
    (.getAttribute node (name k))))

(defn attrs
  "Get attributes of `node`. See `attr`."
  [node & ks]
  (for [k ks] (attr node k)))

(defn node
  "Create a DOM node from `*document*` or `js/document` with attributes and
  child nodes. If tag is a vector, the first item is a namespace and the
  the second is the tag name.
  
  See also `set-attrs` and `set-attr`."
  [tag attrs & children]
  (let [doc (or *document* js/document)
        node (if-let [[ns tag] (and (vector? tag) tag)]
               (.createElementNS (or *document* js/document) ns tag)
               (.createElement (or *document* js/document) tag))]
    (apply set-attrs node (apply concat (seq attrs)))
    (doseq [c children]
      (if (string? c)
        (.appendChild node (text c))
        (.appendChild node c)))
    node))

(defn set-style
  "Set style `(name k)` to `(str v)` on `node`."
  ([node k v]
   (style/setStyle node (name k) (str v)))
  ([node k v & kvs]
   (set-style node k v)
   (doseq [[k v] (partition 2 kvs)] (set-style node k v))))

(defn viewport-size
  "Return the page's viewport dimensions as a vector pair."
  []
  (let [size (.getSize (viewport/getInstanceForWindow))]
    [(. size -width) (. size -height)]))

(defn viewport-center
  "Return the coordinates of the center of the page's viewport as a vector
  pair."
  []
  (map #(/ % 2) (viewport-size)))

(defn seq<-
  "Convert an `aget`-able object to a sequence."
  [s]
  (for [x (range (. s -length))]
    (aget s x)))

(defn computed-style
  "Get the computed style of `node`."
  [node]
  (.getComputedStyle js/window node))

(defn computed-dimensions
  "Return the computed dimensions of `node` as a vector pair."
  [node]
  (let [cs (computed-style node)
        w (js/parseInt (. cs -width))
        h (js/parseInt (. cs -height))]
    [w h]))

(defn set-capture
  "If the `setCapture` method exists, call it on `node`."
  [node]
  (when (. node -setCapture)
    (.setCapture node)))

(defn copy-children
  "Copy child nodes of `from` to `to`. If a destination `doc` is supplied, use
  `importNode` instead of `cloneNode`."
  [from to & [doc]]
  (doseq [n (seq<- (. from -childNodes))]
    (.appendChild
      to
      (if doc
        (.importNode doc n true)
        (.cloneNode n true)))))

(defn id->node
  "Return the node with id `id`. Search under `root` or `js/document`."
  [id & [root]]
  (.getElementById (or root *document* js/document) id))

(defn visible?
  "Returns true if `node` is visible (CSS visibility rule)."
  [node]
  (not= "hidden" (.. node -style -visibility)))

(defn set-visible
  "Set CSS visibility rule."
  [node visible?]
  (set! (.. node -style -visibility)
        (if visible? "" "hidden")))

(defn create-document
  "Create a new DOMDocument with a document node with namespace `ns` and tag
  name `root`. Populate with a sequence of `children` nodes."
  [ns root & children]
  (let [doc (.. js/document -implementation (createDocument ns root nil))
        doc-root (. doc -documentElement)]
    (doseq [c children]
      (.appendChild doc-root c))
    doc))

(defn ancestors
  "Return a vector of the ancstors of `node`, including `node` as the first
  item."
  [node]
  (loop [node node acc [node]]
    (if-let [p (. node -parentNode)]
      (recur p (conj acc p))
      acc)))

(defn ancestors-to
  "Return a vector of the ancestors of `node` up to but not including
  `ancestor`. If `ancestor` is not an ancestor of `node`, return `nil`.

  See also `ancestors`."
  [node ancestor]
  (loop [node node acc []]
    (let [p (. node -parentNode)]
      (cond
        (= node ancestor) acc
        p (recur p (conj acc node))
        :else nil))))

(defn ancestor?
  "Tests if `node1` is an ancestor of `node2` or is `node2`."
  [node1 node2]
  (loop [parent node2]
    (or (= parent node1)
        (and (. parent -parentNode)
             (recur (. parent -parentNode))))))

(defn ancestor-by-class
  "Returns the first ancestor of `node` that has `class`, or returns `nil`."
  [node class]
  (loop [node node]
    (cond
      (classes/has node class) node
      (. node -parentNode) (recur (. node -parentNode))
      :else nil)))

(defn remove-node
  "Remove `node` from its parent, if it has one."
  [node]
  (when-let [parent (. node -parentNode)]
    (.removeChild parent node)))

(extend-type js/Node
  IHash
  (-hash [o] (goog.getUid o)))
