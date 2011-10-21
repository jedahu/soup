(ns me.panzoo.soup.dom
  (:require
    [goog.dom :as dom]
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
  (if-let [[ns k] (seq k)]
    (.removeAttributeNS node ns (name k))
    (.removeAttribute node (name k))))

(defn set-attr
  "Set attribute `k` to `(str v)` on `node`. If `k` is a keyword, `(name k)` is
  the attribute name. If `k` is a vector, the first item is the namespace and
  the `name` of the second is the attribute name."
  [node k v]
  (if-let [[ns k] (and (vector? k) k)]
    (.setAttributeNS node ns (name k) (str v))
    (.setAttribute node (name k) (str v))))

(defn set-attrs
  "Set attributes on `node`. `attrs` must be a map of attribute keywords to
  values. If a key is a seq, the first item must be a namespace and the second
  a keyword. See `set-attr` and `set-attr-ns` for more details."
  [node & attrs]
  (doseq [[k v] attrs] (set-attr node k v)))

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
    (apply set-attrs node attrs)
    (doseq [c children]
      (.appendChild node c))
    node))

(defn set-style
  "Set style `(name k)` to `(str v)` on `node`."
  [node k v]
  (style/setStyle node (name k) (str v)))

(defn set-styles
  "Set styles on `node`. `styles` must be a map of key value pairs. See
  `set-style` for more details."
  [node & styles]
  (doseq [[k v] styles] (set-style node k v)))

(defn viewport-size []
  (let [size (. (viewport/getInstanceForWindow)
                (getSize))]
    [(. size width) (. size height)]))

(defn viewport-center []
  (map #(/ % 2) (viewport-size)))

(defn seq<- [s]
  (if (or (instance? js/NodeList s)
          (instance? js/HTMLCollection s))
    (for [x (range (.length s))]
      (aget s x))
    (seq s)))

(defn computed-style
  [node]
  (.getComputedStyle js/window node))

(defn computed-dimensions
  [node]
  (let [cs (computed-style node)
        w (js/parseInt (. cs width))
        h (js/parseInt (. cs height))]
    [w h]))

(defn set-capture
  [node]
  (when (. node setCapture)
    (. node (setCapture))))

(defn copy-children [from to]
  (doseq [n (seq<- (.. from childNodes))]
    (.appendChild to (.cloneNode n true))))

(defn id->node [id & [root]]
  (.getElementById (or root *document* js/document) id))

(defn visible? [node]
  (not= "hidden" (.. node style visibility)))

(defn set-visible [node visible?]
  (set! (.. node style visibility)
        (if visible? "" "hidden")))

(defn create-document [ns root & children]
  (let [doc (.. js/document implementation (createDocument ns root))
        doc-root (. doc documentElement)]
    (doseq [c children]
      (.appendChild doc-root c))
    doc))
