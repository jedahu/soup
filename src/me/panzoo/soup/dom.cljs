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

(defn set-attr
  "Set attribute `(name k)` to `(str v)` on `node`."
  [node k v & opts]
  (let [[ns|tag & [tag]] (.split (name k) ":")]
    (if tag
      (.setAttributeNS node ns|tag (name k) (str v))
      (.setAttributeNS node nil ns|tag (str v)))))

(defn set-attrs
  "Set attributes on `node`. `attrs` must be a map of attribute names to
  values. See `set-attr` for more details."
  [node & attrs]
  (doseq [[k v] attrs] (set-attr node k v)))

(defn- node
  "Usually better to use `node-maker`.
  
  Create a DOM node from document `doc` with attributes and child nodes. `opts`
  may have the following keys:
  
  :ns-prefix-map (IPersistentMap) Defaults to `default-prefix-map`.
  
  See also `set-attrs` and `set-attr`."
  [doc tag attrs children opts]
  (let [[ns|tag & [tag]] (.split tag ":")
        n (if tag
            (.createElementNS
              doc
              ((:ns-prefix-map opts) ns|tag)
              tag)
            (.createElementNS
              doc
              (or (:null-ns opts) nil)
              ns|tag))]
    (apply set-attrs n attrs)
    (doseq [c children]
      (.appendChild n c))
    n))

(defn node-maker
  "Create a node-making function with `opts` as its defaults. `opts` may have
  the following keys:
  
  :ns-prefix-map (IPersistentMap) Defaults to `default-prefix-map`.

  :null-ns (String) Defaults to `nil`.

  :document (DOM Document) Defaults to `js/document`.
  
  The returned function has the signature `[tag & [attrs & children]`, where
  `tag` is a string denoting the namespace prefix and tag-name, `attrs` is a
  map of keyword attribute names to attributes, and `children` is a sequence of
  child nodes."
  [& {:as opts}]
  (fn [tag & [attrs & children]]
    (node (or (:document opts)
              js/document)
          tag attrs children
          (merge {:ns-prefix-map default-prefix-map
                  :null-ns xhtmlns}
                 opts))))

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
