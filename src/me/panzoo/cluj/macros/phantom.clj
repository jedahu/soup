(ns me.panzoo.cluj.macros.phantom)

(defmacro passert
  "An assert macro for phantomjs that exits on failure.
  
  The following must be in an included externs file:
      var phantom;
      phantom.exit = function(x) {};"
  [form & [msg]]
  `(when-not ~form
     (.log js/console (cljs.core/str "Assertion failed: " ~msg))
     (.log js/console (cljs.core/pr-str '~form))
     (.exit js/phantom 1)))
