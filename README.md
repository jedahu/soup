# Soup

Useful clojurescript functions and macros.

## Namespaces and functions

### me.panzoo.soup.geometry

Miscellaneous functions.

### me.panzoo.soup.dom

Browser DOM manipulation.

### me.panzoo.soup.svg

Functions on SVG DOM and associated types.

### me.panzoo.soup.macros

#### defslurp

`(def name (slurp uri))` but at compile time.

#### timeout

Execute forms in order with a time delay between each.

### me.panzoo.soup.macros.phantom

For use with [phantomjs](http://phantomjs.org).

#### passert

An assert macro for use with phantomjs.
