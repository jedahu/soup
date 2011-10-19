(ns me.panzoo.soup.geometry)

(defn angle-A
  "Use the sine rule to calculate angle A for a triangle with sides of length
  `a`, `b`, and `c`, where `a` is the length of the side opposite angle A."
  [a b c]
  (.acos js/Math
         (/ (- (+ (* b b)
                  (* c c))
               (* a a))
            (* 2 b c))))

(defn line-y
  "Find f(x) for the line `[x1 y1]` to '[x2 y2]`."
  [x [x1 y1] [x2 y2]]
  (+ y1
     (/ (* (- x x1)
           (- y2 y1))
        (- x2 x1))))
