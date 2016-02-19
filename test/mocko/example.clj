(ns mocko.example)

(defn nullary
  []
  "nullary")

(defn unary
  [a]
  (str "unary " a))

(defn binary
  [a b]
  (str "binary " a " " b))
