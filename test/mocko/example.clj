(ns mocko.example)

(def value 100)

(defn nullary
  []
  "nullary")

(defn unary
  [a]
  (str "unary " a))

(defn binary
  [a b]
  (str "binary " a " " b))

(defn trinary
  [a b c]
  (str "trinary " a " " b " " c))
