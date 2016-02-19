# mocko

An incredibly simple mocking library for Clojure.

## Usage

```clojure
;;; Imagine we have a function, example.com/do-something, which calls another
;;; function, example.other/launch-missiles to launch some missiles.

(ns example.core-test
  (:require [clojure.test :refer :all]
            [example.core :as core]
            [example.other :as other]
            [mocko.core :refer :all]))

(deftest do-something-test
  (with-mocks
    (mock! #'other/launch-missiles {[:russia] :nyet
                                    [:germany] :nein
                                    [:usa] :yeehaw})
    (is (= :nyet (core/do-something :comrade)))))
```

## License

Copyright © 2016 Coda Hale

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
