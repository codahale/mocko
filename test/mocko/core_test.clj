(ns mocko.core-test
  (:require [clojure.test :refer :all :as test]
            [mocko.core :refer :all]
            [mocko.example :as example]))

(deftest unmocked-behavior-test
  (is (= "nullary" (example/nullary)))
  (is (= "unary :c" (example/unary :c)))
  (is (= "binary :c :d" (example/binary :c :d))))

(deftest mocked-behavior-test
  (testing "Mocking outside a context"
    (is (thrown? IllegalStateException
                 (mock! #'example/binary {})))
    (is (thrown? IllegalStateException
                 (mock! #'example/binary {}))))

  (testing "Nested mocks"
    (is (thrown? IllegalStateException
                 (with-mocks
                   (with-mocks)))))

  (testing "Verifying order outside a context"
    (is (thrown? IllegalStateException
                 (verify-call-order #'example/unary))))

  (testing "Verifying an unmocked function"
    (is (thrown? IllegalArgumentException
                 (with-mocks
                   (verify-call-order #'example/unary)))))

  (testing "Basic mocking"
    (with-mocks
      (mock! #'example/nullary {[] "yay"})
      (mock! #'example/unary {[:a] "woo"
                              [:b] "wahoo"
                              [:c] "whee"})
      (mock! #'example/binary (fn [a b]
                                (str "mocked " a " " b)))
      (mock! #'example/trinary :never)
      (is (= "yay" (example/nullary)))
      (is (= "woo" (example/unary :a)))
      (is (= "wahoo" (example/unary :b)))
      (is (= "whee" (example/unary :c)))
      (is (= "mocked :x :y" (example/binary :x :y)))))

  (testing "Verifying order"
    (with-mocks
      (mock! #'example/nullary {[] "yay"})
      (mock! #'example/unary {[:a] "woo"})
      (example/nullary)
      (example/unary :a)
      (verify-call-order #'example/nullary
                         #'example/unary)))

  (testing "Uncalled mocks"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/nullary {[] "yay"})))
      (is (= [{:type :fail
               :expected [[#'mocko.example/nullary []]]
               :message "Some mocks were not called."}]
             @result))))

  (testing "Uncalled multi-mocks"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/unary {[:a] "yay"
                                   [:b] "woo"})
           (example/unary :a)))
      (is (= [{:type :fail
               :expected [[#'mocko.example/unary [:b]]]
               :message "Some mocks were not called."}]
             @result))))

  (testing "Called mocks"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/nullary :never)
           (example/nullary)))
      (is (= [{:type :fail
               :message "Unexpected call of #'mocko.example/nullary"}]
             @result))))

  (testing "Out-of-order mocks"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/nullary {[] "yay"})
           (mock! #'example/unary {[:a] "woo"})
           (example/unary :a)
           (example/nullary)
           (verify-call-order #'example/nullary
                              #'example/unary)))
      (is (= [{:type :fail
               :expected [#'mocko.example/nullary #'mocko.example/unary]
               :actual [#'mocko.example/unary #'mocko.example/nullary]
               :message "Mocks were called out of order."}]
             @result))))

  (testing "Bad mock args"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/unary {[:a] "woo"})
           (try
             (example/unary :b)
             (catch IllegalArgumentException _))))
      (is (= [{:type :fail
               :expected {[:a] "woo"}
               :actual [:b]
               :message "Unexpected arguments for #'mocko.example/unary"}
              {:type :fail
               :expected [[#'mocko.example/unary [:a]]]
               :message "Some mocks were not called."}]
             @result))))

  (testing "Sequential mocks"
    (with-mocks
      (mock! #'example/unary {[:a] "woo"})
      (is (= "woo" (example/unary :a)))

      (mock! #'example/unary {[:a] "yay"})
      (is (= "yay" (example/unary :a))))

    (is (= "unary :c" (example/unary :c))))

  (testing "Stubbing"
    (is (= 100 example/value))
    (with-mocks
      (stub! #'example/value 200)
      (is (= 200 example/value)))
    (is (= 100 example/value)))

  (testing "Sequential stubs"
    (is (= 100 example/value))
    (with-mocks
      (stub! #'example/value 200)
      (is (= 200 example/value))
      (stub! #'example/value 300)
      (is (= 300 example/value)))
    (is (= 100 example/value)))

  (testing "Stubbing outside a context"
    (is (thrown? IllegalStateException
                 (stub! #'example/unary "woo"))))

  (testing "any args"
    (with-mocks
      (mock! #'example/unary {[:mocko.core/any] "x"})
      (mock! #'example/trinary {[:a :b :mocko.core/any] nil
                                [:mocko.core/any :x :mocko.core/any] 1})
      (is (= "x" (example/unary 1)))
      (is (= "x" (example/unary :foo)))
      (is (= 1 (example/trinary 1 :x 3)))
      (is (nil? (example/trinary :a :b :c)))))

  (testing "unexpected calls with any args"
    (let [result (atom [])]
      (with-redefs-fn {#'test/do-report (fn [m] (swap! result conj m))}
        #(with-mocks
           (mock! #'example/binary {[:a :mocko.core/any] "woo"})
           (try
             (example/binary :b :x)
             (catch IllegalArgumentException _))))
      (is (= [{:type :fail
               :expected {[:a :mocko.core/any] "woo"}
               :actual [:b :x]
               :message "Unexpected arguments for #'mocko.example/binary"}
              {:type :fail
               :expected [[#'mocko.example/binary [:a :mocko.core/any]]]
               :message "Some mocks were not called."}]
             @result))))

  (testing "forces unambiguous args"
    (is (thrown? IllegalArgumentException
                 (with-mocks
                   (mock! #'example/binary {[:a :mocko.core/any] nil
                                            [:mocko.core/any :x] 1}))))

    (is (thrown? IllegalArgumentException
                 (with-mocks
                   (mock! #'example/binary {[:a :b] 1
                                            [:a :mocko.core/any] 2}))))

    (is (thrown? IllegalArgumentException
                 (with-mocks
                   (mock! #'example/binary
                          {[:a :b] 1
                           [:mocko.core/any :mocko.core/any] 2}))))))
