(ns mocko.core
  "Mocko is a simple mocking library."
  (:require [clojure.data :as data]
            [clojure.string :as string]
            [clojure.test :as test]))

(def context (atom nil))

(defmacro with-mocks
  "A macro which establishes a lexical scope for mock applicability."
  [& body]
  `(try
     (when @context
       (throw (IllegalStateException. "Cannot nest mocks")))

     ;; create new context
     (reset! context {:originals {}
                      :mocks {}
                      :calls []})
     ~@body
     ;; check that all mocks were called
     (when-let [uncalled# (first (data/diff
                                  (set (keys (:mocks @context)))
                                  (set (map first (:calls @context)))))]
       (test/do-report {:type :fail
                        :expected uncalled#
                        :message "Some mocks were not called."}))
     (finally
       ;; restore all originals
       (run! (fn [[ref-fn# fn#]]
               (alter-var-root ref-fn# (constantly fn#)))
             (:originals @context))
       ;; wipe context
       (reset! context nil))))

(defn verify-call-order
  "Verifies the given mocked functions were called in the given order."
  [& ref-fns]
  (when-not @context
    (throw (IllegalStateException.
            "can't verify mocks outside of with-mocks")))
  (let [called (filter (set ref-fns) (map first (:calls @context)))]
    (when-not (= ref-fns called)
      (test/do-report {:type :fail
                       :expected (vec ref-fns)
                       :actual (vec called)
                       :message "Mocks were called out of order."}))))

(defn- mock-fn
  [ref-fn values]
  (fn [& args]
    (let [args (or args [])] ; handle empty args
      (swap! context update-in [:calls] conj [ref-fn args])
      (if (fn? values)
        (apply values args)
        (if (contains? values args)
          (let [ret (get values args)]
            (if (fn? ret) (apply ret args) ret))
          (test/do-report {:type :fail
                           :expected values
                           :actual (vec args)
                           :message (str "Unexpected arguments for "
                                         ref-fn)}))))))

(defn mock!
  "Mocks the given function. Takes either a map of function arguments to
  function return values or an actual function."
  [ref-fn values]
  (when-not @context
    (throw (IllegalStateException. "can't mock outside of with-mocks")))

  ;; save original fn
  (swap! context assoc-in [:originals ref-fn] @ref-fn)

  (let [m (mock-fn ref-fn values)]
    (swap! context assoc-in [:mocks ref-fn] m)
    (alter-var-root ref-fn (constantly m))))
