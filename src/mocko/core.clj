(ns mocko.core
  "Mocko is a simple mocking library."
  (:require [clojure.data :as data]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :as test]))

(def ^:private context
  "The ugly bit of globally mutable donkness."
  (atom nil))

(defn- was-called?
  "Whether or not the given function was called with the given values."
  [[fn-var values]]
  (if values
    (some #{[fn-var values]} (:calls @context))
    (some #(= (first %) fn-var) (:calls @context))))

(defn with-mocks-fn
  "Calls the given function inside of a mock context."
  [body-fn]
  (when @context
    (throw (IllegalStateException. "Cannot nest mocks")))
  (try
    ;; create new context
    (reset! context {:originals {}
                     :mocks {}
                     :calls []})
    ;; call the body
    (body-fn)
    ;; check that all mocks were called
    (when-let [uncalled (->> (:mocks @context)
                             (mapcat (fn [[fn-var values]]
                                       (if (fn? values)
                                         [[fn-var]]
                                         (map #(vector fn-var %)
                                              (keys values)))))
                             (remove was-called?)
                             seq)]
      (test/do-report {:type :fail
                       :expected (vec (sort uncalled))
                       :message "Some mocks were not called."}))
    (finally
      ;; restore all originals
      (run! (fn [[fn-var fn]]
              (alter-var-root fn-var (constantly fn)))
            (:originals @context))
      ;; wipe context
      (reset! context nil))))

(defmacro with-mocks
  "A macro which establishes a lexical scope for mock applicability."
  [& body]
  `(with-mocks-fn (fn [] ~@body)))

(defn verify-call-order
  "Verifies the given mocked functions were called in the given order."
  [& fn-vars]
  (when-not @context
    (throw (IllegalStateException.
            "can't verify mocks outside of with-mocks")))
  (when-let [missing (seq (set/difference (set fn-vars)
                                          (set (keys (:mocks @context)))))]
    (throw (IllegalArgumentException. (str "Some functions not mocks: "
                                           missing))))
  (let [called (filter (set fn-vars) (map first (:calls @context)))]
    (when-not (= fn-vars called)
      (test/do-report {:type :fail
                       :expected (vec fn-vars)
                       :actual (vec called)
                       :message "Mocks were called out of order."}))))

(defn- mock-fn
  "Returns the mock function for the given function and values."
  [fn-var values]
  (fn [& args]
    (if (= values :never)
      (test/do-report {:type :fail
                       :message (str "Unexpected call of " fn-var)})
      (let [args (or args [])]           ; handle empty args
        (swap! context update-in [:calls] conj [fn-var args])
        (if (fn? values)
          (apply values args)
          (if (contains? values args)
            (get values args)
            (test/do-report {:type :fail
                             :expected values
                             :actual (vec args)
                             :message (str "Unexpected arguments for "
                                           fn-var)})))))))

(defn mock!
  "Mocks the given function. Takes either a map of function arguments to
  function return values, an actual function, or `:never` if the function should
  never be called.."
  [fn-var values]
  (when-not @context
    (throw (IllegalStateException. "can't mock outside of with-mocks")))

  ;; save original fn
  (when-not (contains? (:originals @context) fn-var)
    (swap! context assoc-in [:originals fn-var] @fn-var))

  (let [m (mock-fn fn-var values)]
    (when-not (= values :never)
      (swap! context assoc-in [:mocks fn-var] values))
    (alter-var-root fn-var (constantly m))))

(defn stub!
  "Stubs the given function or var, which is to say it replaces it without
  expectation of being called."
  [fn-var value]
  (when-not @context
    (throw (IllegalStateException. "can't stub outside of with-mocks")))

  (when-not (contains? (:originals @context) fn-var)
    (swap! context assoc-in [:originals fn-var] @fn-var))

  (alter-var-root fn-var (constantly value)))
