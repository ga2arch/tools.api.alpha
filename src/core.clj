(ns core
  (:require [clojure.tools.reader :as edn]
            [clojure.java.io :as jio]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [postwalk]]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::schema-version string?)
(s/def ::service string?)
(s/def ::test string?)
(s/def ::stag string?)
(s/def ::prod string?)
(s/def ::base-path (s/keys :req-un [::test ::stag ::prod]))
(s/def ::description string?)

(s/def ::types-name
  (s/or
    :kind-normal keyword?
    :kind-generic (s/cat :type keyword?
                         :symbols (s/+ symbol?))))

(s/def ::types-schema
  (s/map-of keyword? (s/or
                       :type keyword?
                       :symbol symbol?
                       :list (s/coll-of (s/or :type keyword?
                                              :symbol symbol?)))))

(s/def ::types (s/map-of ::types-name ::types-schema))
(s/def ::routes-context
  (s/cat :context #{:context}
         :version keyword?
         :path string?))

(s/def ::routes
  (s/map-of (s/or :context ::routes-context
                  :endpoint ::routes-endpoint)
            map?))

(s/def ::schema (s/keys :req-un [::schema-version
                                 ::service
                                 ::base-path
                                 ::types
                                 ::routes]
                        :opt-un [::description]))

(defn normalize-param
  [[k v]]
  (cond
    (vector? v)
    {:name        (name k)
     :type        (first v)
     :description (second v)
     :required    (boolean ((into #{} v) :required))}

    (keyword? v)
    {:name     (name k)
     :type     v
     :required false}))

(defn normalize-params
  [params]
  (mapv normalize-param params))

(defn get-type
  [types t]
  (let [k (keyword (name t))]
    (get types k)))

(defn make-type
  [types [t & args]]
  (let [k (keyword (name t))
        type (get-type types k)
        symbols (zipmap (:args type) args)]
    (postwalk #(or (get symbols %) %) type)))

(defn normalize-type
  [{types :types} body]
  (cond
    (vector? body)
    (let [[t desc] body]
      (cond
        (seq? t)
        {:type        (make-type types t)
         :description desc}

        (map? t)
        {:type        {:name   (name (ffirst t))
                       :kind   :concrete
                       :schema (second (first t))}
         :description desc}

        (keyword? t)
        {:type        (get-type types body)
         :description desc}))

    (seq? body)
    {:type (make-type types body)}

    (keyword? body)
    {:type (get-type types body)}))

(defn update-values [m f]
  (reduce (fn [r [k v]] (assoc r k (f v))) {} m))

(defn normalize-types
  [types]
  (into
    {}
    (for [[k v] types]
      (cond
        (keyword? k)
        [k {:name   (name k)
            :kind   :concrete
            :schema v}]

        (seq? k)
        [(first k) {:name   (name (first k))
                    :kind   :generic
                    :args   (rest k)
                    :schema v}]))))

(defn normalize-return
  [api return]
  (update-values return (partial normalize-type api)))

(defn normalize-endpoint
  [api [method path] v]
  (merge
    v
    {:method       method
     :path         path
     :path-params  (normalize-params (:path-params v))
     :query-params (normalize-params (:query-params v))
     :body         (normalize-type api (:body v))
     :return       (normalize-return api (:return v))}))

(defn normalize-context
  [api [_ version base-path] v]
  (if-let [routes (:routes v)]
    ;; if the context has sub routes
    (let [path-params (:path-params v)]
      (for [[[method path] details] routes]
        (normalize-endpoint
          api
          [method (str base-path path)]
          (update details :path-params #(merge path-params %)))))))

(defn normalize-routes
  [api routes]
  (let [routes (for [[k v] routes]
                 (case (first k)
                   :context (normalize-context api k v)
                   (normalize-endpoint api k v)))]
    (->> routes
         (filter (comp not nil?))
         flatten
         (into []))))

(defn load-api
  [path]
  (let [api (edn/read-string (slurp path))
        api (update api :types normalize-types)
        api (update api :routes (partial normalize-routes api))]
    api))

