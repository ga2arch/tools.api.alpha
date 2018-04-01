(ns core
  (:require [clojure.tools.reader :as edn]
            [clojure.java.io :as jio]
            [clojure.spec.alpha :as s]
            [selmer.parser :as t]
            [selmer.filters :refer [add-filter!]]
            [clojure.walk :refer [postwalk prewalk]]
            [camel-snake-kebab.core :refer :all]
            [clojure.spec.gen.alpha :as gen]))

;; global
(add-filter! :upperHead
             (fn [x]
               [:safe (str (clojure.string/upper-case (first x))
                           (subs x 1))]))

(selmer.util/turn-off-escaping!)

;; specs
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

;; api loader / normalizer

(defn normalize-param
  "Transform parameter in the form {uid [:type/uuid \"description\"]} into
  {:name \"uid\"
   :type :type/uuid
   :description \"description\"}"
  [[param-name param-details]]
  (cond
    (vector? param-details)
    {:name        (name param-name)
     :type        (first param-details)
     :description (second param-details)
     :required    (boolean ((into #{} param-details) :required))}

    (keyword? param-details)
    {:name     (name param-name)
     :type     param-details
     :required false}))

(defn normalize-params
  [params]
  (mapv normalize-param params))

(defn get-type
  "Retrieve a custom type from the types map"
  [types t]
  (let [k (keyword (name t))]
    (get types k)))

(defn make-type
  "Concrete-ize a generic type by substituting the generic param"
  [types [t & args]]
  (let [k (keyword (name t))
        type (get-type types k)
        args (for [arg args]
               (cond
                 (keyword? arg)
                 (get-type types arg)
                 :default arg))
        symbols (zipmap (:args type) args)]
    (postwalk #(or (get symbols %) %) type)))

(defn normalize-type
  "Transform type in the form:
   * [] type + description
   * () generic type building,
   * {} inline type declaration
   into
   {:type {:name ...
           :kind ...
           :schema ...}
    :description ..."
  [{types :types} body]
  (cond
    (vector? body)
    (let [[t desc] body]
      (cond
        (seq? t)
        {:type        (make-type types t)
         :description desc}

        (map? t)
        {:type        {:name   (ffirst t)
                       :kind   :concrete
                       :schema (second (first t))}
         :description desc}

        (keyword? t)
        {:type        (get-type types t)
         :description desc}))

    (seq? body)
    {:type (make-type types body)}

    (keyword? body)
    {:type (get-type types body)}))

(defn update-values [m f]
  (reduce (fn [r [k v]] (assoc r k (f v))) {} m))

(defn normalize-types
  "Transform the types in the types map in the form:
  * {:Name {..}} type declaration
  * {(:Name Sym) {..}} generic type declaration
  into
  {:Name {:name :Name
          :kind :concrete|:generic
          :args () (if generic)
          :schema {..}}"
  [types]
  (into
    {}
    (for [[k v] types]
      (cond
        (keyword? k)
        [k {:name   k
            :kind   :concrete
            :schema v}]

        (seq? k)
        [(first k) {:name   (first k)
                    :kind   :generic
                    :args   (rest k)
                    :schema v}]))))

(defn normalize-return
  [api return]
  (update-values return (partial normalize-type api)))

(defn normalize-endpoint
  "Transform endpoint in the form {[:GET \"/api/\"] {..}}
   into
   {:method :GET
    :path   \"/api\"
    ...}
    normalizing params, body and return"
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
  "Transform context into set of routes by prepending the context base path
   to all the sub routes paths"
  [api [_ version base-path] v]
  (if-let [routes (:routes v)]
    (let [path-params (:path-params v)]
      (for [[[method path] details] routes]
        (normalize-endpoint
          api
          [method (str base-path path)]
          (update details :path-params #(merge path-params %)))))))

(defn normalize-routes
  "Transform context into routes and normalize routes"
  [api routes]
  (let [routes (for [[k v] routes]
                 (case (first k)
                   :context (normalize-context api k v)
                   (normalize-endpoint api k v)))]
    (->> routes
         (filter (comp not nil?))
         flatten
         (into []))))

(defn resolve-include
  [m include]
  (cond
    (string? include)                                       ;; "file.edn"
    (merge m (edn/read-string (slurp include)))

    (seq? include)                                          ;; ("file.edn" "arg1")
    (let [data (edn/read-string (slurp (first include)))]
      (if-let [args (:signature data)]
        (let [data (dissoc data :signature)
              arg->value (zipmap args (rest include))]
          (merge m (postwalk #(or (get arg->value %) %) data)))
        data))

    (vector? include)                                       ;; ["file.edn"]
    (loop [m m
           include include]
      (if (empty? include)
        m
        (recur (merge m (resolve-include m (first include)))
               (rest include))))))

(defn resolve-includes
  [api]
  (prewalk (fn [x]
             (if-let [include (:include x)]
               (resolve-include x include)
               x)) api))

(defn load-api
  [path]
  (let [api (edn/read-string (slurp path))
        api (resolve-includes api)
        api (update api :types normalize-types)
        api (update api :routes (partial normalize-routes api))]
    api))

;; generators

(defn type->java [type]
  (cond
    (vector? type)
    (str "List<" (type->java (first type)) ">")

    (map? type)
    (case (:kind type)
      :generic (format "%s<%s>" (->PascalCase (name (:name type))) (apply str (mapv type->java (:args type))))
      :concrete (->PascalCase (name (:name type))))

    (keyword? type)
    (case type
      :type/string "String"
      :type/integer "Integer"
      :type/int "int"
      :type/void "void"
      :type/uuid "UUID"
      :type/date "Date"
      :type/boolean "Boolean")))

;; java beans

(defn schema->fields
  [schema]
  (for [[field type] schema]
    {:name (->camelCase (name field))
     :type (type->java type)}))

(defn type->bean
  [type]
  (t/render (slurp "resources/templates/bean")
            {:name   (type->java type)
             :ctor   (->PascalCase (name (:name type)))
             :fields (schema->fields (:schema type))}))

(defn route->bean
  [{:keys [query-params return body]}]
  (let [query (filter map? (map :type query-params))
        return (filter map? (map :type (vals return)))
        body (filter map? (vec (:type body)))]
    (distinct (concat query return body))))

(defn generate-beans
  [api]
  (mapv type->bean (distinct (mapcat route->bean (:routes api)))))

;; retrofit

(defn prepare-params-retrofit
  [{:keys [query-params path-params method body]}]
  (let [params (let [path (mapv #(format "@Path(\"%s\") %s %s" (:name %) (type->java (:type %)) (->camelCase (:name %))) path-params)
                     query (mapv #(format "@Query(\"%s\") %s %s" (:name %) (type->java (:type %)) (->camelCase (:name %))) query-params)]
                 (apply str (interpose ", " (concat path query))))]
    (case method
      (:POST :PUT :PATCH) (str params ", "
                               (format "@Body %s %s"
                                       (type->java (:type body))
                                       (->camelCase (name (get-in body [:type :name])))))
      params)))

(defn prepare-route-retrofit
  [route]
  (let [config {:method (name (:method route))
                :params (prepare-params-retrofit route)
                :path   (:path route)
                :return "Call<_>"}
        config (if-let [{:keys [retrofit]} (:plugins route)]
                 (cond-> config
                         (some? (:name retrofit)) (assoc :name (:name retrofit))
                         (some? (:return retrofit)) (assoc :return (:return retrofit)))
                 config)
        return (->>
                 (into [] (:return route))
                 (filter #(< (first %) 300))
                 (sort-by first)
                 first
                 second
                 :type)]
    (update config :return #(clojure.string/replace % "_" (type->java return)))))

(defn generate-retrofit
  [{:keys [service routes] :as api}]
  (let [config {:service (->PascalCase service)
                :routes  (mapv prepare-route-retrofit routes)}]
    (t/render (slurp "resources/templates/retrofit") config)))

