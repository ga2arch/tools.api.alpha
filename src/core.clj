(ns core
  (:require [clojure.tools.reader :as edn]
            [clojure.java.io :as jio]
            [clojure.spec.alpha :as s]
            [selmer.parser :as t]
            [selmer.filters :refer [add-filter!]]
            [clojure.walk :refer [postwalk prewalk]]
            [camel-snake-kebab.core :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [expound.alpha :as expound]
            [yaml.core :as yaml])
  (:import (clojure.lang PersistentVector IPersistentMap Keyword)))

;; global
(add-filter! :upperHead
             (fn [x]
               [:safe (str (clojure.string/upper-case (first x))
                           (subs x 1))]))

(selmer.util/turn-off-escaping!)

(defn deep-merge
  "Recursively merges maps. If value are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

;; specs
(s/def ::schema-version string?)
(s/def ::service string?)
(s/def ::test string?)
(s/def ::stag string?)
(s/def ::prod string?)
(s/def ::base-path (s/keys :req-un [::test ::stag ::prod]))
(s/def ::description string?)

(def simple-types #{:type/string
                    :type/integer
                    :type/int
                    :type/void
                    :type/uuid
                    :type/date
                    :type/boolean})

(s/def :type/name symbol?)
(s/def :type/kind #{:generic :concrete})
(s/def :type/schema (s/map-of symbol? (s/or :simple simple-types
                                            :list vector?
                                            :generic symbol?)))

(s/def ::type (s/keys :req-un [:type/name :type/kind :type/schema]))
(s/def ::types (s/map-of symbol? ::type))

(s/def :route/description string?)
(s/def :route/path string?)
(s/def :route/return.type (s/keys :req-un [::type ::description]))
(s/def :route/return (s/map-of int? :route/return.type))
(s/def :route/method #{:GET :POST :PATCH :PUT :DELETE :OPTIONS})
(s/def :route/produces (s/coll-of #{:application/json}))
(s/def :route/consumers (s/coll-of #{:application/json}))

(s/def :route.query-param/name symbol?)
(s/def :route.query-param/type (s/or :simple simple-types
                                     :list vector?
                                     :generic symbol?))

(s/def :route/query-params (s/coll-of (s/keys :req-un [:route.query-param/name
                                                       :route.query-param/type]
                                              :opt-un [::description])))

(s/def ::route (s/keys :req-un [:route/path
                                :route/query-params]))

(s/def ::routes (s/coll-of ::route))
(s/def ::api (s/keys :req-un [::schema-version
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
    {:name        param-name
     :type        (first param-details)
     :description (second param-details)
     :required    (boolean ((into #{} param-details) :required))}

    (keyword? param-details)
    {:name     param-name
     :type     param-details
     :required false}))

(defn normalize-params
  [params]
  (mapv normalize-param params))

(defn make-type
  "Concrete-ize a generic type by substituting the generic param"
  [types [t & args]]
  (let [type (get types t)
        args (for [arg args]
               (cond
                 (symbol? arg)
                 (get types arg)
                 :default arg))
        symbols (zipmap (:args type) args)]
    (postwalk #(or (get symbols %) %) type)))

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
  (->> (for [[k v] types]
         (cond
           (or (symbol? k) (keyword? k))
           [k {:name   k
               :kind   :concrete
               :schema v}]

           (seq? k)
           [(first k) {:name   (first k)
                       :kind   :generic
                       :args   (rest k)
                       :schema v}]))
       (into {})))

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

        (symbol? t)
        {:type        (get types t)
         :description desc}))

    (seq? body)
    {:type (make-type types body)}

    (map? body)
    (if (seq? (ffirst body))
      {:type (make-type (merge types (normalize-types body)) (ffirst body))}
      {:type {:name   (ffirst body)
              :kind   :concrete
              :schema (second (first body))}})

    (symbol? body)
    {:type (get types body)}))

(defn update-values [m f]
  (reduce (fn [r [k v]] (assoc r k (f v))) {} m))

(defn normalize-return
  [api return]
  (for [[code v] return]
    (assoc (normalize-type api v) :code code)))

(defn normalize-endpoint
  "Transform endpoint in the form {[:GET \"/api/\"] {..}}
   into
   {:method :GET
    :path   \"/api\"
    ...}
    normalizing params, body and return"
  [api [method path] route]
  (merge
    route
    {:method       method
     :path         path
     :path-params  (normalize-params (:path-params route))
     :query-params (normalize-params (:query-params route))
     :body         (normalize-type api (:body route))
     :return       (normalize-return api (:return route))}))

(defn normalize-context
  "Transform context into set of routes by prepending the context base path
   to all the sub routes paths"
  [api [_ version base-path] context]
  (if-let [routes (:routes context)]
    (for [[[method path] route] routes]
      (normalize-endpoint
        api
        [method (str "/" (name version) base-path path)]
        (-> (deep-merge (dissoc context :include :routes) route)
            (assoc :version version
                   :context base-path))))))

(defn normalize-routes
  "Transform context into routes and normalize routes"
  [api routes]
  (let [dups (fn [seq]
               (for [[id freq] (frequencies seq)
                     :when (> freq 1)]
                 id))
        routes (->> (for [[k v] routes]
                      (case (first k)
                        :context (normalize-context api k v)
                        (normalize-endpoint api k v)))
                    (filter some?)
                    flatten)]
    (if-let [dup (seq (dups (map (juxt :method :path) routes)))]
      (throw (ex-info "duplicate route" {:route dup}))
      routes)))

(defn resolve-include
  [m include]
  (cond
    (string? include)                                       ;; "file.edn"
    (deep-merge m (edn/read-string (slurp include)))

    (seq? include)                                          ;; ("file.edn" "arg1")
    (let [data (edn/read-string (slurp (first include)))]
      (if-let [args (:signature data)]
        (let [data (dissoc data :signature)
              arg->value (zipmap args (rest include))]
          (deep-merge m (postwalk #(or (get arg->value %) %) data)))
        data))

    :else
    m))

(defn resolve-includes
  [api]
  (prewalk (fn [x]
             (if-let [include (:include x)]
               (if (vector? include)
                 (apply merge (map (partial resolve-include x) include))
                 (resolve-include x include))
               x)) api))

(defn load-api
  [path]
  (let [api (-> (edn/read-string (slurp path))
                resolve-includes
                (update :types normalize-types)
                (#(update % :routes (partial normalize-routes %))))]
    (if (s/valid? ::api api)
      api
      (expound/expound ::api api))))

;; generators

(def convert-type nil)
(defmulti convert-type (fn [generator type] (println type) [generator (class type) (cond
                                                                                     (vector? type)
                                                                                     nil
                                                                                     (keyword? type)
                                                                                     type
                                                                                     (map? type)
                                                                                     (:kind type))]))
(defmethod convert-type [:java PersistentVector nil] [_ type]
  (str "List<" (convert-type :java (first type)) ">"))
(defmethod convert-type [:java IPersistentMap :generic] [_ type]
  (format "%s<%s>" (->PascalCase (name (:name type))) (apply str (mapv (partial convert-type :java) (:args type)))))
(defmethod convert-type [:java IPersistentMap :concrete] [_ type]
  (->PascalCase (name (:name type))))
(defmethod convert-type [:java Keyword :type/string] [_ _] "String")
(defmethod convert-type [:java Keyword :type/integer] [_ _] "Integer")
(defmethod convert-type [:java Keyword :type/void] [_ _] "void")
(defmethod convert-type [:java Keyword :type/uuid] [_ _] "UUID")
(defmethod convert-type [:java Keyword :type/date] [_ _] "Date")
(defmethod convert-type [:java Keyword :type/boolean] [_ _] "Boolean")

;; java beans

(defn type->java [type]
  (convert-type :java type))

(defn schema->fields
  [schema]
  (for [[field type] schema]
    {:name (->camelCase (name field))
     :type (convert-type :java type)}))

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

(defn prepare-path-params-retrofit
  [path-params]
  (mapv #(format "@Path(\"%s\") %s %s"
                 (:name %)
                 (type->java (:type %))
                 (->camelCase (:name %))) path-params))

(defn prepare-query-params-retrofit
  [query-params]
  (mapv #(format "@Query(\"%s\") %s %s"
                 (:name %)
                 (type->java (:type %))
                 (->camelCase (:name %))) query-params))

(defn prepare-body-retrofit
  [{:keys [type]}]
  (format "@Body %s %s"
          (type->java type)
          (->camelCase (name (:name type)))))

(defn prepare-params-retrofit
  [{:keys [query-params path-params method body]}]
  (let [params (let [path (prepare-path-params-retrofit path-params)
                     query (prepare-query-params-retrofit query-params)]
                 (apply str (interpose ", " (concat path query))))]
    (case method
      (:POST :PUT :PATCH) (str params ", " (prepare-body-retrofit body))
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
        return (->> (:return route)
                    (filter #(< (:code %) 300))             ;; filter by 200 return codes
                    (sort-by :code)                         ;; sort by return code
                    first
                    :type)]
    (update config :return #(clojure.string/replace % "_" (type->java return)))))

(defn generate-retrofit
  [{:keys [service routes] :as api}]
  (let [config {:service (->PascalCase service)
                :routes  (mapv prepare-route-retrofit routes)}]
    (t/render (slurp "resources/templates/retrofit") config)))

;; raml

(defmethod convert-type [:raml PersistentVector nil] [_ type]
  (str (convert-type :raml (first type)) "[]"))
(defmethod convert-type [:raml IPersistentMap :generic] [_ type]
  (->PascalCase (name (:name type))))

(defmethod convert-type [:raml IPersistentMap :concrete] [_ type]
  (->PascalCase (name (:name type))))

(defmethod convert-type [:raml Keyword :type/string] [_ _] "string")
(defmethod convert-type [:raml Keyword :type/integer] [_ _] "int")
(defmethod convert-type [:raml Keyword :type/void] [_ _] "void")
(defmethod convert-type [:raml Keyword :type/uuid] [_ _] "string")
(defmethod convert-type [:raml Keyword :type/date] [_ _] "date")
(defmethod convert-type [:raml Keyword :type/boolean] [_ _] "bool")

(defn type->raml
  [{:keys [kind schema] :as type}]
  (cond
    (keyword? type)
    (convert-type :raml type)
    :else
    {(name (:name type)) {:properties (into {} (mapv (fn [[k v]] [(name k) (convert-type :raml v)]) schema))}}))

(defn path-params->raml
  [path-params]
  (let [xf (map (fn [param]
                  [(name (:name param)) {:type (type->raml (:type param))
                                         :displayName (:description param)}]))]
    (into {} xf path-params)))

(defn route->raml
  [{:keys [path method query-params path-params]}]
  {path
   {(.toLowerCase (name method))
    {:uriParameters
     (path-params->raml path-params)}}})

(defn to-raml
  [{:keys [service base-path types routes]}]
  (let [data {:title   service
              :baseUri (:test base-path)
              :types   (apply merge (mapv (fn [[_ v]] (type->raml v)) types))
              :routes  (mapv route->raml routes)}]
    (yaml/generate-string data :dumper-options {:flow-style   :block
                                                :scalar-style :plain})))