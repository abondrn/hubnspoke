(ns semantic
  (:require [ajax.core :as ajax]
            [clojure.data.json :as json]
            [clojure.spec.alpha :as spec]
            [com.yetanalytics.flint :as f]
            [com.yetanalytics.flint.spec.query :as qs]
            [expound.alpha :as expound]))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn async-get [endpoint params & [then else]]
  (let [promise (promise)
        default-callback #(deliver promise %)
        then-callback (if then #(deliver promise (then %)) default-callback)
        else-callback (if else #(deliver promise (else %)) default-callback)]
    (ajax/GET endpoint (assoc params
                              :handler then-callback
                              :error-handler else-callback))
    promise))

(defn async-get-json [endpoint params & [then else]]
  (let [parse #(json/read-str % :key-fn keyword)
        then-callback (if then (comp then parse) parse)]
    (async-get endpoint (assoc params :response-format :text) then-callback else)))

;; TODO several of the downstream functions should probably have two
;; arities so one can do things like (entity :es "churro").
(def ^:dynamic *default-language* (atom :en))

(defn default-language
  "Returns the unwrapped value of the in-scope binding of `*default-language*`, which can be an atom containing a keyword representing the current default language (like `:en`) or an atom containing such a keyword."
  []
  (deref *default-language*))

(def prefixes
  "RDF prefixes automatically supported by the WikiData query service."
  {:bd "<http://www.bigdata.com/rdf#>"
   :cc "<http://creativecommons.org/ns#>"
   :dct "<http://purl.org/dc/terms/>"
   :geo "<http://www.opengis.net/ont/geosparql#>"
   :hint "<http://www.bigdata.com/queryHints#>"
   :ontolex "<http://www.w3.org/ns/lemon/ontolex#>"
   :owl "<http://www.w3.org/2002/07/owl#>"
   :p "<http://www.wikidata.org/prop/>"
   :pq "<http://www.wikidata.org/prop/qualifier/>"
   :pqn "<http://www.wikidata.org/prop/qualifier/value-normalized/>"
   :pqv "<http://www.wikidata.org/prop/qualifier/value/>"
   :pr "<http://www.wikidata.org/prop/reference/>"
   :prn "<http://www.wikidata.org/prop/reference/value-normalized/>"
   :prov "<http://www.w3.org/ns/prov#>"
   :prv "<http://www.wikidata.org/prop/reference/value/>"
   :ps "<http://www.wikidata.org/prop/statement/>"
   :psn "<http://www.wikidata.org/prop/statement/value-normalized/>"
   :psv "<http://www.wikidata.org/prop/statement/value/>"
   :rdf "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
   :rdfs "<http://www.w3.org/2000/01/rdf-schema#>"
   :schema "<http://schema.org/>"
   :skos "<http://www.w3.org/2004/02/skos/core#>"
   :wd "<http://www.wikidata.org/entity/>"
   :wdata "<http://www.wikidata.org/wiki/Special:EntityData/>"
   :wdno "<http://www.wikidata.org/prop/novalue/>"
   :wdref "<http://www.wikidata.org/reference/>"
   :wds "<http://www.wikidata.org/entity/statement/>"
   :wdt "<http://www.wikidata.org/prop/direct/>"
   :wdtn "<http://www.wikidata.org/prop/direct-normalized/>"
   :wdv "<http://www.wikidata.org/value/>"
   :wikibase "<http://wikiba.se/ontology#>"
   :xsd "<http://www.w3.org/2001/XMLSchema#>"})

(def uri->prefix
  "Reverse lookup table from RDF namespace url to prefix name. Used by `uri->keyword`."
  (reduce (fn [m [k v]]
            (assoc m
                   (subs v 1 (dec (count v)))
                   (str (name k))))
          {}
          prefixes))

(defn uri->keyword
  "Use regex `pattern` to extract the base and final portions of `uri` and convert them into a namespaced keyword. Returns nil if the pattern match is not successful."
  [pattern uri]
  (let [[_ base-uri trimmed-value] (re-find pattern uri)]
    (when-let [prefix (uri->prefix base-uri)]
      (when trimmed-value
        (keyword (uri->prefix base-uri) trimmed-value)))))

(defn clojurize-values
  "Convert the values in `result` to Clojure types."
  [result]
  (into {} (map (fn [[k {:keys [type value datatype] :as v}]]
                  [k (condp = type
                       "uri" (or (uri->keyword #"(.*#)(.*)$" value)
                                 (uri->keyword #"(.*/)([^/]*)$" value)
                                 (:value v))
                       "literal" (condp = datatype
                                   "http://www.w3.org/2001/XMLSchema#decimal" (Float/parseFloat value)
                                   "http://www.w3.org/2001/XMLSchema#integer" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#double" (Float/parseFloat value)
                                   "http://www.w3.org/2001/XMLSchema#float" (Float/parseFloat value)
                                   "http://www.w3.org/2001/XMLSchema#long" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#int" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#short" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#positiveInteger" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#unsignedLong" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#unsignedInt" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#unsignedShort" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" (Integer/parseInt value)
                                   "http://www.w3.org/2001/XMLSchema#negativeInteger" (Integer/parseInt value)
                                   ;"http://www.opengis.net/ont/geosparql#wktLiteral"
                                   ;"http://www.w3.org/2001/XMLSchema#dateTime" (tick/instant value)
                                   nil value ; no datatype, return literal as is
                                   v) ; unknown datatype, return whole value map
                       v)]) ; unknown value type, return whole value map
                result)))

(defn clean-up-symbols-and-seqs
  "Remove the namespace portion of namespaced symbols in `sparql-form`. We need to do this because clojure's backtick automatically interns symbols in the current namespace, but the SPARQL DSL wants unnamespaced symbols. Likewise, convert LazySeqs to proper Lists for the benefit of Flint's error checking."
  [sparql-form]
  (clojure.walk/postwalk
   (fn [e]
     (cond (symbol? e) (symbol (name e))
           (seq? e) (apply list e)
           :else e))
   sparql-form))

(defn do-query
  "Query the WikiData endpoint with the SPARQL query in `sparql-text` and convert the return into Clojure data structures."
  [sparql-text & [endpoint]]
  (-> endpoint
      (or "https://query.wikidata.org/sparql")
      (async-get-json {:params {:query sparql-text
                                :format :json}}
                      #(->> % :results :bindings (mapv clojurize-values)))
      deref))

(spec/check-asserts true)

(defn format-query [query]
  (binding [spec/*explain-out* expound/printer]
    (spec/assert qs/query-spec query))
  (f/format-query query))

;; TODO should label service be optional?
(defn query
  ([sparql-form]
   (query {} sparql-form))
  ([opts sparql-form]
   (-> sparql-form
       clean-up-symbols-and-seqs
       (update :prefixes merge prefixes)
       (update :where conj [:service :wikibase/label
                            [[:bd/serviceParam :wikibase/language (str "[AUTO_LANGUAGE]," (name (default-language)))]]])
       format-query
       do-query)))

(def entity*
  "Memoized implementation of language-aware entity lookup."
  (memoize
   (fn [lang label criteria]
     (-> `{:select [?item ?sitelinks]
           :where  [[?item :rdfs/label {~lang ~label}]
                    [?item :wikibase/sitelinks ?sitelinks]
                    ~@(mapv (fn [[p e]] `[?item ~p ~e])
                            (partition 2 criteria))
                    ;; no :instance-of / :subclass-of wikidata properties
                    ;; and no disambiguation pages
                    [:minus [[?item (cat (* :wdt/P31) (+ :wdt/P279)) :wd/Q18616576]
                             [?item :wdt/P31 :wd/Q4167410]]]]
           ;; choose the entity with the most sitelinks on Wikipedia 
           :order-by [(desc ?sitelinks)]
           :limit 1}
         query
         first
         :item))))

(defn entity
  "Return a keyword like :wd/Q42 for the most popular WikiData entity that matches `label`."
  [label & criteria]
  (let [[lang label'] (if (map? label)
                        (first label)
                        [(default-language) label])]
    (entity* lang label' criteria)))

(defn wdt->wd [arc]
  (let [prefix (namespace arc)
        id (name arc)]
    (if (or (#{"p" "ps" "wdt"} prefix) (nil? prefix))
      (keyword "wd" id)
      arc)))

(def label*
  "Memoized implementation of language-aware label lookup."
  (memoize
   (fn [lang id]
     (let [rdfs-label (->> `{:select [?label]
                             :where  [[~(wdt->wd id) :rdfs/label ?label]
                                      [:filter (= (lang ?label) ~(name lang))]]}
                           query
                           first
                           :label)]
       (if rdfs-label rdfs-label id)))))

(defn label
  "Returns the label of the entity with `id`. If `lang` is specified it, overrides `*default-language*`."
  ([id] (label* (default-language) id))
  ([lang id] (label* lang id)))

(def describe*
  "Memoized implementation of language-aware description lookup."
  (memoize
   (fn [lang id]
     (->> `{:select [?description]
            :where [[~(wdt->wd id) :schema/description ?description]
                    [:filter (= (lang ?description) ~(name lang))]]}
          query
          first
          :description))))

(defn describe
  "Returns the description of the entity with `id`. If `lang` is specified it, overrides `*default-language*`."
  ([id] (describe* (default-language) id))
  ([lang id] (describe* lang id)))

(defn search
  ([text]
   (search (default-language) text))
  ([lang text]
   (->> (-> "https://www.wikidata.org/w/api.php"
            (async-get-json
             {:params {:action "wbsearchentities"
                       :search text
                       :language (name lang)
                       :uselang (name lang)
                       :type "item"
                       :format "json"}})
            deref
            :search)
        (map (fn [{:keys [:description :id :display] :as vs}]
               {:id (keyword "wd" id)
                :description description
                :label (get-in display [:label :value])})))))

(defn apply-map [m kw-functions]
  (into m
        (for [[k f] kw-functions
              :let [v (m k)]
              :when (some? v)]
          [k (f v)])))

;; gets everything for the entity; :claims key has the properties
(def entity-data*
  (memoize (fn [item-id]
             (-> (str "https://www.wikidata.org/wiki/Special:EntityData/" item-id ".json")
                 (async-get-json {:query-params {:format "json"}})
                 deref
                 :entities
                 ((keyword item-id))))))

(defn clojurize-claim
  "Convert the values in `result` to Clojure types."
  [{:keys [datatype datavalue] :as snak}]
  (let [simple (get datavalue :value nil)]
    (condp = datatype
      "monolingualtext" {(-> datavalue :value :language keyword) (-> datavalue :value :text)}
      "wikibase-entityid" (keyword (str "wd/" (-> datavalue :id)))
      "wikibase-item" (keyword (str "wd/" (-> datavalue :value :id)))
      "wikibase-property" (keyword (str "wdt/" (-> datavalue :value :id)))
      "commonsMedia" simple
      "string" simple
      "external-id" simple
      "time" simple
      "url" simple
      "quantity" {:amount (-> datavalue :value :amount)
                  :units (-> datavalue :value :unit)}
      [datatype datavalue])))

(defn claims->clj [claims]
  (into {}
        (for [[p values] claims]
          [p
           (for [v values
                 :let [ms (v :mainsnak)]]
             {:type (ms :datatype)
              :value (clojurize-claim ms)
              :hash (ms :hash)
              :rank (v :rank)})])))

(defn entity-data [item & [lang]]
  (let [lang' (or lang (default-language))
        full (entity-data* (name item))
        get-lang #(and (some? %) (-> % lang' :value))]
    (apply-map full {:descriptions get-lang
                     :labels get-lang
                     :aliases #(map :value (% lang'))
                     :claims claims->clj
                     ;:modified 
                     :sitelinks #(% (keyword (str (name lang') "wiki")))})))

(defn filter-claims [claims & {vpred :values ppred :properties}]
  (into {}
        (for [[p values] claims
              :let [filtered-values
                    (for [v values
                          :when (and vpred (vpred v))]
                      v)]
              :when (and ppred (ppred p filtered-values))]
          [p filtered-values])))

;; helpful query construction utilities

(def instance-of :wdt/P31)
(def subclass-of :wdt/P279)
(def kind-of `(cat ~instance-of (* ~subclass-of)))
(def has-label '(alt :rdfs/label :skos/altLabel :schema/alternateName))

; TODO pass custom filters
(defn search-properties-by-label
  "Find properties which contain a string in their label"
  [label]
  (->> `{:select-distinct [?property ?label]
         :where [[?property :a :wikibase/Property]
                 [?property ~has-label ?label]
                 [:filter (contains ?label ~label)]]}
       query
       distinct
       (group-by :property)
       (map (fn [[p pls]] [p (map :label pls)]))
       (into {})))

(defn get-property
  "Returns the unique property which has a given label, or false"
  [label]
  (let [res (search-properties-by-label label)
        unique? (= 1 (count res))
        first-prop (first (keys res))]
    (and unique? (keyword "wdt" (name first-prop)))))

(defn coalesce-options
  "Allows matching against a tuple, where each part is either a single value or a list of options"
  [s v o]
  (let [[subs? preds? objs?] (map sequential? [s v o])
        pred (if preds? (cons 'alt v) v)
        sub (if subs? (first s) s)
        obj (if objs? (first o) o)]
    (filter some?
            [(if subs? [:values {sub (rest s)}])
             (if objs? [:values {obj (rest o)}])
             [sub pred obj]])))

#_(defn clojurize-claims [e-data]
    (reduce
     (fn [m [prop snaks]]
       (assoc m (wdt->readable (->> prop name (str "wdt/") keyword))
              (mapv (comp clojurize-claim :mainsnak) snaks)))
     {}
     (:claims e-data)))