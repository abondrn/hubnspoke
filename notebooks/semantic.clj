;; # Semantic Queries

;; Clerk can be very helpful when exploring any kind of data,
;; including the sorts of things for which we might turn to the
;; Semantic Web. To give a sense of what that's like, this notebook
;; gives some examples of querying WikiData for facts about the world.

(ns semantic
  {:nextjournal.clerk/visibility {:code :fold :result :hide}
   :nextjournal.clerk/error-on-missing-vars :off}
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [arrowic.core :as arr] 
            [semantic :as s]))

{:nextjournal.clerk/visibility {:code :show :result :show}}

(s/search "James")

{:nextjournal.clerk/error-on-missing-vars :off}

(defmacro def+apply "Macro to create a definition then apply various functions on it"
  [var val & fns]
  `(do
     (def ~var ~val)
     ~@(for [f (butlast fns)]
         (println `(~f ~var) "->" (f var)))
     (-> ~var ~(last fns))))

(def+apply jcm (s/entity "James Clerk Maxwell") s/entity-data)

(def+apply invented-or-discovered-by :wdt/P61 s/entity-data)

;; Now we can ask questions, like "what is James Clerk Maxwell famous
;; for having invented or discovered?"

(s/query `{:select [?what]
           :where  [[?what ~invented-or-discovered-by ~jcm]]})

;; The WikiData internal ID `:wd/Q1080745` doesn't immediately mean much
;; to a human, so we'll try again by appending `Label` to the end of
;; the `?what` logic variable so we can see a human readable label
;; that item:

(s/query `{:select [?whatLabel]
           :where  [[?what ~invented-or-discovered-by ~jcm]]})

;; Ah, better. 😊 This ceremony is required because WikiData uses a
;; language-neutral data representation internally, leaving us with an
;; extra step to get readable results. This can be a little annoying,
;; but it does have benefits. For example, we can ask for an entity's
;; label in every language for which it has been specified in
;; WikiData:

(s/query `{:select [?what ?label]
           :where  [[?what ~invented-or-discovered-by ~jcm]
                    [?what :rdfs/label ?label]]})

;; One of the nice things about data encoded as a knowledge graph is
;; that we can ask questions that are difficult to pose any other way,
;; then receive answers as structured data for further processing.

;; Here, for instance, is a query asking for things discovered or
;; invented by anyone who has as one of their occupations "physicist":

(s/search-properties-by-label "occupation")

(def has-occupation :wdt/P106)

(def inventions-and-discoveries
  (s/query `{:select [?whatLabel ?whomLabel]
             :where  [[?what ~invented-or-discovered-by ?whom]
                      [?whom ~has-occupation ~(s/entity "physicist")]]
             :limit 500}))

;; ## Tabular data

;; It's great that we can retrieve this information as a sequence of
;; maps that we can explore interactively in Clerk, but sometimes it's
;; more pleasant to display data organized in a table view:

(clerk/table inventions-and-discoveries)

;; Once we see how a given table looks, we might decide that it would
;; be better if, for example, these inventions were grouped by
;; inventor. This is just the sort of thing that Clojure sequence
;; functions can help us do:

(clerk/table (->> inventions-and-discoveries
                  (group-by :whomLabel)
                  (mapv (fn [[whom whats]] [whom (apply str (interpose " ; " (map :whatLabel whats)))]))))

;; ## Geospatial data

;; Some data are more naturally viewed in other ways, of course. In
;; this example we find every instance of any subclass of "human
;; settlement" (village, town, city, and so on) in Germany that has a
;; German language placename ending in _-ow_ or _-itz_, both of which
;; indicate that it was originally named by speakers of a Slavic
;; language.

(s/search-properties-by-label "instance of")

(s/search-properties-by-label "subclass of")

(s/search-properties-by-label "coordinate location")

(s/search-properties-by-label "country")

(def slavic-place-names
  (let [in-country '(alt :wdt/P495 :wdt/P17 :wdt/P3005)
        has-coords ':wdt/P625]
    (->> `{:select *
           :where [{?ort {~s/kind-of #{~(s/entity "human settlement")}
                          ~in-country #{~(s/entity "Germany")}
                          :rdfs/label #{?name}
                          ~has-coords #{?lonlat}}}
                   [:filter (= (lang ?name) "de")]
                   [:filter (regex ?name "(ow|itz)$")]]
           :limit 1000}
         query
       ;; cleanup lon-lat formatting for map plot!
         (mapv #(let [[lon lat] (-> %
                                    :lonlat
                                    :value
                                    (str/replace #"[^0-9 \.]" "")
                                    (str/split #" "))]
                  {:name (:name %) :latitude lat :longitude lon})))))

;; The `:coordinate-location` in this query is the longitude/latitude
;; position of each of these places in a somewhat unfortunate string
;; fomat. The `mapv` at the end converts these `lonlat` strings into
;; key/value pairs so Vega can plot the points on a map. This gives us
;; a very clear picture of which parts of Germany were Slavic prior to
;; the Germanic migrations:
  
(v/vl {:width 650 :height 650
       :config {:projection {:type "mercator" :center [10.4515 51.1657]}}
       :layer [{:data {:url "https://raw.githubusercontent.com/deldersveld/topojson/master/countries/germany/germany-regions.json"
                       :format {:type "topojson" :feature "DEU_adm2"}}
                :mark {:type "geoshape" :fill "lightgray" :stroke "white"}}
               {:encoding {:longitude {:field "longitude" :type "quantitative"}
                           :latitude {:field "latitude" :type "quantitative"}}
                :mark "circle"
                :data {:values slavic-place-names}}]})

;; Sometimes the data needs a more customized view. Happily, we can
;; write arbitrary hiccup to be rendered in Clerk. We'll use this
;; query to fetch a list of different species of _Apodiformes_ (swifts
;; and hummingbirds), returning a name, image, and map of home range
;; for each one.

(s/search-properties-by-label "image")

(let [has-parent-taxon (s/get-property "parent taxon")
      subtaxon `(* ~has-parent-taxon)
      has-taxon-rank (s/get-property "taxon rank")
      is-species [has-taxon-rank (s/entity "species")]
      has-range-map (s/get-property "taxon range map image")
      has-image '(alt :wdt/P18 :wdt/P242)]
  [has-parent-taxon has-taxon-rank has-range-map]
  (->> (s/query `{:select-distinct [?item ?itemLabel ?pic ?range]
                  :where [[?item ~subtaxon ~(s/entity "Apodiformes")]
                          [?item ~@is-species]
                          [?item :rdfs/label ?englishName]
                          [?item ~has-image ?pic]
                          [?item ~has-range-map ?range]
                          [:filter (= (lang ?englishName) "en")]]
                  :limit 11})
       (mapv #(vector :tr
                      [:td.w-32 (:itemLabel %)]
                      [:td [:img.w-80 {:src (:pic %)}]]
                      [:td [:img.w-80 {:src (:range %)}]]))
       (into [:table])
       clerk/html))

;; ## Network diagrams

;; Another useful technique when dealing with semantic or graph-shaped
;; data is to visualize the results as a tree. Here we gather all the
;; languages influenced by Lisp or by languages influenced by Lisp (a
;; transitive query across the graph), and visualize them in a big
;; network diagram.

;; Because Clerk's `html` viewer also understands SVGs, we can just
;; plug in an existing graph visualization library and send the output
;; to Clerk.

;; The graph is really huge, so you'll need to scroll around a bit to
;; see all the languages.

(let [directly-influenced-by (s/get-property "influenced by")
      had-influence-from `(* ~directly-influenced-by)
      lisp (s/entity "Lisp")
      form `{:select [?itemLabel ?influencedByLabel]
             :where [[?item ~had-influence-from ~lisp]
                     [?item ~directly-influenced-by ?influencedBy]
                     [?influencedBy ~had-influence-from ~lisp]]}
      data (s/query form)
      edges (map (juxt :itemLabel :influencedByLabel) data)
      vertices (-> edges flatten distinct)
      graph (arr/with-graph (arr/create-graph)
              (let [vertex (reduce #(assoc %1 %2 (arr/insert-vertex! %2)) {} vertices)]
                (doseq [[to from] edges]
                  (arr/insert-edge! (vertex from)
                                    (vertex to)))))]
  
  (-> graph 
        arr/as-svg
      clerk/html
        (assoc :nextjournal/width :full)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/label :wdt/P31)

(s/label :wd/Q5)

(s/label :wd/P40)

;; wdno = no value!
(s/query '{:select [?human ?humanLabel]
           :where [{?human {:wdt/P31 #{:wd/Q5}
                            :rdf/type #{:wdno/P40}}}]
           :limit 20})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO places can be a 2D Point() for lat/lon, or have an entity ID
;; prepended to indicate that the location is not on Earth. Not sure
;; the nicest way to return that info.

{:place :wd/Q25908808,
 :loc
 {:datatype "http://www.opengis.net/ont/geosparql#wktLiteral",
  :type "literal",
  :value
  "<http://www.wikidata.org/entity/Q3123> Point(-351.1 -44.87)"}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more advanced query features

(def query-2
  (s/query '{:prefixes {:dc  "<http://purl.org/dc/elements/1.1/>"
                        :xsd "<http://www.w3.org/2001/XMLSchema#>"}
             :select   [?title]
             :from     ["<http://my-anime-rdf-graph.com>"]
             :where    [[:union [{_b1 {:dc/title     #{{:en "Attack on Titan"}}
                                       :dc/publisher #{?publisher}}}]
                         [{_b2 {:dc/title     #{{:jp "進撃の巨人"}}
                                :dc/publisher #{?publisher}}}]]
                        {?work {:dc/publisher #{?publisher}
                                :dc/title     #{?title}
                                :dc/date      #{?date}}}
                        [:filter (<= #inst "2010-01-01T00:00:00Z" ?date)]]}))

;; adapted from https://www.wolfram.com/language/12/rdf-and-sparql/find-public-and-alive-sparql-endpoints.html

(def endpoints
  (query '{:select [?itemLabel ?endpoint]
           :where [[?item :wdt/P5305 ?endpoint]]}))

; TODO add timeout
(defn liveness "Runs a minimal query to check if the SPARQL service is responsive" [endpoint]
  (do-query "SELECT * WHERE { ?s ?p ?o . } LIMIT 1" endpoint))

(liveness (:endpoint (rand-nth endpoints)))

;; adapted from https://www.wolfram.com/language/12/rdf-and-sparql/request-linked-data-from-a-website.html

(def album-ld
  (deref
   (s/async-get-json "https://musicbrainz.org/release/30b9c67f-812c-430e-8b4a-d6fda8a7b08b"
                     {:headers {"accept" "application/ld+json"}})))

;; Write a query that requests, for all tracks, the track number and name.
(map #(dissoc % :recordingOf (keyword "@type")) (:track album-ld))