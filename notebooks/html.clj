;; demonstration of HTML parsing of Wikipedia
(ns html
  (:require [nextjournal.clerk :as clerk]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [hickory.core :as h]
            [hickory.select :as hs]
            [scrape :as scrape]
            [better-cond.core :as b]))

;; First, let's try parsing the tables on the Last Week Tonight page:

(scrape/parse-wiki-tables "https://en.wikipedia.org/wiki/List_of_Last_Week_Tonight_with_John_Oliver_episodes")

;; TODO: display via https://book.clerk.vision/#tables

;; This is a bit hard to view. Now lets look at all of the links contained in the tables using Hickory's select feature:

(defn extract-links [h]
  (hs/select (hs/tag :a) h))

(defn hickory->hiccup [hickory]
  "Convert from Hickory to Hiccup notation for display purposes"
 (match hickory
   {:type :element :tag t :content c :attrs a} (vec (->> c (map hickory->hiccup) (into [t a]) (filter some?)))
   nil ""
   :else hickory))

(def links 
  (->> "https://en.wikipedia.org/wiki/List_of_Last_Week_Tonight_with_John_Oliver_episodes"
       scrape/get-html
       (hs/select (hs/tag :table))
       (map extract-links) ; within each table, extract links
       (mapcat (fn [i links] (map #(assoc % :season i) links)) (range)) ; label each group of links by their season, then merge
       (filter (fn [elem] (and
                           (not= (:class (:attrs elem)) "mw-selflink-fragment")
                           (not (str/starts-with? (:href (:attrs elem)) "#cite_note-"))))) ; ignore citations for now
       (map (fn [e] (let [a (:attrs e) ; convert relative to absolute links so you can click on them
                          rel (:href a)
                          abs (str "https://en.wikipedia.org" rel)
                          a' (assoc a :href abs)
                          e' (assoc e :attrs a')]
                      e')))))

;; TODO: display as OL, resolve weird issue with nils

;; Next, lets try teasing out information from the outline of a Wikipedia article:

(def first-url (-> links first :attrs :href))
(def example-wiki-html (->> first-url scrape/get-html (hs/select (hs/id :mw-content-text)) first :content first))

(defn heading-level [tag]
  (let [m (re-matches #"h(\d+)" (name tag))]
    (if m (parse-long (second m)) m)))

;; TODO: index references
(defn parse-header [level attrs sections]
  "Keep parsing until the current header ends; returns the section and the remaining ungrouped elements"
  (loop [contents []
         remaining sections] 
    (b/cond
      :let [tag (keyword (str "h" level))]

      (empty? remaining) [{:type :section :tag tag :content contents :attrs attrs} []]

      :let [current (first remaining)
            rest (next remaining)]

      :let [current-level (heading-level (:tag current))]

      (some? current-level) (if (> current-level level)
                              (let [[headline editsection] (:content current)
                                    [current-header current-remaining] (parse-header current-level
                                                                                     {:level current-level
                                                                                      :id (:id (:attrs headline))
                                                                                      :name (first (:content headline))
                                                                                      :editsection (:attrs (second (:content editsection)))
                                                                                      :rest (:attrs current)}
                                                                                     rest)]
                                (recur (conj contents current-header) current-remaining))
                              
                              [{:type :section :content contents :attrs attrs :tag tag} remaining])
      
      :else (recur (conj contents current) rest))))

(defn parse-page [h]
  "Given Hickory representation of the main Wikipedia area, return one grouped into sections"
  (let [elems (->> h :content (filter #(and (map? %) (not= (:type %) :comment))))
        groups (group-by (fn [e]
                           (let [a (:attrs e)
                                 class (:class a)]
                             (cond (= class "navbox") :navbox
                                   (= class "infobox scotus") :infobox
                                   (= class "shortdescription nomobile noexcerpt noprint searchaux") :short
                                   (= (:tag e) :style) :drop
                                   (= class "mw-empty-elt") :drop
                                   (= class "navbox-styles") :drop
                                   (= (:property a) "mw:PageProp/toc") :drop
                                   :else :etc)))
                         elems)]
    (first (parse-header 1
                         {:level 1
                          :navs (:navbox groups)
                          :infobox (-> groups :infobox first)
                          :short (-> groups :short first :content first)}
                         (:etc groups)))))

(parse-page example-wiki-html)

;; TODO: use above method to create an interlicked knowledge graph