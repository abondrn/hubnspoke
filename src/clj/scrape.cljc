;; utilities to extract structured information from web pages
;; I am still figuring out the API so until then this is very much a WIP
(ns scrape
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :refer [subset?]]
            [hickory.core :as h]
            [hickory.select :as hs]
            
            [api :as api]))

(defn get-html "Gets the parsed Hickory representation of a web destination"
  [url] ; str -> Hickory
  (-> @(api/async-request {:uri url}) h/parse h/as-hickory))

(def re-word #"\w") 

(defn word-char?
  [char] ; char -> bool
  (not (nil? (re-matches re-word (str char)))))

(defn one-of?
  [x & things] ; T [T] -> bool
  (contains? (vec things) x))

(defn submap?
  [m M] ; map map -> bool
  (= (select-keys M (keys m)) m))

(defn join-tokens "Joins two strings by respecting their word boundaries"
  [current upnext] ; returns a single string
  (let [end (last current)
        start (first upnext)
        sep (if (and
                 (or (word-char? end) (one-of? end \) \.))
                 (or (word-char? start) (= start \()))
              " "
              "")]
    (str current sep upnext)))

(defn join-adjacent-strings "Simplifies a mixed collection of strings and objects by adjoining colocated strings"
  [coll]
  (loop [result []
         remaining coll]
    (if (empty? remaining)
      result
      (let [current (first remaining)
            rest (next remaining)
            upnext (first rest)]
        (cond
          (and (string? current) (string? upnext))
          (recur result
                 (cons (join-tokens current  upnext) (next rest)))
          (= 0 (count current))                    (recur result rest)
          (vector? current)                        (recur result (concat current rest))
          :else                                    (recur (conj result current) rest))))))

(defn simplify "Attempts to clean up nonsemantic elements in a Hickory object"
  [elem]
  (match elem
    ([sub] :seq)                                            (recur sub)
    ([& subs] :seq) (match (join-adjacent-strings (map simplify subs))
                      ([]        :seq) ""
                      ([sub']    :seq) sub'
                      ([& subs'] :seq) subs')
    {:type :element :content nil}                           ""
    {:tag :div :attrs (_ :only [:style :class]) :content c} (recur c)
    {:tag :abbr :attrs {:title title} :type :element}       (recur title)
    {:type :element :attrs a :content c :tag t}             (let [simplified (simplify c)]
                                                              (cond
                                                                (nil? a)                           simplified
                                                                (subset? (set (keys a)) #{:style}) simplified
                                                                (= a {:class "nowrap"})            simplified
                                                                (and (= t :sup) (submap? {:class "reference"} a)) simplified
                                                                (not-empty simplified)             (assoc elem
                                                                                                          :content simplified
                                                                                                          :attrs (if (and (= t :a) (= (:title a) simplified))
                                                                                                                   (dissoc a :title)
                                                                                                                   a))
                                                                :else                              ""))
    :else                                                   (if (string? elem) (str/trim elem) elem)))

(defn extract-table [h]
  (let [r (:content (last (:content h)))
        header (->> r first :content (map (comp simplify :content)))
        rows (->> r rest (filter #(not (string? %))) (map (comp #(map (comp simplify :content) %) :content)))]
    [header rows]))

(defn parse-wiki-tables
  [url]
  (let [dom (get-html url)
        tables (map extract-table (hs/select (hs/tag :table) dom))]
    tables))
