(ns apis
  (:require [nextjournal.clerk :as clerk]
            [martian.core :as martian]
            [api :as api]))

(let [m @(api/bootstrap-swagger "https://api.semanticscholar.org/graph/v1/swagger.json")]
  (martian/explore m))