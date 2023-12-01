(ns api
  (:require [ajax.core :as ajax]
            [clojure.string :as str]
            [martian.core :as martian]
            [martian.interceptors :as i]
            [martian.openapi :as openapi]
            [tripod.context :as tc]
            [promesa.core :as p]
            [promesa.exec :as px]))

(def formats {:transit (ajax/transit-response-format)
              :json-str (ajax/json-response-format)
              :json-kw (ajax/json-response-format {:keywords? true})
              :url (ajax/url-request-format)
              :ring (ajax/ring-response-format)
              :raw (ajax/raw-response-format)
              :text (ajax/text-request-format)
              :detect (ajax/detect-response-format)})

(defn async-request [params]
  (p/create (fn [resolve reject]
              @(ajax/ajax-request (assoc params
                                         :method (get params :method :get)
                                         :format (get formats (get params :format :url))
                                         :response-format (get formats (get params :response-format :detect))
                                         :handler (fn [[ok result]]
                                                    (if ok
                                                      (resolve result)
                                                      (reject result))))))
            px/default-executor))

(def ^:private go-async
  i/remove-stack)

(def perform-request
  {:name ::perform-request
   :leave (fn [{:keys [request] :as ctx}]
            (-> ctx
                go-async
                (assoc :response
                       (p/then (async-request request)
                               (fn [response]
                                 (:response (tc/execute (assoc ctx :response response))))))))})

(def default-interceptors
  (concat martian/default-interceptors [i/default-encode-body i/default-coerce-response perform-request]))

(def default-opts {:interceptors default-interceptors})

(defn bootstrap [api-root concise-handlers & [opts]]
  (martian/bootstrap api-root concise-handlers (merge default-opts opts)))

(defn bootstrap-openapi [url & [{:keys [server-url trim-base-url?] :as opts} load-opts]]
  (p/then (async-request {:uri url :response-format :json-kw})
          (fn [response]
            (let [definition response
                  raw-base-url (openapi/base-url url server-url definition)
                  base-url (if trim-base-url?
                             (str/replace raw-base-url #"/$" "")
                             raw-base-url)]
              (martian/bootstrap-openapi base-url definition (merge default-opts opts))))))

(def bootstrap-swagger bootstrap-openapi)
