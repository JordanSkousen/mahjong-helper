(ns mahjong-helper.http
  (:require [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [re-re-frame.core :refer [console dispatch reg-event-x reg-fx]]))

(defonce debug?
  ^boolean js/goog.DEBUG)

(defn- prep-event [event] (if (keyword? event) [event] event))

(defn- http-response-interceptor
  [event config]
  (let [event' (prep-event event)]
    [::http-response-interceptor event' config]))

(reg-event-x
 ::http-response-interceptor
 (fn [db
      then-or-catch
      {:keys [url method body params load-flag-path]
       :as config}
      response]
   (let [response-body-only? (if (contains? config :response-body-only?)
                               (:response-body-only? config)
                               (:ok? response))
         response' (cond-> response
                     response-body-only? (get :body))]
     (when debug?
       (console :group (str "HTTP " (-> method name string/upper-case) " request to") url)
       (when body
         (console :log "Body:" body))
       (when params
         (console :log "Params:" params))
       (console :log "Response:" response')
       (console :groupEnd))
     {:fx [[:dispatch (conj then-or-catch response')]
           (when load-flag-path
             [:dispatch [::load-flag load-flag-path false]])]})))

(reg-fx
 ::http-via-fetch
 (fn [{:keys [method uri headers on-success on-failure params body response-body-as]}]
   (let [uri' (if params
                (let [url (js/URL. uri)]
                  (doall
                   (for [[key val] params]
                     (.append (.-searchParams url)
                              (if (keyword? key)
                                (name key)
                                key)
                              val)))
                  (.toString url))
                uri)
         read-body (fn [response]
                     (let [ok? (or (.-ok response) false)
                           response' {:headers (when (.-headers response) ;; on safari if a CORS error occurs or something, the headers are null
                                                 (-> response
                                                     .-headers
                                                     .entries
                                                     (js/Object.fromEntries)
                                                     (js->clj :keywordize-keys true)))
                                      :ok? ok?
                                      :status (.-status response)}
                           dispatch-evt (fn [body]
                                          (->> body
                                               (assoc response' :body)
                                               (conj (if ok? on-success on-failure))
                                               dispatch))]
                       (go
                         (case response-body-as
                           :json
                           ;; we don't use response.json(), in case the json is malformed
                           ;; if it's malformed, response.json() would throw an error and response.text() would fail because the body has been "consumed"
                           ;; so we do it like this, so we can still know what the response's body was and throw that instead
                           (let [response-text (when (.-text response)
                                                 (<p! (.text response)))]
                             (try
                               (dispatch-evt (if (not (string/blank? response-text))
                                               (-> response-text
                                                   js/JSON.parse
                                                   (js->clj :keywordize-keys true))
                                               response-text))
                               (catch js/Error _
                                 (dispatch-evt response-text))))

                           :text
                           (dispatch-evt (<p! (.text response)))

                           :edn
                           (dispatch-evt (edn/read-string (<p! (.text response))))

                           :array-buffer
                           (dispatch-evt (<p! (.arrayBuffer response)))

                           :blob
                           (dispatch-evt (<p! (.blob response)))

                           :form-data
                           (dispatch-evt (<p! (.formData response)))))))]
     (go
       (try
         (let [response (<p! (js/fetch uri'
                                       (cond-> {:method method}
                                         (some? headers) (assoc :headers headers)
                                         (some? body) (assoc :body body)
                                         :always clj->js)))]
           (read-body response))
         (catch js/Error response
           (read-body response)))))))

(defn http-request
  [db {:keys [url method headers body params then catch]
       :as config}]
  (let [headers' (merge {"Content-Type" "application/json"}
                        headers)
        body-is-json? (= (get headers' "Content-Type") "application/json")]
    {::http-via-fetch
     (cond-> {:method           (-> method name string/upper-case)
              :uri              url
              :headers          headers'
              :on-success       (http-response-interceptor then config)
              :on-failure       (http-response-interceptor catch config)
              :response-body-as (get config :response-body-as :json)}
       params (assoc :params params)
       body (assoc :body (if body-is-json?
                           (-> body clj->js js/JSON.stringify)
                           body)))}))

(defn http-fx-body
  "Sends a http request.
   When using the fxs like :http-get or :http-post, the params are:
     `url`: The URL of the request.
     `config`: map with the following params:
       - `headers`: map of request headers
       - `body`: the request body. Converted to JSON by default, unless you specify a different 'Content-Type' in `headers`.
       - `params`: map of query params to add to the URL.
       - `load-flag-path`: the path of a param in the app-db to set to true while the request is loading, and false after completion.
       - `response-body-only?`: set to `false` to receive the entire response, including headers, status code and body. Defaults to `true`.
       - `response-body-as`: possible options are `:json`, `:text`, `:edn`, `:array-buffer`, `:blob`, `:form-data`. Defaults to `:json`.
     `then`: an event to dispatch on a successful response (2xx). Can be a keyword or coll (the HTTP response is always provided as the last param btw).
     `catch`: an event to dispatch on a failed response (4xx/5xx). Can be a keyword or coll (the HTTP response is always provided as the last param btw)."
  [method [url {:keys [load-flag-path] :as config} then catch]]
  (let [config' (merge config {:url url
                               :method method
                               :then then
                               :catch catch})]
    (when load-flag-path
      (dispatch [::load-flag load-flag-path true]))
    (dispatch [::http-request config'])))

(reg-fx
 :http-get
 (partial http-fx-body :get))
(reg-fx
 :http-post
 (partial http-fx-body :post))
(reg-fx
 :http-put
 (partial http-fx-body :put))
(reg-fx
 :http-delete
 (partial http-fx-body :delete))
(reg-fx
 :http-patch
 (partial http-fx-body :patch))

(reg-event-x
 ::http-request ;; do not use -- use above fxs (http-get, http-post, etc)
 (fn [db config]
   (http-request db config)))

(reg-event-x
 ::load-flag
 (fn [db path flag]
   (assoc-in db path flag)))

(defn reg-event-http
  [key f]
  (reg-event-x
   key
   (fn [db & params]
     (let [result (apply f db params)
           result'
           (reduce (fn [result' http-key]
                     (if (contains? result' http-key)
                       (update result' http-key (fn [[url http-config then-fn-or-evt catch-fn-or-evt]]
                                                  [url
                                                   http-config
                                                   (if (fn? then-fn-or-evt)
                                                     [::reg-event-http-after then-fn-or-evt]
                                                     then-fn-or-evt)
                                                   (if (fn? catch-fn-or-evt)
                                                     [::reg-event-http-after catch-fn-or-evt]
                                                     catch-fn-or-evt)]))
                       result'))
                   result
                   [:http-get :http-post :http-put :http-delete :http-patch])]
       result'))))

(reg-event-x
 ::reg-event-http-after
 (fn [db after-fn response]
   (let [result (after-fn db response)]
     (if (vector? result)
       {:dispatch result}
       result))))