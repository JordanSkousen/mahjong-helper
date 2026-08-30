(ns mahjong-helper.router
  (:require [clojure.string :as string]
            [mahjong-helper.solitaire.views :as solitaire]
            [mahjong-helper.views :as home]
            [re-re-frame.core :refer [dispatch reg-event-x reg-fx reg-grab]]
            [reitit.core :as r]
            [reitit.frontend :as rf]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.history :as rfh]))

(def PAGES
  [{:id :home
    :view home/Main}
   {:id :solitaire
    :view solitaire/Main}])

(defonce ^:private !router (atom nil))
(defonce ^:private !history (atom nil))

;; ================================================================================================
;; Custom EndSegmentHistory that allows us to store the current page at the end of the URL
;; ================================================================================================
(defn url->router-path
  [url]
  (let [^goog.Uri uri (.parse goog.Uri url)]
    (-> uri
        .getPath
        (string/split "/")
        last
        (->> (str "/")))))
(defn current-url-with-router-path
  [router-path]
  (let [^goog.Uri current-url (.parse goog.Uri (.. js/window -location -href))
        url (-> current-url
                .getPath
                (subs 1)
                (string/split "/")
                drop-last
                vec
                (conj (subs router-path 1))
                (->> (string/join "/")))]

    (str "/"
         url
         (when (.hasQuery current-url)
           "?")
         (.getQuery current-url))))

(defn ignore-anchor-click?-end-segment
  ;; This is copied from reitit.frontend.history, and is exactly the same EXCEPT the last bit
  [router e el ^goog.Uri uri]
  (let [current-domain (when (exists? js/location)
                         (.getDomain ^goog.Uri (.parse goog.Uri js/location)))]
    (and (or (and (not (.hasScheme uri)) (not (.hasDomain uri)))
             (= current-domain (.getDomain uri)))
         (not (.-altKey e))
         (not (.-ctrlKey e))
         (not (.-metaKey e))
         (not (.-shiftKey e))
         (or (not (.hasAttribute el "target"))
             (contains? #{"" "_self"} (.getAttribute el "target")))
         ;; Left button
         (= 0 (.-button e))
         ;; isContentEditable property is inherited from parents,
         ;; so if the anchor is inside contenteditable div, the property will be true.
         (not (.-isContentEditable el))
         ;; **this part is different** match the uri the anchor's pointing to to a router page
         (rf/match-by-path router (url->router-path (.toString uri))))))

(defrecord EndSegmentHistory [router on-navigate]
  rfh/History
  (-init [this]
    ;; Listen for browser Back/Forward button clicks
    (.addEventListener js/window "popstate"
                       (fn [_]
                         (rfh/-on-navigate this (rfh/-get-path this)))))

  (-stop [_]
    (.removeEventListener js/window "popstate"))

  (-get-path [_]
    ;; Read the "?page=..." query param to determine the current path
    (url->router-path (.. js/window -location -href))) ;; Default to root if no page param

  (-href [_ path]
    ;; Convert a path like "/start" into "/ims/pub/ui/XXX/start" or whatever
    (current-url-with-router-path path))

  (-on-navigate [_ path]
    ;; Match the path (e.g. "/start") against routes and trigger the callback
    (let [match (rf/match-by-path router path)]
      (on-navigate match))))
;; ================================================================================================

(defn on-navigate [new-match]
  (when new-match
    (dispatch [::navigated new-match])))

(defn init-router!
  []
  (let [router (rf/router ["/"
                           (->> PAGES
                                (map (fn [{:keys [id] :as page}]
                                       [(name id) (merge page {:name id
                                                               :href (name id)})]))
                                (into [["" (-> PAGES
                                               first
                                               (merge {:name :0
                                                       :href ""}))]]))])
        h (->EndSegmentHistory router on-navigate)]
    (reset! !router router)
    (reset! !history h)
    ;; Initialize listeners
    (rfh/-init h)
    ;; Trigger the initial route dispatch
    (rfh/-on-navigate h (rfh/-get-path h))))

(reg-event-x
 :goto-page
 (fn [db & route] 
   {:fx [[:push-state route]]}))

(defn push-state-end-segment
  [route-name]
  (let [match (r/match-by-name @!router route-name)
        path (:path match)
        url (current-url-with-router-path path)]
    ;; 1. Update Browser URL
    (.pushState js/history nil "" url)
    ;; 2. Trigger Reitit Match
    (if match
      (dispatch [::navigated match])
      (js/console.error "No match for route:" route-name))))

(reg-fx
 :push-state
 (fn [route]
   (apply push-state-end-segment route)))

(reg-event-x
 ::navigated
 (fn [db new-match]
   (let [history (get-in db [:ui :history] #{})
         old-match (get-in db [:ui :current-route])
         controllers (rfc/apply-controllers (:controllers old-match) new-match)]
     {:db (-> db
              (assoc-in [:ui :current-route] (assoc new-match :controllers controllers))
              (assoc-in [:ui :history] (conj history (-> new-match :data :name))))
      :fx [(when (not= (:path old-match) (:path new-match))
             [::scroll-to-top])]})))

(reg-fx
 ::scroll-to-top
 (fn []
   (.scrollTo js/window #js {:top 0 :left 0 :behavior "instant"})))

(reg-grab
 :current-route
 (fn [db]
   (get-in db [:ui :current-route])))