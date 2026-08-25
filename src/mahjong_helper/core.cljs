(ns mahjong-helper.core
  (:require ["@mui/material/styles" :refer [createTheme ThemeProvider]]
            [mahjong-helper.handlers]
            [mahjong-helper.router :refer [init-router!]]
            [mahjong-helper.subs]
            [mahjong-helper.utils :refer [read-storage]]
            [devtools.core :as devtools]
            [re-re-frame.core :refer [subscribe dispatch-sync]]
            [reagent.dom :as rdom]))

(defn install-devtools [] ; this is used to invert cljs console colors so it's actually readable in dark mode
  (let [{:keys [cljs-land-style]} (devtools/get-prefs)]
    (devtools/set-pref! :cljs-land-style (str "filter:invert(1);" cljs-land-style)))
  (devtools/install!))

(def mui-theme (createTheme (clj->js {:palette {:primary {:main "#000"}
                                                :secondary {:main "#fff"}}
                                      :typography {:fontFamily "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif"}})))

(defn router-component []
  [(or (-> @(subscribe [:current-route]) :data :view) :<>)])

(defn ^:dev/after-load mount-root []
  (init-router!)
  (rdom/render [:> ThemeProvider {:theme mui-theme}
                [router-component]] 
               (.getElementById js/document "app")))

(defn init []
  (dispatch-sync [:initialize-db (read-storage)])
  (install-devtools)
  (mount-root))