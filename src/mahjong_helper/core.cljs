(ns mahjong-helper.core
  (:require ["@mui/material/styles" :refer [createTheme ThemeProvider]]
            [mahjong-helper.handlers]
            [mahjong-helper.router :refer [init-router!]]
            [mahjong-helper.subs]
            [mahjong-helper.views :as views]
            [devtools.core :as devtools]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(defn install-devtools [] ; this is used to invert cljs console colors so it's actually readable in dark mode
  (let [{:keys [cljs-land-style]} (devtools/get-prefs)]
    (devtools/set-pref! :cljs-land-style (str "filter:invert(1);" cljs-land-style)))
  (devtools/install!))

(def mui-theme (createTheme (clj->js {:palette {:primary {:main "#000"}
                                                :secondary {:main "#fff"}}
                                      :typography {:fontFamily "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen', 'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif"}})))

(defn ^:dev/after-load mount-root []
  (init-router!)
  (rdom/render [:> ThemeProvider {:theme mui-theme}
                [views/Main]] 
               (.getElementById js/document "app")))

(defn init []
  (rf/dispatch-sync [:initialize-db])
  (install-devtools)
  (mount-root))