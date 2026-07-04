#_{:clj-kondo/ignore [:underscore-in-namespace]}
(ns mahjong_helper.dev.shadow
  (:require [clojure.string :as string]
            [shadow.cljs.devtools.api :as shadow]))

;; Run this with:
;; npx shadow-cljs clj-run src.mahjong_helper.dev.shadow/watch

(set! *warn-on-reflection* true)

(defn watch
  {:shadow/requires-server true}
  [& _args]
  ;; Both watches keep running until ctrl-c.

  ;; You could also call bash script here, so you could manage the Sass CLI options in one place for
  ;; both watch and release.
  (let [cmd [(if (string/includes? (System/getProperty "os.name") "Windows")
               "npx.cmd"
               "npx")
             "sass" "--watch"
             "-I" "." ;; Optional: include project root as a Sass include path
             "src/mahjong_helper/styles.scss:public/css/styles.css"]
        ;; Redirect stdin to process, so ctrl-c is seen by sass.
        ;; Also redirect output to stdout.
        builder (-> (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String cmd))
                    (.inheritIO))
        process (.start builder)]
    ;; Ctrl-c would close sass, but if shadow java process is just killed,
    ;; we need to also kill sass.
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread.
                        ;; Fn is Runnable
                       (fn []
                         (.destroy process))))
    (shadow/watch :browser)))