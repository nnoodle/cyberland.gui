(ns cyberland.gui.core
  (:require [clojure.java.shell :refer [sh]]
            [cljfx.api :as fx]
            [clj-http.client :as http]
            [clojure.core.cache :as cache]
            [cyberland.gui.view :as view]
            [cyberland.gui.event :as event])
  (:import [javafx.application Platform])
  (:gen-class))


(defn- http-effect
  "HTTP side effect event handler"
  [v dispatch!]
  (try
    (http/request
     (-> (merge {:as :byte-array} v)
         (assoc :async? true)
         (dissoc :on-response :on-exception))
     (fn [response]
       (dispatch! (assoc (:on-response v) :response response)))
     (fn [exception]
       (dispatch! (assoc (:on-exception v) :exception exception))))
    (catch Exception e
      (dispatch! (assoc (:on-exception v) :exception e)))))

(defn- shell-effect ; what could possibly go wrong?
  "Shell command event handler"
  [v dispatch!]
  (dispatch! (assoc (:dispatch v) :output (sh "sh" "-c" (:command v)))))

(def *context
  "cljfx application state."
  (atom
   (fx/create-context
    {:scene :pick-server
     :alert nil
     :server "https://cyberland.bobignou.red/"
     :server-config {}
     :board "x"
     :posts {}
     :reply-to 0
     :post-body ""
     :pending? false}
    cache/lru-cache-factory)))

(defn -main []
  (def app
    "cljfx application."
    (fx/create-app *context
                   :event-handler event/handle
                   :desc-fn (fn [_]
                              {:fx/type fx/ext-many
                               :desc [{:fx/type view/root}
                                      {:fx/type view/alert}]})
                   :effects {:http http-effect
                             :shell shell-effect}))
  (Platform/setImplicitExit true))

(comment ((app :renderer)))
