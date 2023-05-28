(ns cyberland.gui.event
  (:require [cljfx.api :as fx]
            [clojure.pprint :refer [pprint]]
            [org.bovinegenius.exploding-fish :as uri])
  (:import [java.net URL]))

(defn- stringify-keys
  [m]
  (into {} (map (juxt (comp name key) val) m)))

(defn- popup-error
  [context msg]
  {:context (fx/swap-context context assoc
                             :alert {:showing true
                                     :content-text (.toString msg)}
                             :pending? false)})

(defmulti handle :event/type)

(defmethod handle :default [event]
  #_(def foo event)
  (pprint (:fx/event event))
  (println ":default handler"))

(defmethod handle ::http-failure [{:keys [fx/context exception]}]
  (when (instance? clojure.lang.ExceptionInfo exception)
    (pprint (.getInfo exception)))
  (.printStackTrace exception)
  (popup-error context exception))

(defmethod handle ::open-server [{:keys [fx/context fx/event]}]
  (try
    (let [url (.toString (URL. (-> event .getTarget .getText)))]
      {:context (fx/swap-context context assoc
                                 :server url
                                 :pending? true)
       :http {:method :get
              :as :json
              :url (uri/path url "/config")
              :on-exception {:event/type ::http-failure}
              :on-response {:event/type ::open-server-config}}})
    (catch Exception e
      (popup-error context e))))

(defmethod handle ::open-server-config [{:keys [fx/context response]}]
  (let [c (stringify-keys (reverse (:body response)))
        b (-> c first key)]
    {:context (fx/swap-context context assoc
                               :server-config c
                               :board b)
     :http {:method :get
            :as :json
            :url (uri/path (fx/sub-val context :server) (str "/" b "/"))
            :query-params {:num (-> (get c b) :max_replies_no_thread)}
            :on-exception {:event/type ::http-failure}
            :on-response {:event/type ::load-board}}}))

(defmethod handle ::load-board [{:keys [fx/context response]}]
  {:context (fx/swap-context context assoc
                             :posts (->> (:body response)
                                         (sort-by :id)
                                         (group-by :replyTo))
                             :reply-to 0
                             :pending? false
                             :scene :list-boards)})

(defmethod handle ::pick-server-menu [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :scene :pick-server)})

(defmethod handle ::set-reply-to [{:keys [fx/context fx/event reply-to]}]
  (when-not (nil? event)
    {:context (fx/swap-context context assoc
                               :reply-to (or reply-to
                                             #_(-> event .getSource .getTreeItem .getValue :id)
                                             (-> event .getValue :id)))}))

(defmethod handle ::set-comment [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc :comment event)})

(defmethod handle ::set-shell-command [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc :shell-command event)})

(defmethod handle ::preview-shell-command [{:keys [fx/context]}]
  (let [cmd (fx/sub-val context :shell-command)]
    (if (empty? cmd)
      (popup-error context "No shell command to preview.")
      {:shell {:command cmd
               :dispatch {:event/type ::print-shell-command}}})))

(defmethod handle ::print-shell-command [{:keys [output]}]
  (println (:out output))
  (when-not (zero? (:exit output))
    (binding [*out* *err*]
      (println "exit code:" (:exit output))
      (println (:err output)))))

(defmethod handle ::post-post [{:keys [fx/context output]}]
  (let [content (fx/sub-val context :comment)
        cmd (fx/sub-val context :shell-command)]
    (cond (empty? content)
          (popup-error context "Comment is empty.")

          (and (seq cmd) (empty? output))
          {:context (fx/swap-context context assoc :shell-command "")
           :shell {:command cmd
                   :dispatch {:event/type ::post-post}}}

          :else
          {:context (fx/swap-context context assoc
                                     :comment ""
                                     :pending? true)
           :http {:method :post
                  :url (uri/path (fx/sub-val context :server) (str "/" (fx/sub-val context :board) "/"))
                  ;; "https://httpbin.org/post"
                  :form-params {:content (if-let [out (:out output)]
                                           (str out content)
                                           content)
                                :replyTo (fx/sub-val context :reply-to)}
                  :on-exception {:event/type ::http-failure}
                  :on-response {:event/type ::reload-board}}})))

(defmethod handle ::set-board [{:keys [fx/context fx/event]}]
  {:context (fx/swap-context context assoc
                             :board (-> event .getTarget .getValue))
   :dispatch {:event/type ::reload-board}})

(defmethod handle ::reload-board [{:keys [fx/context response]}]
  {:context (fx/swap-context context assoc :pending? true)
   :http {:method :get
          :as :json
          :url (uri/path (fx/sub-val context :server) (str "/" (fx/sub-val context :board) "/"))
          :query-params {:num (-> (fx/sub-val context :server-config)
                                  (get (fx/sub-val context :board))
                                  :max_replies_no_thread)}
          :on-exception {:event/type ::http-failure}
          :on-response {:event/type ::load-board}}})

;; (defmethod handle ::set-reply-to [{:keys [fx/context]}])
;; (defmethod handle ::set-reply-to [{:keys [fx/context]}])

(defmethod handle ::close-alert [{:keys [fx/context]}]
  {:context (fx/swap-context context assoc :alert nil)})
