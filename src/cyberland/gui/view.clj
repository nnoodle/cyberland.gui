(ns cyberland.gui.view
  (:require [cljfx.api :as fx]
            [cljfx.ext.tree-view :as fx.ext.tree-view]
            [cyberland.gui.event :as event]
            [cyberland.gui.colors :refer [xterm-colors]])
  (:import java.time.Instant
           javafx.application.Platform
           [net.digger.util.vt VTParser VTEmulator]))


(defn- ansi->data [s]
  (let [buf (atom [])
        vt (VTParser.
            (reify VTEmulator
              (actionCSIDispatch [this ch intermediate-chars params]
                (swap! buf conj {:ch ch :params params}))
              (actionExecute [this ch] (swap! buf conj ch))
              (actionPrint [this ch] (swap! buf conj ch))
              ))]
    (.parse vt s)
    @buf))

(def ^:private rgb->hex
  (memoize
   (fn [[r g b]]
     (format "#%02x%02x%02x" r g b))))

(defn- select-color
  [[ground table r g b & other]]
  (let [ground ({38 :-fx-fill 48 :-fx-background-color} ground)
        color (rgb->hex
               (if (= 2 table)
                 [r g b]
                 (get xterm-colors r)))]
    (if (and (seq other)                ; not-empty
             (or (= (first other) 38)
                 (= (first other) 48)))
      (merge {ground color} (select-color other))
      {ground color})))

(comment
  (select-color [38 2 137 152 201 48 2 185 199 190]) ; => fg & bg
  (select-color [48 2 137 152 201 38 2 185 199 190]) ; => fg & bg
  (select-color [38 2 137 152 201])                  ; fg
  (select-color [48 2 137 152 201])                  ; bg
  (select-color [38 5 137 152 201])                  ; fg 256
  (select-color [48 5 185 199 190]))                 ; bg 256

(defn- xterm-color
  [n]
  (rgb->hex (get xterm-colors n)))

(defn- ansi->text-flow
  [s]
  (let [buf (atom [])
        vt (VTParser.
            (reify VTEmulator
              ;; https://en.wikipedia.org/wiki/ANSI_escape_code
              (actionCSIDispatch [this ch intermediate-chars params]
                (when (= ch \m)
                  (swap! buf conj
                         (let [sgr (first params)]
                           (case sgr
                             0 :reset
                             38 (select-color params)
                             48 (select-color params)
                             (cond
                               (<= 30 sgr 37) {:-fx-fill (xterm-color (- sgr 30))}
                               (<= 40 sgr 47) {:-fx-background-color (xterm-color (- sgr 40))}
                               (<= 90 sgr 97) {:-fx-fill (xterm-color (- sgr 30))}
                               (<= 100 sgr 107) {:-fx-background-color (xterm-color (- sgr 30))}
                               :else (binding [*out* *err*]
                                       (println "unknown SGR:" sgr)
                                       :ignore)))))))
              (actionExecute [this ch] (swap! buf conj ch))
              (actionPrint [this ch] (swap! buf conj ch))))
        parsed (do (.parse vt s) @buf)
        styled (:text
                (reduce
                 (fn [{:keys [style text] :as col} v]
                   (cond (= :ignore v) col
                         (= :reset v) (assoc col :style {})
                         (string? v) (update col :text conj {:style style :text v})
                         :else (update col :style merge v)))
                 {:style {}
                  :text []}
                 (->> parsed
                      (partition-by char?)
                      (map #(if (char? (first %))
                              (apply str %)
                              (into [] %)))
                      flatten)))]
    {:fx/type :text-flow
     :style {:-fx-font-family "monospace"
             :-fx-fill :black}
     :children (for [{:keys [style text]} styled]
                 (if (empty? style)
                   {:fx/type :text
                    :text text}
                   {:fx/type :text-flow
                    :style style
                    :children [{:fx/type :text
                                :style {:-fx-fill :inherit}
                                :text text}]}))}))

(defn- pick-server
  [{:keys [fx/context]}]
  {:fx/type :v-box
   :padding 15
   :alignment :center-left
   :children
   [{:fx/type :label
     :text "Cyberland Server"}
    {:fx/type :text-field
     :prompt-text "Cyberland Server URL"
     :text "https://cyberland.bobignou.red/"
     :pref-width 300
     :disable (fx/sub-val context :pending?)
     :on-action {:event/type ::event/open-server}}]})

(def ^:private mock-post
  {:id 0
   :replyTo 0
   :time 0
   :bumpCount 0 ; unused
   :content "The Board"})

(defn- format-post
  [post]
  (let [post (merge mock-post post)]
    {:head (format "No. %d; >>%d; %s\n"
                   (:id post)
                   (:replyTo post)
                   (.toString (Instant/ofEpochSecond (:time post))))
     :body (:content post)}))

(defn- posts-children
  "Tree of posts replying to reply-id"
  [reply-id posts]
  (for [x (filter #(not= reply-id (:id %)) (posts reply-id))]
    {:fx/type :tree-item
     :value x
     :children (posts-children (:id x) (dissoc posts reply-id))}))

(defn- posts-tree
  [ctx]
  (let [posts (fx/sub-val ctx :posts)
        x (or (some #(and (= 0 (:id %)) %)
                    (posts 0))
              mock-post)]
    {:fx/type :tree-item
     :expanded true
     :value x
     :children (posts-children 0 posts)}))

(defn- ansi? [ctx]
  (:enable_ansi_code ((fx/sub-val ctx :server-config) (fx/sub-val ctx :board))))

(defn- list-boards
  [{:keys [fx/context]}]
  {:fx/type :v-box
   :padding 15
   :spacing 7
   :children
   [{:fx/type :h-box
     :spacing 3
     :children [{:fx/type :button
                 :tooltip  {:fx/type :tooltip, :text "Pick a different server"}
                 :text "←"
                 :on-action {:event/type ::event/pick-server-menu}}
                {:fx/type :combo-box
                 :tooltip {:fx/type :tooltip, :text "Select board"}
                 :button-cell (fn [x] {:text (str "/" x "/")})
                 :cell-factory {:fx/cell-type :list-cell
                                :describe (fn [x] {:text (str "/" x "/")})}
                 :items (fx/sub-val context (comp keys :server-config))
                 :disable (fx/sub-val context :pending?)
                 :on-action {:event/type ::event/set-board}}
                {:fx/type :button
                 :tooltip {:fx/type :tooltip, :text "Reload posts"}
                 :text "↺"
                 :disable (fx/sub-val context :pending?)
                 :on-action {:event/type ::event/reload-board}}]}
    {:fx/type fx.ext.tree-view/with-selection-props
     :v-box/vgrow :always
     :props {:selection-mode :single
             :on-selected-item-changed {:event/type ::event/set-reply-to}}
     :desc {:fx/type :tree-view
            :max-height ##Inf
            :cell-factory {:fx/cell-type :tree-cell
                           :describe
                           (let [ansi? (fx/sub-ctx context ansi?)]
                             (fn [x]
                               (let [txt (format-post x)]
                                 {:graphic
                                  {:fx/type :text-flow
                                   :children [{:fx/type :text
                                               :style {:-fx-font-weight :bold}
                                               :text (:head txt)}
                                              (if ansi?
                                                (ansi->text-flow (:body txt))
                                                {:fx/type :text
                                                 :font "monospace"
                                                 :text (:body txt)})]}})))}
            :root (fx/sub-ctx context posts-tree)}}
    {:fx/type :h-box
     :spacing 3
     :children [{:fx/type :button
                 :text "Post!"
                 :disable (fx/sub-val context :pending?)
                 :on-action {:event/type ::event/post-post}}
                {:fx/type :button
                 :tooltip {:fx/type :tooltip, :text "Click to not reply"}
                 :on-action {:event/type ::event/set-reply-to, :reply-to 0}
                 :text (fx/sub-val context #(if (zero? (:reply-to %))
                                              "New Thread"
                                              (format "Replying to >>%d" (:reply-to %))))}
                {:fx/type :text-field
                 :pref-column-count 30
                 :font "monospace"
                 :tooltip {:fx/type :tooltip
                           :show-duration 20000
                           :text "Optional shell command that will be prepended to your comment.
It's intended for posting ANSI images using your preferred program.
Press enter to preview its output in STDOUT."}
                 :prompt-text "Shell command"
                 :text (fx/sub-val context :shell-command)
                 :on-action {:event/type ::event/preview-shell-command}
                 :on-text-changed {:event/type ::event/set-shell-command}}]}
    {:fx/type :text-area
     :pref-row-count 7
     :wrap-text false
     :prompt-text "Comment"
     :font "monospace"
     :text (fx/sub-val context :comment)
     :on-text-changed {:event/type ::event/set-comment
                       :fx/sync true}}]})

(defn- select-scene
  [ctx]
  {:fx/type (case (:scene ctx) #_(fx/sub-val ctx :scene)
              :pick-server pick-server
              :list-boards list-boards)})

(defn root
  [{:keys [fx/context]}]
  {:fx/type :stage
   :title "Cyberland GUI Frontend"
   :showing true
   :width 500
   :height 700
   :on-close-request (fn [_] (Platform/runLater #(Platform/exit)))
   :scene
   {:fx/type :scene
    :root (fx/sub-val context select-scene)}})

(defn alert
  [{:keys [fx/context]}]
  (merge {:fx/type :alert
          :alert-type :error
          :showing false
          :on-hidden {:event/type ::event/close-alert}
          :button-types [:ok]}
         (fx/sub-val context :alert)))
