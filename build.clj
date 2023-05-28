(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def version "0.1.0-SNAPSHOT")
(def main 'cyberland.gui.core)

(def basis (b/create-basis {:project "deps.edn"}))
(def class-dir "target/classes")
(def uber-file (format "target/cyberland.gui-%s.jar" version))

(defn- log [& args]
  (apply println
         (.format (java.time.LocalDateTime/now)
                  java.time.format.DateTimeFormatter/ISO_LOCAL_DATE_TIME)
         args))

(defn clean [_]
  (log "Cleaning target…")
  (b/delete {:path "target"}))

(defn uber "Baue ein Überjar." [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (log "Compiling…")
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (log "Building Überjar (this may take a while)…")
  (b/uber {:basis basis
           :class-dir class-dir
           :uber-file uber-file
           :main main}))
