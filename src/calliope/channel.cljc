(ns calliope.channel
  (:require [calliope.core :as core]))

(defn- next-sub-id [m]
  (inc (apply max 0 (keys m))))

(defn subscribe-to-channel! [ch callback]
  (let [id (next-sub-id @(.-subs ch))] ;; not thread-safe
      (swap! (.-subs ch) assoc id callback)
      id))

(defn unsubscribe-from-channel! [ch id]
  (swap! (.-subs ch) dissoc id))

(deftype ^:no-doc Channel [subs]
  core/ISub
  (-subscribe! [this context]
    (subscribe-to-channel! this (core/dispatcher context)))
  (-unsubscribe! [this id]
    (unsubscribe-from-channel! this id)))

(defn channel
  "TODO"
  []
  (Channel. (atom {})))

(defn send-to-channel! [ch v]
  (doseq [f (vals @(.-subs ch))]
    (f v)))

(defrecord ^:no-doc SendToChannelCmd [ch v]
  core/ICmd
  (-run! [this context]
    (send-to-channel! channel v)))
