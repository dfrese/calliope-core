(ns calliope.storage
  (:require [calliope.core :as core]))

(defrecord StorageGetCmd [which key]
  core/ICmd
  (-run! [this context]
    (core/dispatch! context (.getItem which key))))

(defrecord StorageSetCmd [which key value]
  core/ICmd
  (-run! [this context]
    (.setItem which key value)))

(defrecord StorageRemoveCmd [which key]
  core/ICmd
  (-run! [this context]
    (.removeItem which key)))

(defn local-storage-get [window key]
  (StorageGetCmd. (.-localStorage window) key))

(defn local-storage-set [window key value]
  (StorageSetCmd. (.-localStorage window) key value))

(defn local-storage-remove [window key]
  (StorageRemoveCmd. (.-localStorage window) key))

(defn session-storage-get [window key]
  (StorageGetCmd. (.-sessionStorage window) key))

(defn session-storage-set [window key value]
  (StorageSetCmd. (.-sessionStorage window) key value))

(defn session-storage-remove [window key]
  (StorageRemoveCmd. (.-sessionStorage window) key))
