(ns calliope.browser
  (:require [calliope.core :as core]
            [edonus.batch :as batch]))

(defrecord ^:no-doc Focus [document id]
  core/ICmd
  (-run! [this context]
    (when-let [node (.getElementById document id)]
      (batch/focus! node))))

(defn focus "Returns a command that sets the focus to the DOM element
  with the given id, in the given document. Note that the id does not
  need to exist now, and is it's silently ignored if does not later." ;; TODO: better explain
  [document id]
  (Focus. document id))

(defrecord ^:no-doc EventListenerSub [target event use-capture?] ;; -> core?
  core/ISub
  (-subscribe! [this dispatch!]
    (let [f (fn [e]
              (dispatch! e))]
      (.addEventListener target event f use-capture?)
      f))
  (-unsubscribe! [this f]
    (.removeEventListener target event f use-capture?)))

(defn location-hash [window]
  ;; e has (.-oldURL e), (.-newURL e).. seems quite useless.
  ;; location.hash is empty, or has a leading #
  (let [hash (.-hash (.-location window))]
    hash))

(defn- target [e]
  (.-target e))

(defn location-hash-updates [window]
  (core/sub-> (EventListenerSub. window "hashchange" false)
              target
              location-hash))

(defrecord ^:no-doc HashCmd [window hash]
  core/ICmd
  (-run! [this context]
    (set! (.-hash (.-location window))
          hash)))

(defn set-location-hash [window hash]
  (HashCmd. window hash))

;; TODO html5 history:
;; state

;; replaceState - change url
;; pushState - change url and go forward in history

;; back, forward, go
