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

(defn- location-hash
  "Returns the current location hash of the given browser window."
  [window]
  ;; e has (.-oldURL e), (.-newURL e).. seems quite useless.
  ;; location.hash is empty, or has a leading #
  (let [hash (.-hash (.-location window))]
    hash))

(defrecord ^:no-doc GetHashCmd [window]
  core/ICmd
  (-run! [this context]
    (core/dispatch! context (location-hash window))))

(defn get-location-hash
  "Returns a command that sends the current location hash."
  [window]
  (GetHashCmd. window))

(defn- target [e]
  (.-target e))

(defn location-hash-updates
  "Returns a subscription to changes of the location hash of the given browser
  window. The message will be the new hash."
  [window]
  (core/sub-> (core/event-listener-sub window "hashchange" false)
              target
              location-hash))

(defrecord ^:no-doc SetHashCmd [window hash]
  core/ICmd
  (-run! [this context]
    (set! (.-hash (.-location window))
          hash)))

(defn set-location-hash
  "Returns a command that sets the location hash of the given browser window. It does not send any messages."
  [window hash]
  (SetHashCmd. window hash))

;; TODO html5 history:
;; state

;; replaceState - change url
;; pushState - change url and go forward in history

;; back, forward, go
