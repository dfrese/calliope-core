(ns dfrese.calliope.port
  "Functions to work with ports, which are named communication
  channels with running applications. Note that any value can be a
  'port', but usually namespaced keywords are a good choice."
  (:require [dfrese.calliope.core :as core]
            [dfrese.calliope.ext :as ext]
            [dfrese.calliope.impl.channel :as channel]))

;; Ports

;; access from outside of an app:

(defn send-to-port!
  "Sends the value to the port of the given application instance. This
  is intended for communicating with a running app from outside."
  [instance port value]
  ;; if there is no channel, the app did not subscribe to this port.
  (when-let [c (core/get-port-channel instance port)]
    (channel/send-to-channel! c value)))

(defn subscribe-to-port!
  "Subscribes to the port of the given application instance, by giving
  a callback that will be invoked with the value sent. This is
  intended for communicating with a running app from outside. Returns
  a unique id that can be used to call [[unsubscribe-from-port!]]
  later."
  [instance port callback]
  (let [ch (core/create-port-channel! instance port)]
    (channel/subscribe-to-channel! ch
                                   callback)))

(defn unsubscribe-from-port!
  "Unsubscribed from the port of the given application instance."
  [instance port id]
  ;; TODO: remove channel after last unsubscribe?
  (channel/unsubscribe-from-channel! (core/get-port-channel instance port) id))

;; access from inside an app:

(defrecord ^:no-doc PortSub [port]
  ext/ISub
  (-subscribe! [this context]
    (let [instance (ext/context-instance context)]
      (assert (some? instance))
      (let [id (subscribe-to-port! instance
                                   port (ext/dispatcher context))]
        [instance id])))
  (-unsubscribe! [this [instance id]]
    (unsubscribe-from-port! instance
                            port id)))

(defn port-sub
  "Returns a subscription for messages sent to the given port."
  [port]
  (PortSub. port))

(defrecord ^:no-doc SendToPortCmd [port v]
  ext/ICmd
  (-run! [this context]
    (send-to-port! (ext/context-instance context) port v)))

(defn send-to-port
  "Returns a command that will send `v` to the given port."
  [port v]
  (SendToPortCmd. port v))
