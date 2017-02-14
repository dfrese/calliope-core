(ns dfrese.calliope.app
  (:require [dfrese.calliope.core :as core]
            [dfrese.calliope.channel :as channel]))

(defprotocol ^:no-doc ICanvas ;; could also be named ICanvasType?
  (normalize-view [this v] "When different return values for view functions shall be possible, this normalizes them to a standard form.")
  (init-canvas! [this element] "May mutate the given dom element node, and returns the initial canvas state.")
  (update-canvas! [this state element v msg-callback] "Update the canvas for the given state and result of the view function `v`, returning a new state.")
  (finish-canvas! [this state element] "Finalize the canvas upon application shutdown."))

(declare handle-message)

(defn- sub-cmd-context [instance]
  (core/context (partial handle-message instance)
                ::instance instance))

(defn context-instance
  "Returns the application instance for the `context` argument in
  command and subscription implementations."
  [context]
  (::instance context))

(defn context-element
  "Returns the dom element the application instance is mounted on, for
  the `context` argument in command and subscription implementations."
  [context]
  (.-element (context-instance context)))

(defn- run-command! [cmd context]
  (when cmd
    (core/-run! cmd context)))

(defn- subscribe! [s context]
  ;; TODO: verify the sub does not try to send a message synchronously?!
  (core/-subscribe! s context))

(defn- unsubscribe! [s id]
  (core/-unsubscribe! s id))

(defn- subs-list [sub]
  (cond
    (nil? sub) []
    (sequential? sub) (vec sub)
    :else (assert false (str "Not a valid subscription result: " (pr-str sub)))))

(defn- update-subs! [prev-sub-map new-sub context]
  ;; Note: changing the context between updates will not cause a re-rendering (so it must not change effectively)
  ;; Note: this allows multiple identical subs (same eff and msg).. not sure if that's sane
  (let [news (subs-list new-sub)
        news-map (group-by identity news)]
    (doall
     (reduce (fn [sub-map k]
               (let [news-k (get news-map k)
                     prev-k (get sub-map k)
                     
                     diff (- (count prev-k) (count news-k))]
                 (cond
                   ;; remove some ?
                   (> diff 0)
                   (update sub-map k
                           (fn [ids]
                             (doseq [id (take diff ids)]
                               (unsubscribe! k id))
                             (drop diff ids)))
                   ;; add some ?
                   (< diff 0)
                   (update sub-map k
                           (fn [ids]
                             (doall
                              (concat (map (fn [_]
                                             (subscribe! k context))
                                           (range (- diff)))
                                      ids))))
                   ;; else, unchanged
                   :else
                   sub-map)))
             prev-sub-map
             (concat news (keys prev-sub-map))))))

(defn- new-canvas-state [canvas-state instance model]
  (let [element (.-element instance)
        canvas (.-canvas (.-app instance))]
    (let [html (normalize-view canvas (.view (.-app instance) model))]
      (update-canvas! canvas canvas-state element html (partial handle-message instance)))))

(defn- set-model! [instance model]
  ;; TODO: will-update, did-update (did-update via vdom/post-commit-hook?)
  ;; Note: msg-callback must not be called synchronously from here!
  (let [context (sub-cmd-context instance)
        sub-map (update-subs! (:sub-map @(.-state instance)) (.subscription (.-app instance) model)
                              context)
        canvas-state (new-canvas-state (:canvas-state @(.-state instance)) instance model)]
    (swap! (.-state instance)
           assoc
           :model model
           :canvas-state canvas-state
           :sub-map sub-map)))

(defn- handle-message [instance msg] ;; + callback for completion?
  (let [[model cmd] (core/extract-model+cmd (.update (.-app instance) (:model @(.-state instance)) msg))]
    (set-model! instance model)
    ;; Note: commands may send new messages synchronously, so we may recur from here:
    (run-command! cmd (sub-cmd-context instance))))

(defrecord ^:no-doc CalliopeApp [canvas init view update subscription])

(deftype ^:no-doc CalliopeInstance [app state element]
  IDeref
  (-deref [this] (:model @state)))

(defn app
  "Returns an app where

  - `canvas` is an implementation of ICanvas, representing the rendering backend to use,
  - `init` is the initial model and optionally an initial command,
  - `view` a function from a model to virtual dom, depending on the canvas used,
  - `update` a function from a model and a message to an updated model, and optionally a command,
  - `subscription` a function from a model to subscriptions"
  [canvas init view update subscriptions]
  (CalliopeApp. canvas init view update subscriptions))

(defn start!
  "Creates and returns an instance of the given app, using the given
  dom element to render the application view."
  [element app]
  (let [canvas (.-canvas app)
        state (atom {:model nil
                     :canvas-state (init-canvas! canvas element)
                     :sub-map {}})
        instance (CalliopeInstance. app state element)]
    (let [[model cmd] (core/extract-model+cmd (.-init app))]
      (set-model! instance model)
      ;; Note: commands may send new messages synchronously, so we may recur from here:
      (run-command! cmd (sub-cmd-context instance))
      instance)))

(defn stop!
  "Shuts down the given application instance. Returns the dom element that was used for it."
  [instance]
  ;; unsubscribe from all subs
  (update-subs! (:sub-map @(.-state instance))
                nil (sub-cmd-context instance))
  ;; clear element
  (let [canvas (.-canvas (.-app instance))
        state (finish-canvas! canvas (:canvas-state @(.-state instance)) (.-element instance))]
    (swap! (.-state instance) assoc :canvas-state state))
  (.-element instance))

;; Ports

;; (defrecord Port [name])

;; (defn port [name]
;;   (Port. name))

;; access from outside of an app:

(defn send-to-port!
  "Sends the value to the port of the given application instance. This
  is intended for communicating with a running app from outside."
  [instance port value]
  ;; if there is no channel, the app did not subscribe to this port.
  (when-let [c (get (:port-map @(.-state instance)) port)]
    (channel/send-to-channel! c value)))

(defn subscribe-to-port!
  "Subscribed to the port of the given application instance, by giving
  a callback that will be invoked with the value sent. This is
  intended for communicating with a running app from outside. Returns
  a unique id that can be used to call [[unsubscribe-from-port!]]
  later."
  [instance port callback]
  (swap! (.-state instance)
         update-in [:port-map port]
         (fn [ch]
           (or ch (channel/channel))))
  (channel/subscribe-to-channel! (get-in @(.-state instance) [:port-map port])
                                 callback))

(defn unsubscribe-from-port!
  "Unsubscribed from the port of the given application instance."
  [instance port id]
  ;; TODO: remove channel after last unsubscribe?
  (channel/unsubscribe-from-channel! (get-in @(.-state instance) [:port-map port]) id))

;; access from inside an app:

(defrecord ^:no-doc PortSub [port]
  core/ISub
  (-subscribe! [this context]
    (let [id (subscribe-to-port! (context-instance context)
                                 port (core/dispatcher context))]
      [(::instance context) id]))
  (-unsubscribe! [this [instance id]]
    (unsubscribe-from-port! instance
                            port id)))

(defn port-sub
  "Returns a subscription for messages sent to the given port."
  [port]
  (PortSub. port))

(defrecord ^:no-doc SendToPortCmd [port v]
  core/ICmd
  (-run! [this context]
    (send-to-port! (context-instance context) port v)))

(defn send-to-port
  "Returns a command that will send `v` to the given port."
  [port v]
  (SendToPortCmd. port v))


;; TODO:
;; commands to read imperative properties like clientHeight.., offsetHeight.., scroll..*   ...for the toplevel element only.

;; (defrecord AccessElementCmd [property]
;;   ICmd
;;   (.-run! [this context]
;;     (let [v (aget (context-element context)
;;                   property)]
;;       ;; FIXME: synchronous?
;;       (core/dispatch! context v))))

;; (def clientHeight (AccessElementCmd. "clientHeight"))
