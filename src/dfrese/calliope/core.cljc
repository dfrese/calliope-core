(ns dfrese.calliope.core
  "Core functions to write and use calliope applications."
  (:require [dfrese.calliope.ext :as ext]
            [dfrese.calliope.impl.cmds :as cmds]
            [dfrese.calliope.impl.subs :as subs]
            [dfrese.calliope.impl.channel :as chan]
            [dfrese.clj.functions :as f]))

(defn cmd->
  "Adds transfomation functions to the given command, so that when the
  command dispatches a message `m`, it is threaded though the given
  transfomations before being delivered to the application."
  [cmd t & ts]
  (apply cmds/translate-cmd cmd t ts))

(defn batch-cmds
  "Combines one or more command into one."
  ([cmd] cmd)
  ([cmd & cmds]
   (apply cmds/batch-cmds cmd cmds)))

(defn sub->
  "Adds transfomation functions to the given subscription, so that
  when the sub dispatches a message `m`, it is threaded though the
  given transfomations before being delivered to the application."
  [sub t & ts]
  (apply subs/translate-sub sub t ts))

;; Models combined with cmds (convenient for initialization and update fns)

(defrecord ^:no-doc ModelWithCmd [model cmd])

(defn add-cmd
  "Adds a command to a model, to be returned from an init or update function
  of an application."
  [model cmd]
  (if (nil? cmd)
    model
    (if (instance? ModelWithCmd model)
      (update model :cmd batch-cmds cmd)
      (ModelWithCmd. model cmd))))

(defn extract-model+cmd
  "Returns a tuple `[model cmd]`, where `cmd` was previously added to
  a model. This is mainly useful for unit tests."
  [model]
  (if (instance? ModelWithCmd model)
    [(:model model) (:cmd model)]
    [model nil]))

(defn extract-model
  "Returns the pure model, dropping any commands previously added to
  it. This is mainly useful for unit tests."
  [model]
  (first (extract-model+cmd model)))

#_(defn extract-cmd [v]
  (second (extract-model+cmd v)))

(defn update-model
  "Calls `f` with the model, without any commands added to it
  previously, and all remaining `args`, and adds any commands again
  afterwards."
  [model f & args]
  (let [[m' c] (extract-model+cmd model)]
    (add-cmd (apply f m' args) c)))

(defn ^:no-doc model->* [m & fs]
  (reduce update-model
          m
          fs))

#?(:clj
   (defmacro model-> "This macro is similar to the threading macro
   `->`, but removes any command added to the model first, and adds it
   again afterwards. So for example
~~~
(model-> (add-cmd {} my-cmd)
         (assoc :a 42))
=
(add-cmd (-> {} (assoc :a 42))
         my-cmd)
~~~"
     [m & exprs]
     (let [fns (map (fn [expr]
                      (if (list? expr)
                        `(fn [v#] (~(first expr) v# ~@(rest expr)))
                        `(fn [v#] (~expr v#))))
                    exprs)]
       `(model->* ~m ~@fns))))

;; Apps

(defrecord ^:no-doc CalliopeApp [canvas init update subscription])

(deftype ^:no-doc CalliopeInstance
  [canvas update subscription state element])

(comment TODO would be nice to have
         #?@(:clj [clojure.lang.IDeref
            (deref [this] (:model @state))])
  #?@(:cljs [cljs.core.IDeref ;; FIXME?
             (-deref [this] (:model @state))])
  )

(defn- call-later! [f & args]
  #?(:clj (future (apply f args)))
  #?(:cljs (.setTimeout js/window #(apply f args) 0))
  nil)

(declare handle-message)

(defn ^:no-doc instance-element [^CalliopeInstance instance]
  (.-element instance))

(defn- sub-cmd-context [instance]
  (ext/context (partial handle-message instance)
               ::instance instance))

(defn- run-command! [cmd context]
  (when cmd
    (ext/-run! cmd context)))

(defn- subscribe! [s context]
  ;; TODO: verify the sub does not try to send a message synchronously?!
  (ext/-subscribe! s context))

(defn- unsubscribe! [s id]
  (ext/-unsubscribe! s id))

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

(defn- new-canvas-state [canvas-state ^CalliopeInstance instance model]
  (let [element (.-element instance)
        canvas (.-canvas instance)]
    ;; Note: Generally, either the view must not dispatch messages synchronously,
    ;; or a model update must not render synchronously, to allow a
    ;; strict state progress.
    
    ;; Because event handlers are usually asynchronously triggered, we
    ;; prevent them handling messages synchronously. We have to,
    ;; because the browser may actually do that! For example when a
    ;; focused element is removed, a 'blur' event is dispatched
    ;; synchronously :-( (it' still better if the user calls blur() in
    ;; on-blur then, to prevent a duplicate message; but only his
    ;; model may break, instead of the whole app)
    (swap! (.-state instance) assoc :msg-mode :async)
    (let [nstate (ext/-update-canvas! canvas canvas-state element model (partial handle-message instance))]
      (swap! (.-state instance) assoc :msg-mode :sync)
      nstate)))

(defn- set-model! [^CalliopeInstance instance model]
  ;; TODO: will-update, did-update (did-update via vdom/post-commit-hook?)
  ;; Note: msg-callback must not be called synchronously here!
  (swap! (.-state instance) assoc :msg-mode :async)
  (let [context (sub-cmd-context instance)
        sub-map (update-subs! (:sub-map @(.-state instance)) ((.-subscription instance) model)
                              context)
        canvas-state (new-canvas-state (:canvas-state @(.-state instance)) instance model)]
    (swap! (.-state instance)
           assoc
           :model model
           :canvas-state canvas-state
           :sub-map sub-map
           :msg-mode :sync)))

(defn- do-handle-message! [^CalliopeInstance instance msg]
  (let [[model cmd] (extract-model+cmd ((.-update instance) (:model @(.-state instance)) msg))]
    (set-model! instance model)
    ;; Note: commands may send new messages synchronously, so we may
    ;; recur from here; but wenn that happens infinitely we'll just
    ;; run out of stack.
    (run-command! cmd (sub-cmd-context instance))))

(defn- warning [msg]
  ;; TODO: something better?
  (println "Warning:" msg))

(defn- handle-message [^CalliopeInstance instance msg] ;; + callback for completion?
  (case (:msg-mode @(.-state instance))
    :async
    (call-later! do-handle-message! instance msg) ;; could issue a warning if it'a a sub? Or leave it as a 'valid' feature?
    :sync
    (do-handle-message! instance msg)
    :stopped
    (warning (str "Message ignored, that was sent to a stopped application instance: " (pr-str msg)))))

(defn ^:no-doc get-port-channel [^CalliopeInstance instance port]
  (get-in @(.-state instance) [:port-map port]))

(defn ^:no-doc create-port-channel! [^CalliopeInstance instance port]
  (get-in (swap! (.-state instance)
                 update-in [:port-map port]
                 (fn [ch]
                   (or ch (chan/channel))))
          [:port-map port]))

(def ^{:doc "A void canvas, which never generates anything visible."} no-canvas
  (reify ext/ICanvas
    (-init-canvas! [this element] nil)
    (-update-canvas! [this state element model msg-callback] nil)
    (-finish-canvas! [this state element] nil)))

(defn ^:no-doc start-internal!
  [element canvas update-f subscription initial-model initial-cmd]
  (let [state (atom {:model initial-model
                     :msg-mode :async
                     :canvas-state (ext/-init-canvas! canvas element)
                     :sub-map {}
                     :port-map {}})
        instance (CalliopeInstance. canvas update-f subscription state element)
        ;; init:
        canvas-state (new-canvas-state (:canvas-state @state) instance initial-model)
        context (sub-cmd-context instance)
        sub-map (update-subs! (:sub-map @state) ((.-subscription instance) initial-model)
                              context)]
    (swap! state assoc
           :canvas-state canvas-state
           :sub-map sub-map
           :msg-mode :sync)
    ;; Note: commands may now send messages synchronously
    (run-command! initial-cmd context)
    instance))

(defn app
  "Returns an app given

  - `canvas`, an implementation of ICanvas, representing the rendering frontend to use,
  - `init`, the initial model and optionally an initial command,
  - `update`, a function returning an updated model and optionally commands, for a given model and message,
  - `subscription`, a function returning a list of subscription for a model."
  [canvas init update subscriptions]
  (assert (satisfies? ext/ICanvas canvas))
  (CalliopeApp. canvas init update subscriptions))

(defn start!
  "Creates and returns an instance of the given app, using the given
  dom element to render the application view."
  [element ^CalliopeApp app]
  (let [[model cmd] (extract-model+cmd (.-init app))
        canvas (.-canvas app)]
    (start-internal! element canvas (.-update app) (.-subscription app)
                     model cmd)))

(defn stop!
  "Shuts down the given application instance. Returns the dom element that was used for it."
  [^CalliopeInstance instance]
  ;; unsubscribe from all subs
  (update-subs! (:sub-map @(.-state instance))
                nil
                ;; does not need a context, because it should only do unsubribtions:
                nil)
  ;; clear element
  (let [canvas (.-canvas instance)
        state (ext/-finish-canvas! canvas (:canvas-state @(.-state instance)) (.-element instance))]
    (swap! (.-state instance) assoc
           :msg-mode :stopped
           :sub-map nil
           :canvas-state state))
  (.-element instance))
