(ns dfrese.calliope.component ;; TODO: -> calliope-orpheus lib?
  (:require [dfrese.calliope.core :as core]
            [dfrese.calliope.app :as app]
            [dfrese.orpheus.core :as orpheus]
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.clj.values :as v]
            [dfrese.edomus.core :as dom]
            [clojure.string :as string]))

(defn- set-instance! [element instance]
  ;; TODO: auto generate name.
  (aset element "__calliope_instance" instance))

(defn- get-instance [element]
  (aget element "__calliope_instance"))

(def properties-changed-port ::properties-changed)

(defrecord ^:no-doc ComponentAppType
  [init view update subscription node-type]
  orpheus/IForeignType
  (foreign-type-create [this props options]
    (let [node (orpheus/create-element-node node-type js/document) ;; FIXME: get document from parent
          model (init {::props props
                       ::options options})
          ;; view_ (fn []) TODO: grap old/new-props here for element-node-...? (need to do normalization in view then (instead of 'canvas-update')
          app (app/app orpheus/canvas model view update subscription)
          instance (app/start! node app)]
      (set-instance! node instance)
      (orpheus/element-node-was-created! node-type node)
      node))
  (foreign-type-patch! [this node old-props new-props options]
    ;; (orpheus/element-node-will-be-updated! node-type node old-props new-props) - not the right props. return v of view...?!
    
    ;; property changes are signalled via a sub, as it's supposed to change the model.
    (app/send-to-port! (get-instance node) properties-changed-port {::props new-props
                                                                    ::options options})

    ;; (orpheus/element-node-was-updated! node-type node props)
    )
  (foreign-type-destroy! [this node props options]
    (orpheus/element-node-will-be-removed! node-type node)
    ;; inform the component??
    (app/stop! (get-instance node))))

(defrecord ^:no-doc DispatchEventCmd
  [name value]
  core/ICmd
  (-run! [this context]
    (let [instance (app/context-instance context)
          model @instance
          dispatch! (::parent-dispatch! model)]
      (when-let [handler (get (::event-handlers model) (string/lower-case name))]
        (let [msg (handler value)]
          (when (some? msg)
            (dispatch! msg)))))))

(defn dispatch-event
  "Returns a command that triggers the given event on the component
  element. If the user of the component registered a handler for
  \"on<name>\", it will be invoked with the given value."
  [name & [value]]
  (DispatchEventCmd. name value))

;; Note: other than for a normal app, `init` must be a function from the components properties to the actual init value (a model+cmd)
(defn- component-type [element-type init view update subscription]
  (ComponentAppType. init view update subscription
                     (or (and (satisfies? orpheus/IElementType element-type) element-type)
                         (orpheus/element-type html/html-ns element-type))))

(defn- ctor-fn [type]
  (fn [props]
    (orpheus/h type props)))

(defn- get-property [props k]
  (get props (name k)))

(defn- get-event-handlers [props]
  (reduce-kv (fn [r k v]
               (if-let [name (base/event-type-name? k)]
                 (assoc r (string/lower-case name) v)
                 r))
             {}
             props))

(defn controlled-component
  "Defines a component where some aspects are controlled by the user.
  - `element-type` the tag name of the main element
  - `init` a function to create an initial model from the initial value of the controlling property
  - `reinit` ...TODO
  - `view`, `update` and `subscription` as usual.
  "
  [element-type init reinit view update subscription]
  ;; TODO: statify? (not crutial though)
  (ctor-fn
   (component-type element-type
                   (fn _init [v]
                     (let [props (::props v)
                           parent-options (::options v)
                           handlers (get-event-handlers props)]
                       {::model (init (partial get-property props))
                        ::event-handlers handlers
                        ::parent-dispatch! (:dispatch! parent-options)}))
                   (fn _view [model]
                     (view (::model model)))
                   (fn _update [model msg]
                     (let [[tag v] (v/untag msg)]
                       (if (= tag ::properties-changed)
                         (let [new-props (::props v)
                               parent-options (::options v)]
                           {::model (reinit (::model model) (partial get-property new-props))
                            ::parent-dispatch! (:dispatch! parent-options)
                            ::event-handlers (get-event-handlers new-props)})
                         (let [[m c] (core/extract-model+cmd (update (::model model) msg))]
                           (core/+cmd (assoc model ::model m)
                                      c)))))
                   (fn _subscription [model]
                     (conj (subscription (::model model))
                           (core/sub-> (app/port-sub properties-changed-port)
                                       (v/tagger ::properties-changed)))))))
