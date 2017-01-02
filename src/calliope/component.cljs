(ns calliope.component
  (:require [calliope.core :as core]
            [orpheus.core :as orpheus]
            [orpheus.html :as html]
            [calliope.app :as app]
            [edomus.core :as dom]))

(defn- set-instance! [element instance]
  ;; TODO: auto generate name.
  (aset element "__calliope_instance" instance))

(defn- get-instance [element]
  (aget element "__calliope_instance"))

(def properties-changed-port ::properties-changed)
(def did-mount-port ::did-mount-port)

(defrecord ^:no-doc ComponentAppType
  [init view update subscription options node-type]
  orpheus/IElementType
  (create-element-node [this document]
    (orpheus/create-element-node node-type document))
  
  (element-node-was-created! [this node]
    (orpheus/element-node-was-created! node-type node)
    (let [model (init (fn [p]
                        (dom/get-property node p)))
          app (app/app model view update subscription)
          instance (app/create-instance! node app)]
      (set-instance! node instance)
      (app/send-to-port! (get-instance node) did-mount-port node) ;; port or fn?
      node))
  
  (element-node-will-be-updated! [this node]
    (orpheus/element-node-will-be-updated! node-type node))
  
  (element-node-was-updated! [this node]
    (orpheus/element-node-was-updated! node-type node)
    ;; property changes are signalled via a sub, as it's supposed to change the model.
    (app/send-to-port! (get-instance node) properties-changed-port
                       (fn [p] (dom/get-property node p)))

    ;; port or fn??? after flush?
    ;; (app/send-to-port! (get-instance node) did-update-port [old-props new-props])
    )
  (element-node-will-be-removed! [this node]
    (orpheus/element-node-will-be-removed! node-type node)
    ;; inform the component??
    (app/destroy-instance! (get-instance node))))

(defrecord ^:no-doc DispatchEventCmd [f args return?]
  core/ICmd
  (-run! [this context]
    (let [element (app/context-element context)
          event (apply f args)
          do-default? (.dispatchEvent element event)]
      (when return?
        (core/dispatch! context do-default?)))))

(defn dispatch-any-event*
  "Returns a command that emits an event on the component
  element. Events are created by `(apply f args)`. If `return?` is
  true, a message is sent to the app afterwards, specifying if
  `preventDefault` was not called during the event
  dispatch. Otherwise, no message is send."
  [return? f & args]
  (DispatchEventCmd. f args return?))

(defn- mk-event [event init]
  ;; TODO: init as cljs map? bubbled, cancelable, etc props.
  (new js/Event event init))

(defn dispatch-event
  "Returns a command that emits the given simple event on the
  component element. Events are created by `(new js/Event event
  init)`. If `return?` is true, a message is sent to the app
  afterwards, specifying if `preventDefault` was not called during the
  event dispatch. Otherwise, no message is send."
  [event & [init]]
  (dispatch-any-event* false mk-event event init))

(defn- mk-custom-event
  [event detail]
  ;; TODO: also bubbled, cancelable, etc props.
  (new js/CustomEvent event #js {"detail" detail}))

(defn dispatch-custom-event
  [event detail]
  (dispatch-any-event* false mk-custom-event event detail))

;; Note: other than for a normal app, `init` must be a function from the components properties to the actual init value (a model+cmd)
(defn- component-type [init view update subscription & [options]]
  (ComponentAppType. init view update subscription (dissoc options :node-type)
                     (or (:node-type options)
                         html/div)))

(defn- ctor-fn [type]
  (fn [props]
    (orpheus/h type props)))

(defn component
  "Returns a constructor for virtual elements, implemented via a model-view-update like apps.
   One difference between apps and components is that components get a property map from the    TODO describe childNodes, event-handlers
   user of the element. Subscribe to [[property-change-port]] so receives updates of the property map.

  - `init` is a function from a property map to the initial model and optionally an initial command,
  - `view` a function from a model to virtual dom,
  - `update` a function from a model and a message to an updated model, and optionally a command,
  - `subscription` a function from a model to subscriptions,
  - `options` is a map, which may set `:node-type` to the type of the main node hosting the component, which defaults to a \"div\"."
  [init view update subscription & [options]]
  (ctor-fn (component-type init view update subscription options)))

(defn controlled-component
  "Defines a component where some aspects of is controlled by the user.
  - `controlling-property` name of a property, which can be set by the user to control this components behaviour or view.
  - `init` a function to create an initial model from the initial value of the controlling property
  - `view`, `update` and `subscription` as usual.
  - the `controlling-property-changed-port` should be subscribed, to react to updates of the controlling property value.
  "
  [controlling-property init reinit view update subscription & [options]]
  ;; could generate change custom event, whenever it would like to change the controlling property by itself (if ever)
  (component (fn [get-property]
               (let [value (get-property controlling-property)]
                 (init value)))
             view
             (fn [model msg]
               (let [[tag v] (orpheus/untag msg)]
                 (if (= tag ::properties-changed)
                   (reinit model (v controlling-property))
                   (update model msg))))
             (fn [model]
               (conj (subscription model)
                     (core/sub-> (app/port-sub properties-changed-port)
                                 (orpheus/tag ::properties-changed))))
             options))
