(ns dfrese.calliope.wcomponent
  (:require [dfrese.calliope.core :as core]
            [dfrese.calliope.app :as app]
            [dfrese.orpheus.core :as orpheus]
            [dfrese.orpheus.html :as html]
            [dfrese.clj.values :as v]
            [dfrese.edomus.core :as dom]))

(defn- set-instance! [element instance]
  ;; TODO: auto generate name.
  (aset element "__calliope_instance" instance))

(defn- get-instance [element]
  (aget element "__calliope_instance"))

(def properties-changed-port ::properties-changed)
(def did-mount-port ::did-mount-port)

(defrecord ^:no-doc ComponentAppType
  [init view update subscription node-type]
  orpheus/IElementType
  (create-element-node [this document]
    (orpheus/create-element-node node-type document))
  
  (element-node-was-created! [this node]
    (let [model (init (fn [p]
                        (dom/get-property node p)))
          app (app/app orpheus/canvas model view update subscription)
          instance (app/start! node app)]
      (set-instance! node instance)
      (app/send-to-port! (get-instance node) did-mount-port node) ;; port or fn?
      node)
    (orpheus/element-node-was-created! node-type node))
  
  (element-node-will-be-updated! [this node old-props new-props]
    ;; TODO: at some point (here?) we should check that the user does not change the same properties as the inner application (esp. the childNodes)
    (orpheus/element-node-will-be-updated! node-type node old-props new-props))
  
  (element-node-was-updated! [this node props]
    ;; property changes are signalled via a sub, as it's supposed to change the model.
    (app/send-to-port! (get-instance node) properties-changed-port
                       (fn [p] (dom/get-property node p)))

    ;; port or fn??? after flush?
    ;; (app/send-to-port! (get-instance node) did-update-port [old-props new-props])
    (orpheus/element-node-was-updated! node-type node props))
  (element-node-will-be-removed! [this node]
    (orpheus/element-node-will-be-removed! node-type node)
    ;; inform the component??
    (app/stop! (get-instance node))))

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
(defn- component-type [element-type init view update subscription]
  (ComponentAppType. init view update subscription
                     (or (and (satisfies? orpheus/IElementType element-type) element-type)
                         (orpheus/element-type html/html-ns element-type))))

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
  - `subscription` a function from a model to subscriptions."
  [element-type init view update subscription]
  (ctor-fn (component-type element-type init view update subscription)))

(defn controlled-component
  "Defines a component where some aspects are controlled by the user.
  - `element-type` the tag name of the main element
  - `controlling-property` name of a property, which can be set by the user to control this components behaviour or view.
  - `init` a function to create an initial model from the initial value of the controlling property
  - `view`, `update` and `subscription` as usual.
  - the `controlling-property-changed-port` should be subscribed, to react to updates of the controlling property value.
  "
  [element-type controlling-property init reinit view update subscription]
  ;; should generate change custom event, whenever it would like to change the controlling property by itself (if ever)
  (component element-type
             (fn [get-property]
               (let [value (get-property controlling-property)]
                 (init value)))
             view
             (fn [model msg]
               (let [[tag v] (v/untag msg)]
                 (if (= tag ::properties-changed)
                   (reinit model (v controlling-property))
                   (update model msg))))
             (fn [model]
               (conj (subscription model)
                     (core/sub-> (app/port-sub properties-changed-port)
                                 (v/tagger ::properties-changed))))))
