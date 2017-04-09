(ns dfrese.calliope.ext
  "Functions and type to extend calliope, namely new commands, subscriptions and canvas implementations."
  (:require [dfrese.clj.functions :as f]))

(defn ^:no-doc context [dispatch-msg! & more]
  (let [m {::dispatch! dispatch-msg!}]
    (if (not-empty more)
      (apply assoc m more)
      m)))

(defn ^:no-doc context-instance
  "Returns the application instance for the `context` argument in
  command and subscription implementations."
  [context]
  (::instance context))

#_(defn context-element
  "Returns the dom element the application instance is mounted on, for
  the `context` argument in command and subscription implementations."
  [context]
  (instance-element (context-instance context)))

(defn dispatcher
  "Given the `context` in a cmd or sub implementation, this returns the dispatcher function."
  [context]
  (::dispatch! context))

(defn dispatch!
  "Given the `context` in a cmd or sub implementation, this dispatches
  the given message to the application."
  [context msg]
  ((dispatcher context) msg))

(defn ^:no-doc update-dispatcher [context f & args]
  (apply update context ::dispatch! f args))

;; Commands

(defprotocol ICmd
  (-run! [this context] "Executes the side effect of this
  command. May immediately or later send messages to the app via `dispatch!` zero or more
  times."))

;; Subscriptions

(defprotocol ISub
  (-subscribe! [this context] "Add a new subscription to this,
  returning an id which can be passed to `unsubscribe!`. May later
  send messages to the app via `dispatch!`.")
  (-unsubscribe! [this id] "Cancel the subscription with the given
  id."))

;; Canvas - defining view in different languages.

(defprotocol ICanvas
  (-init-canvas! [this element] "Returns the initial canvas state for a given dom element. May mutate the element node.")
  (-update-canvas! [this state element model msg-callback] "Update the canvas for the given state and model, returning a new state.")
  (-finish-canvas! [this state element] "Finalize the canvas and element node upon application shutdown."))

