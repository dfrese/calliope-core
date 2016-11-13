(ns calliope.core
  (:require [active.clojure.arrow :as a]))

(defn context [dispatch-msg! & more]
  (apply assoc {::dispatch! dispatch-msg!}
         more))

(defn dispatcher [context]
  (::dispatch! context))

(defn dispatch! [context msg]
  ((dispatcher context) msg))

(defn update-dispatcher [context f & args]
  (apply update context ::dispatch! f args))

;; Commands

(defprotocol ICmd
  (-run! [this context] "Executes the side effect of this
  command. May immediately or later send messages to the app via `dispatch!` zero or more
  times."))

(defrecord ^:no-doc TransformedCmd [base t]
  ICmd
  (-run! [this context]
    (-run! base (update-dispatcher context #(a/>>> t %)))))

(defrecord ^:no-doc BatchedCmds [cmds]
  ICmd
  (-run! [this context]
    (doseq [c cmds]
      (-run! c context))))

(defn cmd-> [cmd t & ts]
  (cond
    (instance? BatchedCmds cmd)
    (BatchedCmds. (map #(apply cmd-> % t ts)
                       (.-cmds cmd)))
    
    (instance? TransformedCmd cmd)
    (TransformedCmd. (.-base cmd)
                     (apply a/>>> (.-t cmd) t ts))
    
    (satisfies? ICmd cmd)
    (TransformedCmd. cmd (apply a/>>> t ts))
    
    :else
    (assert false (str "Not a command:" cmd))))

(defn batch-cmds
  ([cmd] cmd)
  ([cmd & cmds]
   (BatchedCmds. (remove nil? (cons cmd cmds)))))

;; Subscriptions

(defprotocol ISub
  ;; TODO: allow synchronous msgs?
  (-subscribe! [this context] "Add a new subscription to this,
  returning an id which can be passed to `unsubscribe!`. May later
  send messages to the app via `dispatch!`.")
  (-unsubscribe! [this id] "Cancel the subscription with the given
  id."))

(defrecord ^:no-doc TransformedSub [base t]
  ISub
  (-subscribe! [this context]
    (-subscribe! base (update-dispatcher context #(a/>>> t %))))
  (-unsubscribe! [this id]
    (-unsubscribe! base id)))

(defn batch-subs
  ([sub] sub)
  ([s & subs]
   (apply vector (remove nil? (cons s subs)))))

(defn sub-> [sub t & ts]
  (cond
    (vector? sub)
    (mapv #(apply sub-> % t ts)
          sub)
    
    (instance? TransformedSub sub)
    (TransformedSub. (.-base sub)
                     (apply a/>>> (.-t sub) t ts))
    
    (satisfies? ISub sub)
    (TransformedSub. sub (apply a/>>> t ts))

    :else
    (assert false (str "Not a subscription:" sub))))

;; Models combined with cmds (convenient for initialization and update fns)

(defrecord ^:no-doc ModelWithCmd [model cmd])

(defn add-cmd [v cmd]
  (if (nil? cmd)
    v
    (if (instance? ModelWithCmd v)
      (update v :cmd batch-cmds cmd)
      (ModelWithCmd. v cmd))))

(defn extract-model+cmd [v]
  (if (instance? ModelWithCmd v)
    [(:model v) (:cmd v)]
    [v nil]))

(defn update-model [m & args]
  (let [[m' c] (extract-model+cmd m)]
    (add-cmd (apply update m' args) c)))

(defn model->* [m & fs]
  (reduce (fn [m f]
            (let [[m' cmd] (extract-model+cmd m)
                  [m'' cmd'] (extract-model+cmd (f m'))]
              (-> m''
                  (add-cmd cmd)
                  (add-cmd cmd'))))
          m
          fs))

#?(:clj
   (defmacro model-> [m & exprs]
     (model->* m (map (fn [expr]
                        (if (list? expr)
                          `(fn [v] (~(first expr) v ~@(rest expr)))
                          `(fn [v] (~expr v))))
                      exprs))))

#?(:cljs
   (defrecord ^:no-doc EventListenerSub [^js/EventTarget target event-name use-capture?]
              ISub
              (-subscribe! [this context]
                (let [f (fn [e]
                          (dispatch! context e))]
                  (.addEventListener target event-name f use-capture?)
                  f))
              (-unsubscribe! [this f]
                (.removeEventListener target event-name f use-capture?))))

#?(:cljs
   (defn event-listener-sub [^:js/EventTarget target event-name use-capture?]
     (EventListenerSub. target event-name use-capture?)))
