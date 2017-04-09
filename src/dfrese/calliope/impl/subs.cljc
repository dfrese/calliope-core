(ns dfrese.calliope.impl.subs
  (:require [dfrese.calliope.ext :as ext]
            [dfrese.clj.functions :as f]))

(defrecord ^:no-doc TransformedSub [base t]
  ext/ISub
  (-subscribe! [this context]
    (ext/-subscribe! base (ext/update-dispatcher context #(f/comp % t))))
  (-unsubscribe! [this id]
    (ext/-unsubscribe! base id)))

(defn translate-sub [sub t & ts]
  (cond
    (vector? sub)
    (mapv #(apply translate-sub % t ts)
          sub)
    
    (instance? TransformedSub sub)
    (TransformedSub. (.-base ^TransformedSub sub)
                     (apply f/comp (concat ts [t (.-t ^TransformedSub sub)])))
    
    (satisfies? ext/ISub sub)
    (TransformedSub. sub (apply f/comp (concat ts [t])))

    :else
    (assert false (str "Not a subscription:" sub))))
