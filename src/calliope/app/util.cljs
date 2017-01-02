(ns calliope.app.util
  (:require [orpheus.core :as orpheus]
            [orpheus.lift :as lift]
            [orpheus.patch :as patch]))

;; DOM as the rendering target

(defn- ->props [html]
  ;; conveniently be a little relaxed about the return value of 'view'
  (cond
    (vector? html) {"childNodes" html}
    (orpheus/velement? html) {"childNodes" [html]}
    :else html ;; (map? html)
    ))

(defn ^:no-doc clear-element! [element]
  (doseq [c (vec (array-seq (.-childNodes element)))]
    (.removeChild element c)))

(defn ^:no-doc init-canvas! [element]
  ;; try to lift the current dom structure as vdom properties (gives
  ;; smooth first update if the html already matches the initial vdom)
  (lift/lift-properties element))

(defn ^:no-doc update-canvas! [state element new-html msg-callback]
  (patch/patch-properties! element state (->props new-html)
                           {:dispatch! msg-callback}))

(defn finish-canvas! [prev-html element]
  (patch/patch-properties! element (->props prev-html) {}
                           {}))

(defn ^:no-doc flush-canvas! [state element]
  state)

;; (defn element [^js/Element element]
;;   element)

;; (defn element-by-id [^js/DOMDocument document id]
;;   (.getElementById document id))

;; (defn page [^js/DOMDocument document]
;;   (.-body document))
