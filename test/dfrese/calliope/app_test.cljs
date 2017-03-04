(ns dfrese.calliope.app-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.calliope.core :as core]
            [dfrese.calliope.app :as app]))

(defrecord TestSub [f]
  core/ISub
  (-subscribe! [this context]
    (reset! f #(core/dispatch! context %))
    nil)
  (-unsubscribe! [this id]
    (reset! f nil)))

(defn test-sub []
  (let [a (atom nil)]
    [(TestSub. a) a]))

(defrecord TestCmd [f]
  core/ICmd
  (-run! [this context]
    (reset! f #(core/dispatch! context %))))

(defn test-cmd []
  (let [a (atom nil)]
    [(TestCmd. a) a]))

(defn with-print-stacktrace [f]
  (try
    (f)
    (catch :default e
      (println (.-stack e))
      (throw e))))

(defn test-canvas [at view]
  (reify app/ICanvas
    (init-canvas! [this element] ::state)
    (update-canvas! [this state element model msg-callback]
      (assert (= ::state state))
      (reset! at (view model))
      state)
    (finish-canvas! [this state element]
      (assert (= ::state state))
      (reset! at nil))))

(deftest app-test
  (testing "updates dom on sub messages"
    (let [doc (.-body js/document)
          view-atom (atom nil)
          view (fn [txt]
                 [:div txt])
          canvas (test-canvas view-atom view)
          init "Hello"
          update (fn [model txt]
                   txt)
          [sub upd-sub!] (test-sub)
          subscription (fn [model]
                         [sub])]
      (app/start! doc (app/app canvas init update subscription))

      (let [test (fn []
                   ;; (.-wholeText (.-firstChild (.-firstChild (.-body js/document))))
                   @view-atom)]
        (is (= [:div "Hello"] (test)))
        (@upd-sub! "World")
        (is (= [:div "World"] (test))))))
          
  (testing "updates dom on cmd messages"
    (with-print-stacktrace
      (fn []
        (let [doc (.-body js/document)
              view (fn [txt]
                     [:div txt])
              view-atom (atom nil)
              canvas (test-canvas view-atom view)
          
              [cmd upd-cmd!] (test-cmd)
              init (core/+cmd "Hello" cmd)
              update (fn [model txt]
                       txt)
              subscription (fn [model]
                             nil)]
          (app/start! doc (app/app canvas init update subscription))

          (let [test (fn []
                       ;; (.-wholeText (.-firstChild (.-firstChild (.-body js/document))))
                       @view-atom)]
            (is (= [:div "Hello"] (test)))
            (@upd-cmd! "World")
            (is (= [:div "World"] (test)))

            (@upd-cmd! "Hello World")
            (is (= [:div "Hello World"] (test)))
            )))))
  ;; TODO: add/remove cmds/subs on model changes
  )


