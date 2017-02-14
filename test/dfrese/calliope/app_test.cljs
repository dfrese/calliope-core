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

(defn test-canvas [at]
  (reify app/ICanvas
    (normalize-view [this v] v)
    (init-canvas! [this element] nil)
    (update-canvas! [this state element v msg-callback] (reset! at v))
    (finish-canvas! [this state element] (reset! at nil))))

(deftest app-test
  (testing "updates dom on sub messages"
    (let [doc (.-body js/document)
          view-atom (atom nil)
          canvas (test-canvas view-atom)
          init "Hello"
          view (fn [txt]
                 [:div txt])
          update (fn [model txt]
                   txt)
          [sub upd-sub!] (test-sub)
          subscription (fn [model]
                         [sub])]
      (app/start! doc (app/app canvas init view update subscription))

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
              view-atom (atom nil)
              canvas (test-canvas view-atom)
          
              [cmd upd-cmd!] (test-cmd)
              init (core/+cmd "Hello" cmd)
              view (fn [txt]
                     [:div txt])
              update (fn [model txt]
                       txt)
              subscription (fn [model]
                             nil)]
          (app/start! doc (app/app canvas init view update subscription))

          (let [test (fn []
                       ;; (.-wholeText (.-firstChild (.-firstChild (.-body js/document))))
                       @view-atom)]
            (is (= [:div "Hello"] (test)))
            (@upd-cmd! "World")
            (is (= [:div "World"] (test))))))))
  ;; TODO: add/remove cmds/subs on model changes
  )


