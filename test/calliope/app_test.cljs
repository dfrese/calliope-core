(ns calliope.app-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [calliope.core :as core]
            [calliope.app :as app]
            [orpheus.html :as html]))

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

(deftest app-test
  (testing "updates dom on sub messages"
    (let [doc (.-body js/document)
          init "Hello"
          view (fn [txt]
                 (html/div {} txt))
          update (fn [model txt]
                   txt)
          [sub upd-sub!] (test-sub)
          subscription (fn [model]
                         [sub])]
      (app/run doc init view update subscription)

      (let [test (fn []
                   (.-wholeText (.-firstChild (.-firstChild (.-body js/document)))))]
        (is (= "Hello" (test)))
        (@upd-sub! "World")
        (is (= "World" (test))))))
          
  (testing "updates dom on cmd messages"
    (with-print-stacktrace
      (fn [] (let [doc (.-body js/document)
                    
          [cmd upd-cmd!] (test-cmd)
          init (core/+cmd "Hello" cmd)
          view (fn [txt]
                 (html/div {} txt))
          update (fn [model txt]
                   txt)
          subscription (fn [model]
                         nil)]
      (app/run doc init view update subscription)

      (let [test (fn []
                   (.-wholeText (.-firstChild (.-firstChild (.-body js/document)))))]
        (is (= "Hello" (test)))
        (@upd-cmd! "World")
        (is (= "World" (test))))))))
  ;; TODO: add/remove cmds/subs on model changes
  )


