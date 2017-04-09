(ns dfrese.calliope.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            #?(:cljs [dfrese.calliope.core :as core :include-macros true])
            #?(:clj [dfrese.calliope.core :as core])
            [dfrese.calliope.ext :as ext]))

(deftest context-test
  (let [d! (fn [_] nil)]
    (is (= d!
           (ext/dispatcher (ext/context d!))))
    (is (= d!
           (ext/dispatcher (ext/update-dispatcher (ext/context :x)
                                                  (constantly d!)))))))

(deftest model+cmd-test
  (is (= [42 nil]
         (core/extract-model+cmd 42)))
  (is (= [42 :cmd]
         (core/extract-model+cmd (core/add-cmd 42 :cmd))))
  (is (= (core/add-cmd 43 :cmd)
         (core/update-model (core/add-cmd 42 :cmd)
                            inc)))
  (is (= (core/add-cmd 43 :cmd)
         (core/model-> 42
                       (core/add-cmd :cmd)
                       inc))))

;; TODO: sub->, cmd-> and more

(defrecord TestSub [f]
  ext/ISub
  (-subscribe! [this context]
    (reset! f #(ext/dispatch! context %))
    nil)
  (-unsubscribe! [this id]
    (reset! f nil)))

(defn test-sub []
  (let [a (atom nil)]
    [(TestSub. a) a]))

(defrecord TestCmd [f]
  ext/ICmd
  (-run! [this context]
    (reset! f #(ext/dispatch! context %))))

(defn test-cmd []
  (let [a (atom nil)]
    [(TestCmd. a) a]))

(defn with-print-stacktrace [f]
  #?(:clj (f))
  #?(:cljs (try
    (f)
    (catch :default e
      (println (.-stack e))
      (throw e)))))

(defn test-canvas [at view]
  (reify ext/ICanvas
    (-init-canvas! [this element] ::state)
    (-update-canvas! [this state element model msg-callback]
      (assert (= ::state state))
      (reset! at (view model))
      state)
    (-finish-canvas! [this state element]
      (assert (= ::state state))
      (reset! at nil))))

(deftest app-test
  (testing "updates canvas on sub messages"
    (let [doc ::doc
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
      (core/start! doc (core/app canvas init update subscription))

      (let [test (fn []
                   @view-atom)]
        (is (= [:div "Hello"] (test)))
        (@upd-sub! "World")
        (is (= [:div "World"] (test))))))
          
  (testing "updates canvas on cmd messages"
    (with-print-stacktrace
      (fn []
        (let [doc ::doc
              view (fn [txt]
                     [:div txt])
              view-atom (atom nil)
              canvas (test-canvas view-atom view)
          
              [cmd upd-cmd!] (test-cmd)
              init (core/add-cmd "Hello" cmd)
              update (fn [model txt]
                       txt)
              subscription (fn [model]
                             nil)]
          (core/start! doc (core/app canvas init update subscription))

          (let [test (fn []
                       @view-atom)]
            (is (= [:div "Hello"] (test)))
            (@upd-cmd! "World")
            (is (= [:div "World"] (test)))

            (@upd-cmd! "Hello World")
            (is (= [:div "Hello World"] (test)))
            )))))
  ;; TODO: add/remove cmds/subs on model changes
  )


