(ns calliope.browser-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [calliope.browser :as browser]
            [calliope.core :as core]))

(deftest location-hash-test
  (testing "get command"
    (let [c (browser/get-location-hash js/window)
          a (atom nil)]
      (set! (.-hash (.-location js/window)) "#foo")
      (core/-run! c (core/context (fn [v]
                                    (reset! a v))))
      (is (= "#foo" @a))))
  (testing "set command"
    (let [c (browser/set-location-hash js/window "#foo")]
      (set! (.-hash (.-location js/window)) "")
      (core/-run! c (core/context (fn [v]
                                    (is false))))
      (is (= "#foo" (.-hash (.-location js/window)))))))

(deftest location-hash-updates-test
  (async done
         (let [s (browser/location-hash-updates js/window)
               a (atom nil)]
           (set! (.-hash (.-location js/window)) "")
           (core/-subscribe! s (core/context (fn [v]
                                               (is (= v "#foo"))
                                               (done))))
           (set! (.-hash (.-location js/window)) "#foo"))))
