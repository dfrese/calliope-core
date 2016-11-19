(ns calliope.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [calliope.core :as core :include-macros true]))

(deftest context-test
  (let [d! (fn [_] nil)]
    (is (= d!
           (core/dispatcher (core/context d!))))
    (is (= d!
           (core/dispatcher (core/update-dispatcher (core/context :x)
                                                    (constantly d!)))))))

(deftest model+cmd-test
  (is (= [42 nil]
         (core/extract-model+cmd 42)))
  (is (= [42 :cmd]
         (core/extract-model+cmd (core/+cmd 42 :cmd))))
  (is (= (core/+cmd 43 :cmd)
         (core/update-model (core/+cmd 42 :cmd)
                            inc)))
  (is (= (core/+cmd 43 :cmd)
         (core/model-> 42
                       (core/+cmd :cmd)
                       inc))))

;; TODO: sub->, cmd-> and more
