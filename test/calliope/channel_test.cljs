(ns calliope.channel-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [calliope.channel :as ch]))

(deftest channel-test
  (testing "send works, without subscribers"
    (ch/send-to-channel! (ch/channel) 42))
  (testing "subscribe and unsubscribe, receives the messages."
    (let [p1 (ch/channel)
          res (atom nil)
          id (ch/subscribe-to-channel! p1
                                     (fn [v]
                                       (reset! res v)))]
      (ch/send-to-channel! p1 42)
      (is (= 42 @res))
      (ch/unsubscribe-from-channel! p1 id)
      (ch/send-to-channel! p1 21)
      (is (not= 21 @res)))))
