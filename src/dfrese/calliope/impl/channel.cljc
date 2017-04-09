(ns dfrese.calliope.impl.channel
  "Channels can be used for pub/sub communication, meaning one can
  always publish a value on a channel, and all subscribers registered
  at that moment are notified of it.")

(defn- next-sub-id [m]
  (inc (apply max 0 (keys m))))

(declare subscribe-to-channel!)
(declare unsubscribe-from-channel!)

(deftype ^:no-doc Channel [subs])

(defn subscribe-to-channel!
  "Subscribe to the given channel with the given function to be called
  for all messages sent on the channel. Returns an id that can be
  passed later to `unsubscribe-from-channel!`."
  [^Channel ch callback]
  (let [id (next-sub-id @(.-subs ch))] ;; not thread-safe
    (swap! (.-subs ch) assoc id callback)
    id))

(defn unsubscribe-from-channel!
  "Remove the subscription with the given id."
  [^Channel ch id]
  (swap! (.-subs ch) dissoc id))

(defn channel
  "Creates a new channel."
  []
  (Channel. (atom {})))

(defn send-to-channel! [^Channel ch v]
  (doseq [f (vals @(.-subs ch))]
    (f v)))
