(ns org.rathore.amit.webbing.utils
  (:use clojure.walk
        org.rathore.amit.utils.logger))

(filter (complement empty?) (seq (.split "d[general][client_time]" "[\\[\\]]")))

(defn- break-key-for-nesting [nested-key]
  (let [tokens (seq (.split nested-key "[\\[\\]]"))
	filtered (filter (complement empty?) tokens)]
    (map #(.replace % "_" "-") filtered)))

(defn- insert-nested-keys [nested-key value container]
  (let [broken (break-key-for-nesting nested-key)]
    (update-in container broken (constantly value))))

(defn convert-to-nested-map [singularized]
  (let [converter (fn [container [k v]]
		    (insert-nested-keys k v container))
	stringized (reduce converter {} singularized)
	keywordized (keywordize-keys stringized)]
    (merge keywordized stringized)))

(defn log-queued [gws]
  (log-message "Number of queued requests:" (.getCountQueued (.getThreadPoolStatistics (.getStatistics gws)))))

(defn periodically-log-queued [gws time-interval-millis]
  (.startGatheringStatistics (.getStatistics gws))
  (log-queued gws)
  (Thread/sleep time-interval-millis)
  (recur gws time-interval-millis))