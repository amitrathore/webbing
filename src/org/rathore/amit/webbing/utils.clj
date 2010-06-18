(ns org.rathore.amit.webbing.utils
  (:use clojure.walk
        org.rathore.amit.utils.clojure))

(def webbing-bindings (ref {}))

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
	stringized (reduce converter {} singularized)]
    (keywordize-keys stringized)))

(defn singularize-values [a-map]
  (if (empty? a-map)
    {}
    (let [kv (fn [e]
	       {(first e) (aget (last e) 0)})]
      (apply merge (map kv a-map)))))

(defmacro with-webbing-bindings [body]
  `(do
     (push-thread-bindings @webbing-bindings)
     (try ~body
     (finally (pop-thread-bindings)))))

(defmacro register-bindings [bindings]
  `(dosync (ref-set webbing-bindings (hash-map ~@(var-ize bindings)))))

(defmacro binding-for-webbing [bindings & expr]
  `(do
     (register-bindings ~bindings)
     (binding [~@bindings] ~@expr)))

