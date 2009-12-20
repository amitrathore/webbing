(ns org.rathore.amit.webbing.utils)

(filter (complement empty?) (seq (.split "d[general][client_time]" "[\\[\\]]")))

(defn break-key-for-nesting [nested-key]
  (let [tokens (seq (.split nested-key "[\\[\\]]"))]
    (filter (complement empty?) tokens)))

(defn insert-nested-keys [nested-key value container]
  (let [broken (break-key-for-nesting nested-key)]
    (update-in container broken (constantly value))))

(defn convert-to-nested-map [singularized]
  (let [nested-keys (keys singularized)
	nested {}]
    
    )
  )