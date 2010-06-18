(ns org.rathore.amit.webbing.routes
  (:use org.rathore.amit.webbing.utils
        org.rathore.amit.webbing.routes))

(defn params-map-from [request]
  (let [p-map (into {} (.getParameterMap request))
	singularized (singularize-values p-map)]
    (convert-to-nested-map singularized)))

(defn only-jsonp-param? [params-map]
  (and (= 1 (count params-map))
       (= :jsonp (first (keys params-map)))))

(defn is-restful? [request]
  (let [params-map (params-map-from request)]
    (or (empty? params-map) 
	(only-jsonp-param? params-map))))

(defn is-get? [request]
  (= (.toUpperCase (str (.getMethod request))) "GET"))

(defn route-for-uri-string [uri-string registered-keys]
  (first (filter #(.startsWith uri-string %) registered-keys)))

(defn route-for [request handlers]
  (let [registered (keys handlers)
	uri-string (.getRequestURI request)]
    (route-for-uri-string uri-string registered)))

(defn parsed-params-from-uri [request handlers]
  (let [uri-string (.getRequestURI request)
	requested-route (route-for request handlers)
	params-string (.substring uri-string (count requested-route))]
    (rest (.split params-string "/"))))

(defn params-for [request handlers]
   (if (is-restful? request)
     (parsed-params-from-uri request handlers)
     (params-map-from request)))

(defn is-jsonp? [request]
  ((params-map-from request) :jsonp))

(defn jsonp-callback [request]
  ((params-map-from request) :jsonp))
