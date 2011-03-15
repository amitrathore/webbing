(ns org.rathore.amit.webbing.web-server-2)

(import '(com.sun.grizzly.http SelectorThread))
(import '(com.sun.grizzly.http.embed GrizzlyWebServer))
(import '(com.sun.grizzly.tcp.http11 GrizzlyAdapter))
(import '(com.sun.grizzly.util.buf ByteChunk))
(import '(java.net HttpURLConnection))
(require '(org.danlarkin [json :as json]))
(use 'org.rathore.amit.webbing.helper)
(use 'org.rathore.amit.webbing.utils)
(use 'org.rathore.amit.utils.config)
(use 'org.rathore.amit.utils.logger)
(use 'org.rathore.amit.utils.clojure)
(use 'org.rathore.amit.webbing.formats)
(use 'alex-and-georges.debug-repl)

(def webbing-bindings (ref {}))

(def BLANK "")

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

(defn singularize-values [a-map]
  (if (empty? a-map)
    {}
    (let [kv (fn [e]
	       {(first e) (aget (last e) 0)})]
      (apply merge (map kv a-map)))))

(defn is-get? [request]
  (= (.toUpperCase (str (.getMethod request))) "GET"))

(defn is-post? [request]
  (= (.toUpperCase (str (.getMethod request))) "POST"))

(defn params-map-from [request]
  (let [p-map (into {} (.getParameterMap request))
	singularized (singularize-values p-map)
	p-map (convert-to-nested-map singularized)]
    (if (is-post? request)
      (merge p-map (post-parameters request))
      p-map)))

(defn is-jsonp? [request]
  ((params-map-from request) :jsonp))

(defn jsonp-callback [request]
  ((params-map-from request) :jsonp))

(defn only-jsonp-param? [params-map]
  (and (= 1 (count params-map))
       (= :jsonp (first (keys params-map)))))

(defn is-restful? [request]
  (let [params-map (params-map-from request)]
    (or (empty? params-map)
	(only-jsonp-param? params-map))))

(defn route-for [request handlers]
  (let [registered (keys handlers)
	uri-string (.getRequestURI request)]
    (first (filter #(.startsWith uri-string %) registered))))

(defn handler-for [request handlers]
  (handlers (route-for request handlers)))

(defn parsed-params-from-uri [request handlers]
  (let [uri-string (.getRequestURI request)
	requested-route (route-for request handlers)
	params-string (.substring uri-string (count requested-route))]
    (rest (.split params-string "/"))))

(defn params-for [request handlers]
   (if (is-restful? request)
     (parsed-params-from-uri request handlers)
     (params-map-from request)))

(defn response-from [handler params is-restful]
  (if is-restful
    (apply handler params)
    (handler params)))

(defn prepare-response [response-text request]
  (if (is-jsonp? request)
    (str (jsonp-callback request)  "(" (json/encode-to-str response-text) ")")
    response-text))

(defn reply [servlet-response status content]
  (.setStatus servlet-response status)
  (.println
    (.getWriter servlet-response)
    (or content BLANK)))

(defn service-http-request [handler-functions request response]
  (binding [*http-helper* (http-helper request response)]
    (.setCharacterEncoding request "UTF-8")
    (.setCharacterEncoding response "UTF-8")
    (.setContentType response "text/html;charset=UTF-8")
    (let [requested-route (route-for request handler-functions)
          handler (handler-for request handler-functions)]
      (if handler
        (let [params (params-for request handler-functions)
              is-restful (is-restful? request)]
          (log-message (str (.getServerName request) ":" (.getServerPort request)) "recieved "
                          (if (is-jsonp? request) "jsonp" "regular")
                              "request for (" requested-route  (if is-restful "RESTFUL" "QS")  params ")")
          (try
	    (reply response 200 (prepare-response (response-from handler params is-restful) request))
           (catch Exception e
             (log-exception e (str "Webbing failed processing " requested-route " with arguments:" params)))))
        (do
	  (let [uri (.getRequestURI request)
		msg (str "Unable to respond to" uri)]
	    (log-message msg)
	    (reply response 404 msg)))))))

(defn grizzly-adapter-for [handler-functions-as-route-map]
  (proxy [GrizzlyAdapter] []
    (service [req res]
      (with-webbing-bindings
	(service-http-request handler-functions-as-route-map req res)))))

(defn boot-web-server [handler-functions-as-route-map port]
  (let [gws (GrizzlyWebServer. port)]
    (.setCoreThreads gws 100)
    (.setMaxThreads gws 100)
    (.addGrizzlyAdapter gws (grizzly-adapter-for handler-functions-as-route-map))
    (log-message "Started webbing-http-gateway on port" port)
    (.start gws)))
