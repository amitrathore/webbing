(ns org.rathore.amit.webbing.web-server-2)

(import '(com.sun.grizzly.http SelectorThread))
(import '(com.sun.grizzly.http.embed GrizzlyWebServer))
(import '(com.sun.grizzly.tcp.http11 GrizzlyAdapter))
(import '(com.sun.grizzly.util.buf ByteChunk))
(import '(java.net HttpURLConnection))
(require '(org.danlarkin [json :as json]))
(use 'org.rathore.amit.webbing.helper)
(use 'org.rathore.amit.webbing.utils)
(use 'org.rathore.amit.webbing.routes)
(use 'org.rathore.amit.utils.config)
(use 'org.rathore.amit.utils.logger)

(defn handler-for [request handlers]
  (handlers (route-for request handlers)))
    
(defn response-from [handler params is-restful]
  (try
   (if is-restful
     (apply handler params)
     (handler params))
  (catch Exception e
    (log-exception e))))

(defn prepare-response [response-text request]
  (if (is-jsonp? request)
    (str (jsonp-callback request)  "(" (json/encode-to-str response-text) ")")
    response-text))
    
(defn service-http-request [handler-functions request response]
  (binding [*http-helper* (http-helper request response)]
    (let [requested-route (route-for request handler-functions)
	  handler (handler-for request handler-functions)]
      (if handler
	(let [params (params-for request handler-functions)
	      is-restful (is-restful? request)
	      _ (log-message (str (.getServerName request) ":" (.getServerPort request)) "recieved " (if (is-jsonp? request) "jsonp" "regular") "request for (" requested-route  (if is-restful "RESTFUL" "QS")  params ")")
	      response-text (response-from handler params is-restful)]
	  (.println (.getWriter response) (prepare-response response-text request)))
	(log-message "Unable to respond to" (.getRequestURI request))))))

(defn grizzly-adapter-for [handler-functions-as-route-map]
  (proxy [GrizzlyAdapter] []
    (service [req res]
      (with-webbing-bindings 
        (service-http-request handler-functions-as-route-map req res)))))

(defn boot-web-server [handler-functions-as-route-map port]
  (let [gws (GrizzlyWebServer. port)]
    (.addGrizzlyAdapter gws (grizzly-adapter-for handler-functions-as-route-map))
    (log-message "Started webbing-http-gateway on port" port)
    (.start gws)))