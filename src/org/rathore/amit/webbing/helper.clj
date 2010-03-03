(ns org.rathore.amit.webbing.helper
  (:use org.rathore.amit.utils.clojure)
  (:use org.rathore.amit.utils.logger))

(import '(com.sun.grizzly.util.http Cookie)
	'(org.apache.turbine.util BrowserDetector))

(def *http-helper* :__init__)

(defn cookie-hash [request]
  (let [cookies (.getCookies request)
	kv (fn [c] {(.getName c) (.getValue c)})]
    (apply merge (map kv cookies))))

(defn browser-detector [request]
  (let [user-agent (.getHeader request "user-agent")]
    (if user-agent
      (BrowserDetector. user-agent))))

(defn http-helper [request response]
  (let [browser (browser-detector request)
	cookies (cookie-hash request)
	add-cookie (fn [name value age]
                     (let [cookie (Cookie. name value)]
                       (.setMaxAge cookie age)
                       (.setPath cookie "/")
                       (.addCookie response cookie))
                     value)
	read-cookie (fn [name]
		      (if-not (empty? cookies)
			      (cookies name)))]
    (fn [command & args]
      (condp = command
	:add-cookie (apply add-cookie args)
	:read-cookie (apply read-cookie args)
	:ip-address (.getRemoteAddr request)
	:browser-name (if browser (.getBrowserName browser))
	:browser-version (if browser (.getBrowserVersion browser))
	:operating-system (if browser (.getBrowserPlatform browser))
	(throw (Exception. (str "Response-helper: Unknown command, " command)))))))

(defn read-cookie [name] (*http-helper* :read-cookie name))
(defn set-cookie [name value age] (*http-helper* :add-cookie name value age))
(defn requester-ip [] (*http-helper* :ip-address))
(defn browser-name [] (*http-helper* :browser-name))
(defn browser-version [] (*http-helper* :browser-version))
(defn operating-system [] (*http-helper* :operating-system))

;(defmacro defwebmethod [method-name params & exprs]
;  `(def-hash-method ~method-name ~params ~@exprs))

(defmacro defwebmethod [method-name params & exprs]
  `(defn ~method-name [{:keys ~params}]
     ~@exprs))