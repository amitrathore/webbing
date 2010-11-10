(ns org.rathore.amit.webbing.helper
  (:use org.rathore.amit.utils.clojure)
  (:use org.rathore.amit.utils.logger)
  (:use clojure.contrib.math))

(import '(com.sun.grizzly.util.http Cookie)
	'(nl.bitwalker.useragentutils UserAgent))

(def *http-helper* :__init__)

(defn cookie-hash [request]
  (let [cookies (.getCookies request)
	kv (fn [c] {(.getName c) (.getValue c)})]
    (apply merge (map kv cookies))))

(defn browser-version-num-from [user-agent]
  (/ (mod (.getId (.getBrowser user-agent)) (expt 2 8)) 10.))

(defn browser-detector [request]
  (let [user-agent (.getHeader request "user-agent")]
    (if-not (empty? user-agent)
      (UserAgent/parseUserAgentString user-agent))))

(defn http-helper [request response]
  (let [user-agent (browser-detector request)
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
	:ip-address (first (.split (.getHeader request "x-forwarded-for") ","))
	:browser-name (if user-agent (.getName (.getBrowser user-agent)))
	:browser-version (if user-agent (browser-version-num-from user-agent))
	:operating-system (if user-agent (.getName (.getOperatingSystem user-agent)))
	(throw (Exception. (str "Response-helper: Unknown command, " command)))))))

(defn read-cookie [name] (*http-helper* :read-cookie name))
(defn set-cookie [name value age] (*http-helper* :add-cookie name value age))
(defn requester-ip [] (*http-helper* :ip-address))
(defn browser-name [] (*http-helper* :browser-name))
(defn browser-version [] (*http-helper* :browser-version))
(defn operating-system [] (*http-helper* :operating-system))

(defn requestor-details [] {
     :browser-name (browser-name)
     :browser-version (browser-version)
     :operating-system (operating-system)
     :ip-address (requester-ip)})

(defmacro defwebmethod [method-name params & exprs]
  `(defn ~method-name [{:keys ~params}]
     ~@exprs))
