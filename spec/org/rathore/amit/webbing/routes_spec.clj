(ns org.rathore.amit.webbing.routes-spec
  (:use clojure.test 
        org.rathore.amit.webbing.routes))

(def TEST-PATHS [
  "/hello/simple"
  "/merchant/$merchant_id"
])

(def registered-keys (keys TEST-ROUTES))

;;http://cheshire-production.runa.com/merchant/$merchant_id/consumer/$site_cid/cart

(deftest test-simple-routes
  (is (= "/hello/simple" (route-for-uri-string "/hello/simple" registered-keys)))
  (is (= "/hello/simple" (route-for-uri-string "/hello/simple?name=kyle" registered-keys)))
  (is (= "/hello/simple" (route-for-uri-string "/hello/simple?name=kyle&age=35" registered-keys))))