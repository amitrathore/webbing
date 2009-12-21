(ns webbing-spec
 (:use [clojure.test :only [run-tests deftest is]])
 (:use org.rathore.amit.webbing.utils))

(def params {
	     "d[general][page_type]" "undefined" , 
	     "jsonp" "jsonp1261181538202", 
	     "d[runa_page_type][page_type]" "home_page", 
	     "d[general][client_time]" "1261181538285", 
	     "d[general][request_url]" "http://trial-hotel.com/home.html", 
	     "d[general][cid]" "encrypted-consumer-id-1261103840540", 
	     "d[general][referrer]" "", 
	     "d[general][merchant_session_id]" "EBCDIC", 
	     "d[general][timezone_offset]" "480", 
	     "d[general][merchant_id]" "1c030cab-5cbf-c62c-5a3c-287af5b8822b",
	     "_" "1261181538286"
})

(deftest converting-to-nested-map
  (let [nested (convert-to-nested-map params)]
    (is (map? (nested "d")))
    (let [d (nested "d")
	  general (d "general")]
      (is (= (general "client-time") "1261181538285"))
      (is (= (general "request-url") "http://trial-hotel.com/home.html"))
      (is (= (general "cid") "encrypted-consumer-id-1261103840540"))
      (is (= (general "referrer") ""))
      (is (= (general "merchant-session-id") "EBCDIC"))
      (is (= (general "timezone-offset") "480"))
      (is (= (general "merchant-id") "1c030cab-5cbf-c62c-5a3c-287af5b8822b"))
      (is (= (nested "-") "1261181538286"))
      (is (= (nested "jsonp") "jsonp1261181538202")))))