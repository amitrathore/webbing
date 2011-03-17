(ns org.rathore.amit.webbing.formats
  (:use
    org.rathore.amit.utils.logger)
  (:import
    [com.sun.grizzly.tcp.http11 GrizzlyRequest]
    [java.io InputStream]
    [java.nio ByteBuffer]))

(defn concat-byte-buffers [& bufs]
  (if (= 1 (count bufs))
    (.rewind ^ByteBuffer (first bufs))
    (let [bufs (map #(.rewind ^ByteBuffer %) bufs)
	  size (apply + (map #(.remaining ^ByteBuffer %) bufs))
	  buf (ByteBuffer/allocate size)]
      (doseq [b bufs]
	(.put buf b))
      (.rewind buf))))

(defn input-stream->byte-buffer
  [^InputStream stream]
  (let [available (.available stream)]
    (loop [ary ^bytes (byte-array (if (pos? available) available 1024)), offset 0, bufs []]
      (let [ary-len (count ary)]
	(if (= ary-len offset)
	  (recur (byte-array 1024) 0 (conj bufs (ByteBuffer/wrap ary)))
	  (let [byte-count (.read stream ary offset (- ary-len offset))]
	    (if (neg? byte-count)
	      (apply concat-byte-buffers (conj bufs (ByteBuffer/wrap ary 0 offset)))
	      (recur ary (+ offset byte-count) bufs))))))))

(defn byte-buffer->string
  ([buf]
     (byte-buffer->string buf "UTF-8"))
  ([^ByteBuffer buf charset]
     (when buf
       (let [ary (byte-array (.remaining buf))]
	 (.get buf ary)
	 (String. ary ^String charset)))))

(def escaped-characters
  {"%99" "\u2122"
   "%AE" "%A8"})

(defn body-parameters [^ByteBuffer buf]
  (let [body (-> buf byte-buffer->string)
	split-params (when-not (empty? body) (seq (.split ^String body "[&=]")))
	parameter-map (apply hash-map split-params)
	decoded-parameters (zipmap
			     (map #(keyword (.replace ^String % "_" "-")) (keys parameter-map))
			     (map
			       (fn [value]
				 (java.net.URLDecoder/decode
				   (reduce
				     (fn [value [from to]]
				       (.replace value from to))
				     value
				     escaped-characters)
				   "utf-8"))
			       (vals parameter-map)))]
    decoded-parameters))

(defn post-parameters [^GrizzlyRequest request]
  (-> request .getInputStream input-stream->byte-buffer body-parameters))
