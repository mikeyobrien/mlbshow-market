(require '[babashka.curl :as curl]
         '[cheshire.core :as json])

(def base-url "https://mlb21.theshow.com")
(def total-pages (-> (curl/get (format "%s/%s" base-url "/apis/listings.json?type=mlb_card"))
                    :body
                    (json/parse-string true)
                    :total_pages))

(defn get-listings-uri
  [{:keys [type rank rarity page] :as params}]
  (let [param-string (for [[k v] (seq params)]
                       (format "%s=%s" (name k) v))]
    (format "%s/apis/listings.json?%s" base-url (str/join "&" param-string))))


(defn get-listings
  [type max-pages]
  (let [request-url (get-listings-uri {:type type :page 1})]
    (loop [request-url request-url
           listings []]
      (let [body (-> (curl/get request-url)
                     :body
                     (json/parse-string true))
            curr-page (:page body)
            total-pages (:total_pages body)]
        (if (or (= curr-page total-pages)
                (= curr-page (inc max-pages)))
          listings
          (recur (get-listings-uri {:type type :page (inc curr-page)})
                 (into [] (concat listings (:listings body)))))))))

(defn get-listing
  [uuid]
  (let [url (format "%s/apis/listing.json?uuid=%s" base-url uuid)]
    (-> (curl/get url)
        :body
        (json/parse-string true))))

;(count (get-listings "mlb_card" 5))


(def date-fmt (java.time.format.DateTimeFormatter/ofPattern "M/d/y h:m:s a"))
(doseq [order (:completed_orders (get-listing "44007f34034857cf878ebe2f0fa15e06"))]
  (let [parsed-time  (java.time.LocalDateTime/parse (:date order) date-fmt)
        formatted-time (.format parsed-time (java.time.format.DateTimeFormatter/ofPattern "dd-MM-yyyy HH:mm:ss"))]
    (println (format "%s -- %s before %s" formatted-time (:price order) (.isAfter parsed-time (java.time.LocalDateTime/now))))))
