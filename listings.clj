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
    (println (format "fetching: %s" url))
    (-> (curl/get url)
        :body
        (json/parse-string true))))

(defn completed-last-hour?
  [order]
  (let [date-fmt (java.time.format.DateTimeFormatter/ofPattern "M/d/y h:m:s a")
        parsed-time  (java.time.LocalDateTime/parse (:date order) date-fmt)
        one-hr-ago (.minusHours (java.time.LocalDateTime/now (.-UTC java.time.ZoneOffset)) 1)]
    (.isAfter parsed-time one-hr-ago)))

(defn to-int
  [price]
  (Integer/parseInt (str/replace price #"," "")))

(defn latest-orders-for-listing
  [listing]
  (->> (filterv #(completed-last-hour? %) (:completed_orders listing))
       (mapv #(update % :price to-int))))

(defn partition-orders
  [orders]
  (if (empty? orders)
    '()
    (let [prices (mapv #(:price %) orders)
          min (apply min prices)
          max (apply max prices)
          mid (/ (+ min max) 2)]
      (partition-by #(< mid (:price %)) (sort-by :price orders)))))

(defn is-profitable?
  [best-sell best-buy min-profit]
  (let [tax (* 0.15 best-sell)
        adjusted-sell (- best-sell tax)]
    (> adjusted-sell best-buy)))

(defn find-most-traded
  []
  (let [listings (get-listings "mlb_card" 4)
        enhanced (for [listing listings
                       :when (is-profitable? (:best_sell_price listing) (:best_buy_price listing) 100)]
                   (let [partitioned-orders (partition-orders (latest-orders-for-listing (get-listing (get-in listing [:item :uuid]))))
                         buys (second partitioned-orders)
                         sells (first partitioned-orders)]
                     (assoc listing :buys buys :sells sells :activity (+ (count buys) (count sells)))))]
    (reverse (sort-by :activity enhanced))))

(doseq [listing (find-most-traded)]
  (let [name (:listing_name listing)
        uuid (get-in listing [:item :uuid])
        activity (:activity listing)
        best_buy (:best_buy_price listing)
        best_sell (:best_sell_price listing)
        buy_count (count (:buys listing))
        sell_count (count (:sells listing))]
    (when (> activity 50)
      (println (format "%-20s %20s -- activity: %4d\tbuy: %7d (%3d)\tsell: %7d (%3d)\n" name uuid activity best_buy buy_count best_sell sell_count)))))
