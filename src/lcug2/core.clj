(ns lcug2.core
  (:require [clj-http.client :as http]
            [lcug2.html :as html]))

(defn split-links [url links]
  (let [{int true, ext false}
        (->> links
             (map (partial html/in-context url))
             (remove nil?)
             (group-by (comp (partial = (html/base url)) html/base)))]
    {:internals (->> int (map html/resource) (into #{}))
     :externals (->> ext (map str) (into #{}))}))

(def redirect? #{301 302 307})

(defn crawl-url [on-result url]
  (http/get url {:async? true
                 :follow-redirects false}
            (fn [{:keys [status headers body]}]
              (if (redirect? status)
                (-> url
                    (split-links [(get headers "Location")])
                    (assoc :redirect status)
                    (on-result))
                (try
                  (let [html (html/parse body)]
                    (-> url
                        (split-links (html/links-from html))
                        (assoc :words (frequencies (html/words-from html)))
                        (on-result)))
                  (catch Exception ex
                    (on-result {:error (.getMessage ex)})))))
            (fn [ex]
              (on-result {:error (:status (ex-data ex))}))))

(defn init-state [mp cb seed]
  {:max-pending mp
   :callback    cb
   :base        (html/base seed)
   :crawled     {}
   :pending     #{}
   :waiting     #{(html/resource seed)}})

(declare conj-result)

(defn check-completion [{:keys [max-pending callback base crawled pending waiting] :as state}]
  (cond
    (= (count pending) max-pending) state
    (seq waiting) (let [resource (first waiting)]
                    (crawl-url (partial send *agent* conj-result resource)
                               (str base resource))
                    (recur (assoc state :pending (conj pending resource)
                                        :waiting (disj waiting resource))))
    (seq pending) state
    :else (do (callback crawled) (reduced state))))

(defn conj-result [{:keys [crawled waiting pending] :as state} resource result]
  (-> state
      (assoc :crawled (assoc crawled resource result)
             :waiting (into waiting (remove #(or (crawled %) (pending %) (waiting %)) (:internals result)))
             :pending (disj pending resource))
      (check-completion)))

(defn crawl [callback seed & {:keys [max-pending] :or {max-pending 6}}]
  (send (agent (init-state max-pending callback seed)) check-completion))
