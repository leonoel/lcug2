(ns lcug2.core
  (:require [clj-http.client :as http]
            [lcug2.html :as html]
            [lcug2.prez :as prez]))

(defn split-links [url links]
  (let [{int true, ext false}
        (->> links
             (sequence (comp (map (partial html/in-context url)) (remove nil?)))
             (group-by (comp (partial = (html/base url)) html/base)))]
    {:internals (into #{} (map html/resource) int)
     :externals (into #{} (map str) ext)}))

(defn http-get [result error url]
  (http/get url {:async? true
                 :follow-redirects false}
            result error))

(def redirect? #{301 302 307})

(defn parse-response [url {:keys [status headers body]}]
  (if (redirect? status)
    (-> url
        (split-links [(get headers "Location")])
        (assoc :redirect status))
    (let [html (html/parse body)]
      (-> url
          (split-links (html/links-from html))
          (assoc :words (frequencies (html/words-from html)))))))

(defn crawl-url [url]
  (comp (prez/just url)
        (prez/async http-get)
        (map (partial parse-response url))))

(defn init-state [mp cb seed]
  {:max-pending mp
   :callback    cb
   :base        (html/base seed)
   :crawled     {}
   :pending     #{}
   :waiting     #{(html/resource seed)}})

(declare conj-result)

(defn parse-error [e]
  {:error (or (:status (ex-data e)) (str e))})

(defn check-completion [{:keys [max-pending callback base crawled pending waiting] :as state}]
  (cond
    (= (count pending) max-pending) state
    (seq waiting) (let [resource (first waiting)
                        on-result (prez/ps *agent* conj-result resource)]
                    (prez/| on-result (comp on-result parse-error)
                            (crawl-url (str base resource)))
                    (recur (assoc state :pending (conj pending resource)
                                        :waiting (disj waiting resource))))
    (seq pending) state
    :else (do (callback crawled) (reduced state))))

(defn conj-result [{:keys [crawled waiting pending] :as state} resource result]
  (-> state
      (assoc :crawled (assoc crawled resource result)
             :waiting (into waiting (remove #(or (crawled %) (pending %) (waiting %))) (:internals result))
             :pending (disj pending resource))
      (check-completion)))

(defn crawl [callback seed & {:keys [max-pending] :or {max-pending 6}}]
  (send (agent (init-state max-pending callback seed)) check-completion))
