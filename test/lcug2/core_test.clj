(ns lcug2.core-test
  (:require [lcug2.core :refer [crawl]]
            [clojure.test :refer [deftest testing is]]
            [clojure.java.io :refer [resource as-file]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.resource :refer [wrap-resource]])
  (:import (java.io File)))

(def websites-path "websites")

(def timeout 5000)

(def next-port! (partial swap! (atom 9000) inc))

(defn wrap-index [handler path]
  (fn [{:keys [uri] :as request}]
    (handler (if (= uri "/") (assoc request :uri (str uri path)) request))))

(defn read-resource [name]
  (-> name
      (resource)
      (slurp)
      (read-string)))

(defn list-dirs [path]
  (let [dir (-> path (resource) (as-file))]
    (->> (.list ^File dir)
         (sort))))

(defn app [name]
  (-> (constantly {:status 404, :body "Not found"})
      (wrap-resource (str websites-path "/" name "/pages"))
      (wrap-index "index.html")))

(defn test-crawler [name]
  (testing name
    (let [port (next-port!)
          server (run-jetty (app name) {:join? false, :port port})
          crawled (promise)
          stopped (future (is (= (deref crawled timeout :timeout)
                                 (read-resource (str websites-path "/" name "/pages.edn"))))
                          (.stop server))]
      (crawl (partial deliver crawled) (str "http://localhost:" port "/"))
      @stopped)))

(deftest all-websites
  (run! test-crawler (list-dirs websites-path)))