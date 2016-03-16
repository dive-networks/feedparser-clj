(ns feedparser-clj.core-test
  (:require [clojure.test :refer :all]
            [feedparser-clj.core :refer :all])
  (:import [java.io File]))


(defn test-file [filename]
  (File. (str "./test/feedparser_clj/resources/" filename)))

(defn first-entry [src & args]
  (-> (apply parse-feed src args) :entries first))

(deftest test-parse-feed
  (let [trends (test-file "google-trends-custom.xml")
        itunes (test-file "topmusic-itunes.xml")]
    (testing "extra field is empty"
      (is (empty? (-> (first-entry trends :extra nil) :extra))))

    (testing "extra field has data"
      (testing "extracts a single xmlns"
        (is (= {:picture "//t3.gstatic.com/images?q=tbn:ANd9GcQfgpW-VQEdBHDEZ1XqC1pQ_KsnvGA0KXC5uaNpsVbfu0XUCMhP0udQNTBzybD6ESbJVZ6wH4QH"}
               (-> (first-entry trends :extra {:ht [:picture]}) :extra)))

        (is (= {:name "Work (feat. Drake)"
                :artist "Rihanna"
                :price "$1.99"
                :releaseDate "2016-02-23T00:00:00-07:00"}
               (-> (first-entry itunes :extra {:im [:name :artist :price :releaseDate]}) :extra))))

      (testing "extracts multiple xmlns"
        (is (= {:picture "//t3.gstatic.com/images?q=tbn:ANd9GcQfgpW-VQEdBHDEZ1XqC1pQ_KsnvGA0KXC5uaNpsVbfu0XUCMhP0udQNTBzybD6ESbJVZ6wH4QH"
                :news-item-source "Chicago Tribune"}
               (-> (first-entry trends :extra {:ht [:picture]
                                               :mi [:news_item_source]}) :extra))))

      (testing "extra key is dasherized"
        (is (= :approx-traffic
               (-> (first-entry trends :extra {:ht [:approx_traffic]}) :extra keys first)))))))

