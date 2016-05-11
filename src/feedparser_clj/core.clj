(ns feedparser-clj.core
  (require [base64-clj.core :as base64])
  (:import (com.rometools.rome.io SyndFeedInput XmlReader WireFeedInput)
           (java.net URL HttpURLConnection)
           (java.io Reader InputStream File)
           (com.rometools.rome.feed.synd SyndFeedImpl SyndFeed SyndEntry SyndImage SyndPerson SyndCategory SyndLink SyndContent SyndEnclosure)
           (javax.xml XMLConstants)))


(defn- dasherize
  "Returns string `s` underscores replaced with hyphens"
  [s]
  (clojure.string/replace s #"_" "-"))

(defn- update-map-keys [f m]
  (reduce-kv (fn [hsh k v]
    (assoc hsh (f k) v)) {} m))

(defn- merge-maps
  "Given a collection containing collections of maps, each collection's contents is merged
   into a single map."
  [colls]
  (map (partial apply merge) colls))

(defn- interleave-maps
  "Given [[{:a 1}{:a 2}][{:b 1}{:b 2}][{:c 1}{:c 2}]]
   the following would be returned:
     (({:a 1}{:b 1}{:c 1})({:a 2}{:b 2}{:c 2})({:a 3}{:b 3}{:c 3}))"
  [colls]
  (apply map list colls))

(defn- zip-merge-maps [colls]
  (merge-maps (interleave-maps colls)))

;; TODO normalize image paths e.g. //some.path.com should become http://some.path.com
(defn- children-with-namespace-prefix [children ns-prefix]
  (filter #(= (name ns-prefix) (.getNamespacePrefix %)) children))

(defn- foreign-elements [entry ns-prefix]
  (-> entry .getForeignMarkup (children-with-namespace-prefix ns-prefix)))

(defn- element-value-pairs [elements]
  (map #(vector (.getName %)
                 (.getTextNormalize %))
        elements))

(defn- entry-elements [entry element-names ns-prefix]
  (->> (foreign-elements entry ns-prefix)
       element-value-pairs
       (filter (fn [[k _]] (contains? element-names k)))
       (apply concat)))

(defn- extra-elements-map [entries element-names ns-prefix]
  (->> entries
       (map (comp #(update-map-keys (comp keyword dasherize) %)
                  #(apply hash-map %)
                  #(entry-elements % element-names ns-prefix)))))

(defn- extra-elements [synd-feed extra-map]
  (when (seq extra-map)
    (let [entries (.getEntries synd-feed)]
      (->> extra-map
           (map (fn [[ns-prefix element-names]]
                  (let [name-set (->> (map name element-names) (into #{}))
                        prefix (name ns-prefix)]
                    (extra-elements-map entries name-set prefix))))
           zip-merge-maps))))

(defn- add-extra-to-entries [feed-map extra-maps]
  (let [entries (map-indexed
                  (fn [idx entry]
                    (assoc entry :extra (nth extra-maps idx)))
                  (:entries feed-map))]
    (assoc feed-map :entries entries)))

(defrecord feed [authors categories contributors copyright description
                 encoding entries feed-type image language link entry-links
                 published-date title uri])

(defrecord entry [authors categories contents contributors description
           enclosures link published-date title updated-date uri])

(defrecord enclosure [length type uri])

(defrecord person [email name uri])

(defrecord category [name taxonomyURI])

(defrecord content [type value])

(defrecord image [description link title url])

(defrecord link [href hreflang length rel title type])

(defn make-enclosure "Create enclosure struct from SyndEnclosure"
  [^SyndEnclosure e]
  (map->enclosure {:length (.getLength e) :type (.getType e)
                   :url (.getUrl e)}))

(defn make-content "Create content struct from SyndContent"
  [^SyndContent c]
  (map->content {:type (.getType c) :value (.getValue c)}))

(defn make-link "Create link struct from SyndLink"
  [^SyndLink l]
  (map->link {:href (.getHref l) :hreflang (.getHreflang l)
              :length (.getLength l) :rel (.getRel l) :title (.getTitle l)
              :type (.getType l)}))

(defn make-category "Create category struct from SyndCategory"
  [^SyndCategory c]
  (map->category {:name (.getName c)
                  :taxonomyURI (.getTaxonomyUri c)}))

(defn make-person "Create a person struct from SyndPerson"
  [^SyndPerson sp]
  (map->person {:email (.getEmail sp)
                :name (.getName sp)
                :uri (.getUri sp)}))

(defn make-image "Create image struct from SyndImage"
  [^SyndImage i]
  (map->image {:description (.getDescription i)
               :link (.getLink i)
               :title (.getTitle i)
               :url (.getUrl i)}))

(defn make-entry "Create feed entry struct from SyndEntry"
  [^SyndEntry e]
  (map->entry {:authors (map make-person (seq (.getAuthors e)))
               :categories (map make-category (seq (.getCategories e)))
               :contents (map make-content (seq (.getContents e)))
               :contributors (map make-person (seq (.getContributors e)))
               :description (if-let [d (.getDescription e)] (make-content d))
               :enclosures (map make-enclosure (seq (.getEnclosures e)))
               :link (.getLink e)
               :published-date (.getPublishedDate e)
               :title (.getTitle e)
               :updated-date (.getUpdatedDate e)
               :uri (.getUri e)}))

(defn make-feed "Create a feed struct from a SyndFeed"
  [^SyndFeed f]
  (map->feed  {:authors (map make-person (seq (.getAuthors f)))
               :categories (map make-category (seq (.getCategories f)))
               :contributors (map make-person (seq (.getContributors f)))
               :copyright (.getCopyright f)
               :description (.getDescription f)
               :encoding (.getEncoding f)
               :entries (map make-entry (seq (.getEntries f)))
               :feed-type (.getFeedType f)
               :image (if-let [i (.getImage f)] (make-image i))
               :language (.getLanguage f)
               :link (.getLink f)
               :entry-links (map make-link (seq (.getLinks f)))
               :published-date (.getPublishedDate f)
               :title (.getTitle f)
               :uri (.getUri f)}))

(defn ^WireFeedInput gen-feed-input
  []
  (proxy [WireFeedInput] []
    (createSAXBuilder []
      (doto (proxy-super createSAXBuilder)
        (.setFeature XMLConstants/FEATURE_SECURE_PROCESSING true)
        (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)))))

(defn ^SyndFeedInput gen-syndfeedinput
  []
  (proxy [SyndFeedInput] []
      (build [^Reader rdr]
        (SyndFeedImpl. (.build (gen-feed-input) rdr) false))))

(defn- parse-internal [^XmlReader xmlreader]
  (let [feedinput (gen-syndfeedinput)]
    (.build feedinput xmlreader)))

(defn- with-user-agent [connection user-agent]
  "Adds User-Agent property to the given HttpURLConnection to avoid HTTP 429 errors"
  (if user-agent
    (.setRequestProperty connection "User-Agent" user-agent)
    connection))

(defn- with-basic-authentication [connection username password]
  "Adds Basic Authentication property to the given HttpURLConnection"
  (if username
    (let [encoding (base64/encode (str username ":" password))]
      (.setRequestProperty connection "Authorization" (str "Basic " encoding) ))
    connection))

(defn- connection-with-properties [url {:keys [user-agent username password]}]
  "Returns an HttpURLConnection for `url`.
  Options:
  * :user-agent for setting the user agent header
  * :username and :password for setting a Basic authentication header"
  (doto (cast HttpURLConnection (.openConnection (URL. url)))
    (with-user-agent user-agent)
    (with-basic-authentication username password)))

(defn- url-feed? [feedsource]
  (string? feedsource))

(defn parse-feed
  "Get and parse a feed from a URL string, XML File, InputStream or an HttpURLConnection.

   ## Options

  * :user-agent for setting the user agent header (for URL `feedsource` only)

  * :username and :password for setting a Basic authentication header (for URL `feedsource` only)

   * Provide an `:extra` option in order to extract elements from the feed that are not
   supported by the RSS/Atom xmlns. This must be a map with the keys being namespace prefixes
   (either a string or keyword) and the values being a collection of element names without
   their namespace (either strings or keywords). All matching elements for each namespace
   will be merged into a single map and then added to each entry's hash-map under the
   `:extra` key.

   The extra data keys will be dasherized (underscores replaced with hyphens) and cast to
   keywords. Also note that at this time this only supports elements that are direct
   descendants of the entry's root node.

   Example: the Google Trends feed uses a xmlns called 'ht' for some of its elements.
   We want to extract the `ht:picture` and `ht:approx_source` elements. To do this the
   `:extra` map would be set to `{:ht [:picture :approx_source]}`. One of the returned
   entries may look like this:

   `{:title 'Mother Teresa' :extra {:picture 'http://example.com' :approx-source '1,000,000+}}`."
  [feedsource & {:keys [extra] :as options}]
  (let [source (if (url-feed? feedsource)
                 (connection-with-properties feedsource options)
                 feedsource)
        synd-feed (-> (cond
                        (url-feed? source) (XmlReader. (URL. source))
                        (instance? HttpURLConnection source) (XmlReader. ^HttpURLConnection source)
                        (instance? InputStream source) (XmlReader. ^InputStream source)
                        (instance? File source) (XmlReader. ^File source)
                        :else (throw (ex-info "Unsupported source"
                                              {:source source :type (type source)})))
                      parse-internal)
        extra-maps (extra-elements synd-feed extra)]
    (-> (make-feed synd-feed)
        (add-extra-to-entries extra-maps))))
