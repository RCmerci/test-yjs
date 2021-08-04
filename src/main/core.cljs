(ns core
  (:require ["yjs" :as y]
            ["y-websocket" :as y-ws]
            [clojure.zip :as zip]))


(def ydoc1 (y/Doc.))
(def ydoc2 (y/Doc.))
;; (def wsProvider1 (y-ws/WebsocketProvider. "ws://192.168.1.149:1234", "demo", ydoc1))
;; (def wsProvider2 (y-ws/WebsocketProvider. "ws://192.168.1.149:1234", "demo", ydoc2))


(def content1
  {1 {:content "111"
      2 {:content "222" :left 1}
      3 {:content "333" :left 2
         4 {:content "444" :left 3}}}})

(def content2
  {1 {:content "111"
      5 {:content "555" :left 1}}})

(def content3
  {:content1 "222"})

(def content4
  {:content "333"})

(defn ->map [content map]
  (let [map (or map (y/Map.))]
    (mapv (fn [[k v]]
            (cond
              (number? k)
              (.set map (str k) (->map v nil))
              (= k :content)
              (.set map (str k) (y/Text. v))

              :else
              (.set map (str k) v))) content)
    map))




(defn init []
  (println "init"))

(defn fill-doc [doc content]
  (let [m (.getMap doc "content")]
    (->map content m)))

(defn set-m1 [doc]
  (.set (.getMap doc "content") "k1" "v1"))
(defn set-m2 [doc]
  (.set (.getMap doc "content") "k2" "v2"))
;; const stateVector1 = Y.encodeStateVector(ydoc1)
;; const stateVector2 = Y.encodeStateVector(ydoc2)
;; const diff1 = Y.encodeStateAsUpdate(ydoc1, stateVector2)
;; const diff2 = Y.encodeStateAsUpdate(ydoc2, stateVector1)
;; Y.applyUpdate(ydoc1, diff2)
;; Y.applyUpdate(ydoc2, diff1)
(defn merge-doc []
  (let [s1 (y/encodeStateVector ydoc1)
        s2 (y/encodeStateVector ydoc2)
        d1 (y/encodeStateAsUpdate ydoc1 s2)
        d2 (y/encodeStateAsUpdate ydoc2 s1)]
    (y/applyUpdate ydoc1 d2)
    (y/applyUpdate ydoc2 d1)))


(defn merge-doc1 []
  (let [s1 (y/encodeStateAsUpdate ydoc1)
        s2 (y/encodeStateAsUpdate ydoc2)]
    (y/applyUpdate ydoc1 s2)
    (y/applyUpdate ydoc2 s1)))
;; doc1.on('update', update => {
;;   Y.applyUpdate(doc2, update)
;; })

;; doc2.on('update', update => {
;;   Y.applyUpdate(doc1, update)
;; })

(defn update []
  (.on ydoc1 "update" (fn [update] (y/applyUpdate ydoc2 update)))
  (.on ydoc2 "update" (fn [update] (y/applyUpdate ydoc1 update))))

(defn start []
  (println "start"))

(defn stop []
  (println "stop"))


(comment
  (let [m1 (.getMap ydoc1 "m")
        m2 (.getMap ydoc2 "m")]
    (.set m1 "text" (y/Text.))
    (.set m2 "text" (y/Text.))

    (-> ydoc1
        (.getMap "m")
        (.get "text")
        (.insert 0 "text1"))
    (-> ydoc2
        (.getMap "m")
        (.get "text")
        (.insert 0 "text2"))
    )
  )
