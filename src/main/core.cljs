(ns core
  (:require ["yjs" :as y]
            ["y-websocket" :as y-ws]
            [rum.core :as rum]
            [clojure.zip :as zip]))


(def ydoc1 (y/Doc.))                    ;remote
(def ydoc2 (y/Doc.))                    ;local
(def wsProvider1 (y-ws/WebsocketProvider. "ws://192.168.1.149:1234", "demo", ydoc1))
;; (def wsProvider2 (y-ws/WebsocketProvider. "ws://192.168.1.149:1234", "demo", ydoc2))


(def content5
  [{:id "1" :content "111"}
   [{:id "2" :content "222"} {:id "3" :content "333"} [{:id "4" :content "444"}]]
   {:id "5" :content "555"}])

(def content6
  [{:id "1" :content "111"}
   [{:id "2" :content "444"} {:id "6" :content "666"}]
   {:id "5" :content "55"}])


(defn ->content-map [content map]
  (clojure.walk/postwalk (fn [v] (when (map? v)
                                   (.set map (:id v) (y/Text. (:content v))))
                           v) content))


(defn ->struct-array [content arr]
  (let [arr (or arr (y/Array.))]
    (mapv (fn [text-or-sub-arr]
            (cond
              (vector? text-or-sub-arr)
              (do
                (let [child (->struct-array text-or-sub-arr nil content-map)]
                  (when (> (.-length child) 0)
                    (.push arr (clj->js [child])))))

              :else
              (do
                (.push arr (clj->js [(:id text-or-sub-arr)]))))) content)
    arr))


(defn init []
  (println "init"))

(defn fill-doc1 [doc content]
  (let [structarr (.getArray doc "struct")
        contentmap (.getMap doc "content")]
    (->content-map content contentmap)
    (->struct-array content structarr)))


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
(def need-update (atom true))

(defn update []
  (.on ydoc1 "update" (fn [update]
                        (y/applyUpdate ydoc2 update)))
  (.on ydoc2 "update" (fn [update]
                        (y/applyUpdate ydoc1 update)
                        (distinct-struct (.get ydoc2 "struct") (atom #{}))
                        (swap! need-update #(not %)))))

(defn doc->unorder-list [struct contentmap]
  [:ul
   (for [child struct]
     (if (instance? y/Array child)
       (doc->unorder-list (js->clj (.toArray child)) contentmap)
       [:li (str (.toString (.get contentmap child)) " #id: " child)]))])


(rum/defc contents < rum/reactive
  []
  (let [need-update (rum/react need-update)
        struct (js->clj (.toArray (.getArray ydoc2 "struct")))
        contentmap (.getMap ydoc2 "content")]
    (doc->unorder-list struct contentmap)))

(defn start []
  (update)
  (merge-doc)
  (fill-doc1 ydoc2 content5)
  (rum/mount (contents) js/document.body)

  (println "start"))

(defn stop []
  (println "stop"))

(defn find-pos [struct id]
  (let [toplevel (js->clj (.toArray struct))
        index (.indexOf toplevel id)]
    (if (not= -1 index)
      [index]
      (loop [i 0]
        (if (>= i (count toplevel))
          nil
          (let [sublevel (get toplevel i)]
            (if (instance? y/Array sublevel)
              (if-let [index (find-pos sublevel id)]
                (flatten [i index])
                (recur (+ i 1)))
              (recur (+ i 1)))))))))

(defn append-block [id newid newcontent] ;;sibling?=true
  (let [struct (.getArray ydoc2 "struct")
        content (.getMap ydoc2 "content")]
    (.set content newid (y/Text. newcontent))
    (when-let [pos (find-pos struct id)]
      ;; (if sibling?)
      (let [pos* (conj (vec (butlast pos)) (inc (last pos)))
            struct* (loop [i 0 s struct]
                      (if (> i (- (count pos*) 2))
                        s
                        (do
                          (println i)
                          (recur (inc i) (.get s (get pos* i))))))]
        (def xxx [pos pos* struct*])
        (.insert struct* (last pos*) (clj->js [newid]))))))

(defn modify-block [id index insert-content]
  (let [content (.getMap ydoc2 "content")]
    (.insert (.get content id) index insert-content)))


(defn distinct-struct [struct set]
  (loop [i 0]
    (when (< i (.-length struct))
      (let [s (.get struct i)]
        (if (instance? y/Array s)
          (do
            (distinct-struct s set)
            (recur (inc i)))
          (do
            (if (contains? @set s)
              (do
                (.delete struct i 1)
                (if (and
                     (>= (dec i) 0)
                     (< (inc i) (.-length struct))
                     (instance? y/Array (.get struct (dec i)))
                     (instance? y/Array (.get struct (inc i))))
                  (do
                    (distinct-struct (.get struct (inc i)) set)
                    (.push (.get struct (dec i)) (.toArray (.get struct (inc i))))
                    (.delete struct (inc i))
                    (recur i))
                  (recur (inc i))))
              (do
                (swap! set (fn [o] (conj o s)))
                (recur (inc i))))))))))

(comment
  (fill-doc1 ydoc1 content5)
  (merge-doc)
  (fill-doc2 ydoc2 content6)
  )
