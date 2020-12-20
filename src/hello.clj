(ns hello
  (:require
   [hickory.core :as h]
   [hickory.select :as s]
   [hickory.zip :as hz]
   [clojure.zip :as z]
   [daiquiri.normalize :as norm]
   [rum.core :as rum]))


(norm/match-tag :p)
(norm/match-tag :p.foo)
(norm/match-tag :p#the-id)
(norm/match-tag :p#the-id.the-class)
(norm/match-tag :p#the-id.the-class.other-class)


(extend-protocol h/HickoryRepresentable
  clojure.lang.PersistentVector
  (h/as-hickory [this]
    (let [[tag attrs? & body] this
          [attrs body] (if (map? attrs?)
                         [attrs? body]
                         [{} (cons attrs? body)])
          [tag id classes] (norm/match-tag tag)
          [tag attrs] [tag (norm/merge-with-class
                            {:id id :class classes}
                            attrs)]]
      {:type :element
       :tag tag
       :attrs attrs
       :content (mapv h/as-hickory (filter some? body))}))
  
  java.lang.String
  (h/as-hickory [this]
    this))

(extend-protocol h/HiccupRepresentable
  clojure.lang.PersistentArrayMap
  (h/as-hiccup [this]
    (let [{:keys [type tag attrs content]} this]
      (cond
        (string? content)
        content

        (= :element type)
        (vec (concat [(keyword tag) attrs] (mapv h/as-hiccup content))))))

  java.lang.String
  (h/as-hiccup [this]
    this))

(rum/render-static-markup
 (h/as-hiccup (h/as-hickory [:p#the-id.some.classes {:class ["extra classes"]}
                             [:<> "hi"]
                             " "
                             [:span#my-span.stuff "stuff"]])))

(h/as-hickory [:p])
(h/as-hiccup (h/as-hickory [:p [:<> "hi"] "stuff"]))
(h/as-hiccup (h/as-hickory [:p {:class "x y z"} "stuff"]))
(h/as-hiccup (h/as-hickory [:p.some.classes {} [:<> "hi"] "stuff"]))
(h/as-hiccup (h/as-hickory [:p#the-id {} [:<> "hi"] "stuff"]))
(h/as-hiccup (h/as-hickory [:p#the-id.some.classes {} [:<> "hi"] "stuff"]))
(h/as-hiccup (h/as-hickory [:p#the-id.some.classes {:class ["extra classes"]} [:<> "hi"] "stuff"]))


;; Example HTML and selector

(rum/defc custom []
  [:custom-component "CUSTOM COMPONENT!"])

(def main
  [:main
   [:h1 "Welcome to my Web Page"]
   [:div#stuff.things.content
    [:h2.subheading "Hi there"]
    [:div.this "this one"]
    [:div#here.hello
     "Wrap this content"]]
   (custom)
   [:div.this "this one too!"]
   [:footer "Some footer content"]])

(def page
  [:html {:lang :en}
   [:head
    [:title "The Page Title"]
    [:meta {:charset "utf-8"}]
    [:link {:rel :stylesheet :href "/css/main.css"}]]
   [:body
    main]])

;; Just practicing selecting some stuff

(defn here? [loc]
  (= "here" (-> loc z/node :attrs :id)))

(z/node (s/select-next-loc here? (hz/hickory-zip (h/as-hickory page))))

;; Wrap some stuff in a surrounding element

(defn wrap-content [node]
  (assoc node :content [{:type :element
                         :tag "div"
                         :attrs {:class "wrapper"}
                         :content (:content node)}]))

(defn this? [loc]
  (= ["this"] (-> loc z/node :attrs :class)))

(comment
  ;; practicing with toy selector
  (defn select-this [loc]
    (if-let [next (s/select-next-loc this? loc)]
      (z/edit next wrap-content)
      loc))

  (as-> main $
    (h/as-hickory $)
    (hz/hickory-zip $)
    (select-this $)
    (z/root $)
    (h/as-hiccup $)))

;; low-level transformers

(defn transform-first [loc f xform]
  (if-let [selected (s/select-next-loc f loc)]
    (-> selected (z/edit xform) z/root)
    (z/root loc)))

(defn transform-while [loc f xform]
  (loop [loc loc]
    (if (identical? (z/next loc) loc)
      (z/root loc)
      (if-let [selected (s/select-next-loc f loc)]
        (recur (-> selected
                 (z/edit xform)
                 (z/next)))
        (z/root loc)))))

;; updaters - main API

(defn update-first [markup selector xform]
  (-> markup
      h/as-hickory
      hz/hickory-zip
      (transform-first selector xform)
      h/as-hiccup))

(defn update-all [markup selector xform]
  (-> markup
      h/as-hickory
      hz/hickory-zip
      (transform-while selector xform)
      h/as-hiccup))

(comment
  (as-> page $
    (h/as-hickory $)
    (hz/hickory-zip $)
    (transform-while $ this? wrap-content)
    (h/as-hiccup $))

  (update-first page this? wrap-content)
  (update-all page this? wrap-content))