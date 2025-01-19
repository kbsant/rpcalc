(require '[reagent.core :as r]
         '[reagent.dom :as rdom])


(def state (r/atom {:clicks 0}))

(def colors
  {:mnblue "rgb(32,32,80)"
   :black "rgb(0,0,0)"})

(defn frame []
  [:svg {:width "100%" :viewBox "0 0 1500 1000"}
   [:rect {:width "100%" :height "100%" :style {:fill (:mnblue colors)}}]])

(defn my-component []
  [:div
   [:div (frame)]
   [:p "Clicks v2: " (:clicks @state)]
   [:p [:button {:on-click #(swap! state update :clicks inc)}
        "Click me! v2"]]])

(rdom/render [my-component] (.getElementById js/document "app"))