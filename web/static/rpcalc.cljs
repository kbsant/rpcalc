(require '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[clojure.string :as string])


(def state (r/atom {:clicks 0}))

(defn svg-style []
  [:style """
  .frame {fill: rgb(0,20,40)}
  .lcdisplay rect {fill: rgb(120,140,120)}
  .fg-btn {font: 20px sans-serif; fill:white;}
  .fg-btn rect {fill: black; stroke: gold;}
  .fg-btn text.ftext {stroke:orange;}
  .fg-btn text.mtext {stroke:white;}
  .fg-btn text.gtext {stroke:blue;}
  text.ntext {font: 90px monospace; fill:dimgray; stroke:black;}
  """ ])

(def btn-info
  {:f-n   {:x 50 :y 110 :ftext "AMORT" :mtext "n" :gtext "12x"}
   :f-i   {:x 200 :y 110 :ftext "INT" :mtext "i" :gtext "12÷"}
   :f-pv  {:x 350 :y 110 :ftext "NPV" :mtext "PV" :gtext "CFo"}
   :f-pmt {:x 500 :y 110 :ftext "RND" :mtext "PMT" :gtext "CFj"}
   :f-fv  {:x 650 :y 110 :ftext "IRR" :mtext "FV" :gtext "Nj"}
   :f-chs {:x 800 :y 110 :ftext "RPN" :mtext "CHS" :gtext "DATE"}
   :n-7   {:x 950 :y 110 :ftext "" :mtext "7" :gtext "BEG"}
   :n-8  {:x 1100 :y 110 :ftext "" :mtext "8" :gtext "END"}
   :n-9  {:x 1250 :y 110 :ftext "" :mtext "9" :gtext "MEM"}
   :n-v  {:x 1400 :y 110 :ftext "" :mtext "÷" :gtext "⤶"}
   :n-4   {:x 950 :y 210 :ftext "" :mtext "4" :gtext "D.MY"}
   :n-5   {:x 110 :y 210 :ftext "" :mtext "5" :gtext "M.DY"}
   :n-6   {:x 1250 :y 210 :ftext "" :mtext "6" :gtext "x̄w"}
   :n-x   {:x 1400 :y 210 :ftext "" :mtext "x" :gtext "x²"}
   })

(def btn-keys
  {:row-0 [:f-n :f-i :f-pv :f-pmt :f-fv :f-chs :n-7 :n-8 :n-9 :n-v]})
(defn lcdisplay [ntext]
  [:g.lcdisplay
   [:rect {:x 50 :y 10 :width 1300 :height 100}]
   [:text.ntext {:x 55 :y 80 } ntext]])

(defn btn [{:keys [x y height ftext mtext gtext]}]
  (let [x-fn #(- (+ x 20) (* 5 (count %)))]
    [:g.fg-btn
     [:text.ftext {:x (x-fn ftext) :y (+ 30 y) } ftext]
     [:rect {:x (- x 20) :y (+ 40 y) :width 90 :height (or height 80)}]
     [:text.mtext {:x (x-fn mtext) :y (+ 80 y) } mtext]
     [:text.gtext {:x (x-fn gtext) :y (+ 115 y) } gtext]]))

(defn frame []
  [:svg {:width "100%" :viewBox "0 0 1500 1000"}
   (svg-style)
   [:rect.frame {:width "100%" :height "100%"}]
   (lcdisplay "0.0000")
   (-> [:g]
       (into
        (for [btn-key (:row-0 btn-keys)]
          (btn (btn-key btn-info)))))])

(defn my-component []
  [:div
   [:div (frame)]
   [:p "Clicks v2: " (:clicks @state)]
   [:p [:button {:on-click #(swap! state update :clicks inc)}
        "Click me! v2"]]])

(rdom/render [my-component] (.getElementById js/document "app"))