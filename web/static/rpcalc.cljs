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
   :n-5   {:x 1100 :y 210 :ftext "" :mtext "5" :gtext "M.DY"}
   :n-6   {:x 1250 :y 210 :ftext "" :mtext "6" :gtext "x̄w"}
   :n-x   {:x 1400 :y 210 :ftext "" :mtext "x" :gtext "x²"}
   :n-1   {:x 950 :y 310 :ftext "" :mtext "1" :gtext "x̂,r"}
   :n-2   {:x 1100 :y 310 :ftext "" :mtext "2" :gtext "ŷ,r"}
   :n-3   {:x 1250 :y 310 :ftext "" :mtext "3" :gtext "n!"}
   :n-m   {:x 1400 :y 310 :ftext "" :mtext "-" :gtext "←"}
   :n-0   {:x 950 :y 410 :ftext "" :mtext "0" :gtext "x̄"}
   :n-d   {:x 1100 :y 410 :ftext "" :mtext "." :gtext "S"}
   :n-S   {:x 1250 :y 410 :ftext "" :mtext "Σ+" :gtext "Σ-"}
   :n-p   {:x 1400 :y 410 :ftext "" :mtext "+" :gtext "LSTx"}
   })

(def btn-keys
  {:row-0 [:f-n :f-i :f-pv :f-pmt :f-fv :f-chs :n-7 :n-8 :n-9 :n-v]
   :row-1 [:n-4 :n-5 :n-6 :n-x]
   :row-2 [:n-1 :n-2 :n-3 :n-m]
   :row-3 [:n-0 :n-d :n-S :n-p]
   })

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
   (into [:g]
    (for [row-id [:row-0 :row-1 :row-2 :row-3]
          btn-key (row-id btn-keys)]
      (btn (btn-key btn-info))))])

(defn my-component []
  [:div {:style {:color "silver" :background-color "black" :font "9pt sans-serif"}}
   [:p "This site is an exercise - for educational purposes only."]
   [:div (frame)]])

(rdom/render [my-component] (.getElementById js/document "app"))