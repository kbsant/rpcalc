(require '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[clojure.string :as string]
         '[cljs.pprint :as pprint])


(def state
  (r/atom
   {:raw-input ""
    :display-number 0.0
    :display-precision 4}))

(def svg-style
  [:style """
  .frame {fill: rgb(0,20,40)}
  .lcdisplay rect {fill: rgb(120,140,120)}
  .fg-btn {font-family: sans-serif; fill:white;}
  .fg-btn rect {fill: black; stroke: gold;}
  .fg-btn text.ftext {font-size: 18px;stroke:orange;}
  .fg-btn text.mtext {font-size: 22px;stroke:white;}
  .fg-btn text.gtext {font-size: 20px;stroke:blue;}
  .sh-btn {font-family: sans-serif; fill:navy;}
  .sh-btn text {font-size: 32px;fill: black; stroke: navy;}
  .sh-btn rect.f-btn {fill: orange; stroke: white;}
  .sh-btn rect.g-btn {fill: dodgerblue; stroke: white;}
  text.ntext {font: 90px monospace; fill:dimgray; stroke:black;}
  """ ])

(defn num-handler [n]
  (fn [] (swap! state update :raw-input #(str % n))))

(defn btn [{:keys [x y height ftext mtext gtext nfn] :or {height 80}}]
  (let [x-fn #(- (+ x 20) (* 5 (count %)))]
    [:g.fg-btn
     [:text.ftext {:x (x-fn ftext) :y (+ 30 y) } ftext]
     [:rect {:on-click nfn :x (- x 20) :y (+ 40 y) :width 90 :height height}]
     [:text.mtext {:x (x-fn mtext) :y (+ 80 y) } mtext]
     [:text.gtext {:x (x-fn gtext) :y (+ height 35 y) } gtext]]))

(defn shift-btn [{:keys [x y mtext nfn rect-class]}]
  (let [x-fn #(- (+ x 20) (* 5 (count %)))]
    [:g.sh-btn
     [rect-class {:on-click nfn :x (- x 20) :y (+ 40 y) :width 90 :height 80}]
     [:text {:x (x-fn mtext) :y (+ 90 y) } mtext]]))

(def btn-info
  {
   :f-n   {:ftext "AMORT" :mtext "n" :gtext "12x"}
   :f-i   {:ftext "INT" :mtext "i" :gtext "12÷"}
   :f-pv  {:ftext "NPV" :mtext "PV" :gtext "CFo"}
   :f-pmt {:ftext "RND" :mtext "PMT" :gtext "CFj"}
   :f-fv  {:ftext "IRR" :mtext "FV" :gtext "Nj"}
   :f-chs {:ftext "RPN" :mtext "CHS" :gtext "DATE"}
   :n-7   {:ftext "" :mtext "7" :gtext "BEG" :nfn (num-handler 7)}
   :n-8   {:ftext "" :mtext "8" :gtext "END" :nfn (num-handler 8)}
   :n-9   {:ftext "" :mtext "9" :gtext "MEM" :nfn (num-handler 9)}
   :n-v   {:ftext "" :mtext "÷" :gtext "⤶"}
   :f-exp {:ftext "PRICE" :mtext "yˣ" :gtext "√x"}
   :f-inv {:ftext "YTM" :mtext "1/x" :gtext "eˣ"}
   :f-pctt {:ftext "SL" :mtext "%T" :gtext "LN"}
   :f-pctd {:ftext "SOYD" :mtext "Δ%" :gtext "FRAC"}
   :f-pct {:ftext "DB"  :mtext "%" :gtext "INTG"}
   :f-eex {:ftext "ALG" :mtext "EEX" :gtext "ΔDYS"}
   :n-4   {:ftext "" :mtext "4" :gtext "D.MY" :nfn (num-handler 4)}
   :n-5   {:ftext "" :mtext "5" :gtext "M.DY" :nfn (num-handler 5)}
   :n-6   {:ftext "" :mtext "6" :gtext "x̄w" :nfn (num-handler 6)}
   :n-x   {:ftext "" :mtext "x" :gtext "x²"}
   :f-rs  {:ftext "P/R" :mtext "R/S" :gtext "PSE"}
   :f-sst {:ftext "Σ" :mtext "SST" :gtext "BST"}
   :f-run {:ftext "PRGM" :mtext "R↓" :gtext "GTO"}
   :f-x-y {:ftext "FIN" :mtext "x≷y" :gtext "x≤y"}
   :f-clx {:ftext "REG"  :mtext "CLx" :gtext "x=0"}
   :f-entr {:ftext "PREFIX" :mtext "E\nN" :gtext "=" :height 220}
   :n-1   {:ftext "" :mtext "1" :gtext "x̂,r" :nfn (num-handler 1)}
   :n-2   {:ftext "" :mtext "2" :gtext "ŷ,r" :nfn (num-handler 2)}
   :n-3   {:ftext "" :mtext "3" :gtext "n!" :nfn (num-handler 3)}
   :n-m   {:ftext "" :mtext "-" :gtext "←"}
   :f-on  {:ftext "OFF" :mtext "ON" :gtext ""}
   :f-f   {:draw-fn shift-btn :mtext "f" :rect-class :rect.f-btn}
   :f-g   {:draw-fn shift-btn :mtext "g" :rect-class :rect.g-btn}
   :f-sto {:ftext "" :mtext "STO" :gtext "("}
   :f-rcl {:ftext ""  :mtext "RCL" :gtext ")"}
   :f-nop {:draw-fn #(vector :g)}
   :n-0   {:ftext "" :mtext "0" :gtext "x̄" :nfn (num-handler 0)}
   :n-d   {:ftext "" :mtext "." :gtext "S" :nfn (num-handler ".")}
   :n-S   {:ftext "" :mtext "Σ+" :gtext "Σ-"}
   :n-p   {:ftext "" :mtext "+" :gtext "LSTx"}
   })

(def btn-keys
  {:row-0 [:f-n :f-i :f-pv :f-pmt :f-fv :f-chs :n-7 :n-8 :n-9 :n-v]
   :row-1 [:f-exp :f-inv :f-pctt :f-pctd :f-pct :f-eex :n-4 :n-5 :n-6 :n-x]
   :row-2 [:f-rs :f-sst :f-run :f-x-y :f-clx :f-entr :n-1 :n-2 :n-3 :n-m]
   :row-3 [:f-on :f-f :f-g :f-sto :f-rcl :f-nop :n-0 :n-d :n-S :n-p]})

(defn format-prec-float [precision n]
  (pprint/cl-format nil (str "~," precision "f") n))

(defn get-display-num[{:keys [display-number display-precision raw-input]}] 
  (if (string/blank? raw-input)
    (format-prec-float display-precision display-number)
    raw-input))

(defn lcdisplay [ntext]
  [:g.lcdisplay
   [:rect {:x 50 :y 10 :width 1300 :height 100}]
   [:text.ntext {:x 55 :y 80 } ntext]])

(defn render-buttons []
  (into
   [:g]
   (for [[r row-id] (map-indexed vector [:row-0 :row-1 :row-2 :row-3])
         [c btn-key] (map-indexed vector (row-id btn-keys))]
     (let [btn-i (merge {:x (+ 50 (* 150 c))
                         :y (+ 110 (* 135 r))
                         :draw-fn btn}
                        (btn-key btn-info))
           draw-fn (:draw-fn btn-i)]
       (draw-fn btn-i)))))

(def printed-buttons (render-buttons))

(defn frame [state-info]
  [:svg {:width "100%" :viewBox "0 0 1500 1000"}
   svg-style
   [:rect.frame {:width "100%" :height "100%"}]
   (lcdisplay (get-display-num state-info))
   printed-buttons])

(defn my-component []
  (let [state-info @state]
    [:div {:style {:color "silver" :background-color "black" :font "9pt sans-serif"}}
     [:p "This site is an exercise - for educational purposes only."]
     [:div (frame state-info)]]))

(rdom/render [my-component] (.getElementById js/document "app"))
