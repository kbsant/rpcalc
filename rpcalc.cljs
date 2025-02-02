(require '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[clojure.string :as string]
         '[cljs.pprint :as pprint])

(defn log [& x] nil #_ (.log js/console (apply str x)))
(defn spy [x] (log x) x)

(def initial-state
  {:sto [0 0 0 0 0 0 0 0 0 0]
   :raw-input ""
   :lastx 0.0
   :operand-stack [0.0]
   :display-precision 8
   :flags {:date-format :dmy :sci false}
   :shift :none
   :day nil
   :undo {}})

(def state (r/atom initial-state))

(def svg-style
  [:style """
  .frame {fill: rgb(0,20,40)}
  .lcdisplay rect {fill: rgb(120,140,120)}
  .fg-btn {font-family: sans-serif; fill:white;}
  .fg-btn rect {fill: black; stroke: gold;}
  .fg-btn text.ftext {font-size: 18px;stroke:orange;}
  .fg-btn text.mtext {font-size: 22px;stroke:white;}
  .fg-btn text.gtext {font-size: 20px;stroke:dodgerblue;}
  .sh-btn {font-family: sans-serif; fill:navy;}
  .sh-btn text {font-size: 32px;fill: black; stroke: navy;}
  .sh-btn rect.f-btn {fill: orange; stroke: white;}
  .sh-btn rect.g-btn {fill: dodgerblue; stroke: white;}
  text.ntext {font: 90px monospace; fill:dimgray; stroke:black;}
  text.mtext {font: 60px monospace; fill:dimgray; stroke:black;}
  text.stext {font: 30px monospace; fill:black; stroke:black;}
  text.sstext {font: 18px monospace; fill:black; stroke:black;}
  """ ])

(defn swap-state-fn [& args]
  #(apply swap! state args))

(defn number [n]
  (js/Number. n))

(defn peekz [stack]
  (or (peek stack) 0.0))

(defn popz [stack]
  (cond-> stack (seq stack) (pop)))

(defn conjn [stack n]
  (cond-> stack (js/isFinite n) (conj n)))

(defn numberz [s]
  (if (string/blank? s) 0.0 (number s)))

(defn backspace [s]
  (let [n (count s)]
    (if (> n 0) (subs s 0 (dec n)) "")))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn frac-part [d]
  (- d  (Math/trunc d)))

(def intg-part Math/trunc)

(defn percentage [base pct]
  (* base (/ pct 100.0)))

(defn pct-diff [a b]
  (-> (- b a) (/ a) (* 100.0)))

(defn pct-total [base part]
  (* (/ part base) 100.0))

(defn round-prec [f digits]
  (-> f (.toFixed digits) (numberz)))

(defn days-ms [n]
  (* 24 60 60 1000 n))

(defn day-name [n]
  (get ["SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"] n))

(defn day-num7 [n]
  (str (get [7 1 2 3 4 5 6] n)))

(defn ymd-str [y m d]
  (pprint/cl-format nil "~4,'0D-~2,'0D-~2,'0D" y m d))

(defn ymd-date-ts [y m d]
  (->> (ymd-str y m d)
       (spy)
       (js/Date.)
       (.valueOf)
       (spy)))

(defn ts-date [ts]
  (let [date (js/Date. ts)
        y (.getUTCFullYear date)
        m-1 (.getUTCMonth date)
        d (.getUTCDate date)
        dow (.getUTCDay date)]
    [y (inc m-1) d dow]))

(defn ts-date-dmy [ts]
  (let [[y m d dow] (ts-date ts)]
    [(+ d (/ m 100) (/ y 1000000)) dow]))

(defn ts-date-mdy [ts]
  (let [[y m d dow] (ts-date ts)]
    [(+ m (/ d 100) (/ y 1000000)) dow]))

(defn parse-date [to-date-fn ndate]
  (log "parse-date ndate:" ndate)
  (let [dd (intg-part ndate)
        mmyyyy (* 100 (- ndate dd))
        mm (intg-part mmyyyy)
        _ (log "mmyyyy:" mmyyyy " mm:" mm )
        yyyy (-> (* 10000 (- mmyyyy mm)) (round-prec 0) (int))]
    (log "yyyy:" yyyy)
    (to-date-fn yyyy mm dd)))

(defn dmy-date [ndate]
  (parse-date ymd-date-ts ndate))

(defn mdy-date [ndate]
  (parse-date #(ymd-date-ts %1 %3 %2) ndate))

(defn add-days [dmy? ndate days]
  (let [date0-ts ((if dmy? dmy-date mdy-date) ndate)
        date1-ts (+ date0-ts (days-ms days))]
    ((if dmy? ts-date-dmy ts-date-mdy) date1-ts)))

(defn backspace-handler []
  (swap! state
         #(as-> % s
            (update s :raw-input backspace)
            (if (= "" (:raw-input s)) (assoc s :lastx nil) s))))

(defn clx-handler []
  (swap! state assoc :raw-input "" :lastx nil :day nil))

(defn clx-reg-handler []
  (swap! state merge
         (select-keys initial-state
                      [:raw-input :operand-stack :sto :lastx :day])))

(defn update-lastx [{stack :operand-stack :as state-info}]
  (assoc state-info :lastx (peekz stack)))

(defn enter-helper [{r :raw-input :as state-info}]
  (-> state-info
      (assoc :raw-input "" :day nil)
      (update :operand-stack conj (numberz r))
      (update-lastx)))

(defn enter-handler []
  (swap! state enter-helper))

(defn decimal-pt-handler []
  (let [dp-if-none #(cond
                      (string/blank? %) "0."
                      (string/includes? % ".") %
                      :else (str % "."))]
    (swap! state update :raw-input dp-if-none)))

(def max-digits-in 20)

(defn num-handler-fn [n]
  #(swap! state update :raw-input
          (fn [r]
            (if (< (count r) max-digits-in)
              (str r n)
              r))))

(defn push-input-value [stack raw-input]
  (cond-> stack
    (not (string/blank? raw-input)) (conjn (numberz raw-input))))

(defn update-state-result
  [{:keys [raw-input] :as state-info} state-update-fn]
  (-> state-info
      (spy)
      (update :operand-stack push-input-value raw-input)
      (update-lastx)
      (state-update-fn)
      (assoc :raw-input "")
      (spy)))

(defn update-stack-result
  [state-info stack-update-fn op]
  (update-state-result
   state-info
   #(update % :operand-stack (partial stack-update-fn state-info op))))

(defn op-fn [upd-fn op]
  (swap-state-fn #(update-stack-result % upd-fn op)))

(defn unary-op [_ op stack]
  (let [result (op (peekz stack))]
    (-> stack (popz) (conjn result))))

(defn binary-op [_ op stack]
  (let [rhs (peekz stack)]
    (unary-op _ #(op % rhs) (popz stack))))

(defn acc-op [_ op stack]
  (let [rhs (peekz stack)
        lhs (peekz (popz stack))
        result (op lhs rhs)]
    (-> stack (popz) (conjn result))))

(defn add-days-op [{:keys [flags operand-stack] :as state-info}]
  (spy state-info)
  (let [dmy? (= :dmy (:date-format flags))
        days (peekz operand-stack)
        ndate-0 (peekz (pop operand-stack))
        _ (log "add helper days:" days " ndate-0:" ndate-0)
        [ndate-1 dow] (add-days dmy? ndate-0 days)]
    (log "day of week:" dow " ndate-1:" ndate-1)
    (-> state-info
        (update :operand-stack #(-> % (popz) (popz) (conjn ndate-1)))
        (assoc :day dow)
        (spy))))

(defn add-days-fn []
  (swap-state-fn #(update-state-result % add-days-op)))

(defn swap-nop [_ _ stack]
  (let [rhs (peekz stack)
        lhs (peekz (popz stack))]
    (-> stack (popz) (popz) (conjn rhs) (conjn lhs))))

(defn round-nop [{digits :display-precision} _ stack]
  (let [result (-> (peekz stack) (numberz) (round-prec digits) )]
    (-> stack (popz) (conjn result))))

(defn set-flag-fn [f v]
  (fn [] (swap! state #(assoc-in % [:flags f] v))))

(defn toggle-flag-fn [f]
  (fn [] (swap! state #(update-in % [:flags f] not))))

(defn toggle-shift-fn [s]
  (fn [] (swap! state #(update % :shift (fn [e] (if (= s e) :none s))))))

(defn set-precision-fn [n]
  (fn [] (swap!
          state
          #(-> %
               (assoc :display-precision n :shift :none)
               (assoc-in [:flags :sci] false)))))

(defn sto-fn [n]
  #(swap!
    state
    (fn
      [{:keys [raw-input sto-op] :as state-info}]
      (as-> state-info s
        (spy s)
        (update s :operand-stack push-input-value raw-input)
        (if sto-op
          (update-in s [:sto n] sto-op (peekz (:operand-stack s)))
          (assoc-in s [:sto n] (peekz (:operand-stack s))))
        (assoc s :raw-input "" :sto-op nil)
        (spy s)))))

(defn rcl-fn [n]
  #(swap!
    state
    (fn
      [{:keys [sto] :as state-info}]
      (let [d (get sto n)]
        (-> state-info
            (spy)
            (update :operand-stack conjn d)
            (assoc :raw-input "" :lastx d)
            (spy))))))

(defn lastx-handler []
  (swap!
   state
   (fn
     [{:keys [lastx] :as state-info}]
     (-> state-info
         (spy)
         (update :operand-stack conjn lastx)
         (assoc :raw-input "")
         (spy)))))

(defn dispatch-btn [{:keys [nfn ffn gfn sto rcl shiftfn stofn mtext]}]
  (let [shift (:shift @state)]
    (cond
      (and (= :f shift) ffn) (ffn)
      (and (= :g shift) gfn) (gfn)
      (and (= :sto shift) sto) (sto)
      (and (= :rcl shift) rcl) (rcl)
      nfn (nfn)
      (not shiftfn) (log "Unmapped button:" mtext)
      :else :none)
    (if (or shiftfn (and (= :sto shift) stofn))
      ((or shiftfn stofn))
      (swap! state assoc :shift :none))))

(defn btn
  [{:keys [x y height ftext mtext gtext] :or {height 80} :as btn-info}]
  (let [x-fn #(- (+ x 20) (* 5 (count %)))
        click-fn #(dispatch-btn btn-info)]
    [:g.fg-btn
     [:text.ftext {:x (x-fn ftext) :y (+ 30 y) } ftext]
     [:rect
      {:on-click click-fn :x (- x 20) :y (+ 40 y) :width 90 :height height}]
     [:text.mtext {:x (x-fn mtext) :y (+ 80 y) } mtext]
     [:text.gtext {:x (x-fn gtext) :y (+ height 35 y) } gtext]]))

(defn shift-btn [{:keys [x y mtext shiftfn rect-class]}]
  (let [x-fn #(- (+ x 20) (* 5 (count %)))]
    [:g.sh-btn
     [rect-class {:on-click shiftfn :x (- x 20) :y (+ 40 y) :width 90 :height 80}]
     [:text {:x (x-fn mtext) :y (+ 90 y) } mtext]]))

(def btn-info
  {:f-n   {:ftext "AMORT" :mtext "n" :gtext "12x"}
   :f-i   {:ftext "INT" :mtext "i" :gtext "12÷"}
   :f-pv  {:ftext "NPV" :mtext "PV" :gtext "CFo"}
   :f-pmt {:ftext "RND" :mtext "PMT" :gtext "CFj"
           :ffn (op-fn round-nop 1)}
   :f-fv  {:ftext "IRR" :mtext "FV" :gtext "Nj"}
   :f-chs {:ftext "RPN" :mtext "CHS" :gtext "DATE"
           :nfn (op-fn unary-op #(* -1 %))
           :gfn (add-days-fn)}
   :n-7   {:ftext "" :mtext "7" :gtext "BEG"
           :nfn (num-handler-fn 7) :ffn (set-precision-fn 7)
           :sto (sto-fn 7) :rcl (rcl-fn 7)}
   :n-8   {:ftext "" :mtext "8" :gtext "END"
           :nfn (num-handler-fn 8) :ffn (set-precision-fn 8)
           :sto (sto-fn 8) :rcl (rcl-fn 8)}
   :n-9   {:ftext "" :mtext "9" :gtext "MEM"
           :nfn (num-handler-fn 9) :ffn (set-precision-fn 9)
           :sto (sto-fn 9) :rcl (rcl-fn 9)}
   :n-div {:ftext "" :mtext "÷" :gtext "⤶"
           :nfn (op-fn binary-op /)
           :sto #(swap! state assoc :sto-op /)
           :stofn (constantly nil)}
   :f-exp {:ftext "PRICE" :mtext "yˣ" :gtext "√x"
           :nfn (op-fn binary-op Math/pow) :gfn (op-fn unary-op Math/sqrt)}
   :f-inv {:ftext "YTM" :mtext "1/x" :gtext "eˣ"
           :nfn (op-fn unary-op #(/ 1 %)) :gfn (op-fn unary-op Math/exp)}
   :f-pctt {:ftext "SL" :mtext "%T" :gtext "LN"
            :nfn (op-fn binary-op pct-total) :gfn (op-fn unary-op Math/log)}
   :f-pctd {:ftext "SOYD" :mtext "Δ%" :gtext "FRAC"
            :nfn (op-fn binary-op pct-diff) :gfn (op-fn unary-op frac-part)}
   :f-pct {:ftext "DB"  :mtext "%" :gtext "INTG"
           :nfn (op-fn acc-op percentage) :gfn (op-fn unary-op intg-part)}
   :f-eex {:ftext "ALG" :mtext "EEX" :gtext "ΔDYS"
           :nfn (op-fn binary-op #(* %1 (Math/pow 10 %2)))}
   :n-4   {:ftext "" :mtext "4" :gtext "D.MY"
           :nfn (num-handler-fn 4) :ffn (set-precision-fn 4)
           :gfn (set-flag-fn :date-format :dmy)
           :sto (sto-fn 4) :rcl (rcl-fn 4)}
   :n-5   {:ftext "" :mtext "5" :gtext "M.DY"
           :nfn (num-handler-fn 5) :ffn (set-precision-fn 5)
           :gfn (set-flag-fn :date-format :mdy) :sto (sto-fn 5)
           :rcl (rcl-fn 5)}
   :n-6   {:ftext "" :mtext "6" :gtext "x̄w"
           :nfn (num-handler-fn 6) :ffn (set-precision-fn 6)
           :sto (sto-fn 6) :rcl (rcl-fn 6)}
   :n-mul {:ftext "" :mtext "x" :gtext "x²"
           :nfn (op-fn binary-op *) :gfn (op-fn unary-op #(* % %))
           :sto #(swap! state assoc :sto-op *)
           :stofn (constantly nil)}
   :f-rs  {:ftext "P/R" :mtext "R/S" :gtext "PSE"}
   :f-sst {:ftext "Σ" :mtext "SST" :gtext "BST"}
   :f-run {:ftext "PRGM" :mtext "R↓" :gtext "GTO"}
   :f-x-y {:ftext "FIN" :mtext "x≷y" :gtext "x≤y"
           :nfn (op-fn swap-nop 1)}
   :f-clx {:ftext "REG"  :mtext "CLx" :gtext "x=0"
           :nfn clx-handler :ffn clx-reg-handler}
   :f-entr {:ftext "PREFIX" :mtext "EN" :gtext "="
            :height 220 :nfn enter-handler}
   :n-1   {:ftext "" :mtext "1" :gtext "x̂,r"
           :nfn (num-handler-fn 1) :ffn (set-precision-fn 1)
           :sto (sto-fn 1) :rcl (rcl-fn 1)}
   :n-2   {:ftext "" :mtext "2" :gtext "ŷ,r"
           :nfn (num-handler-fn 2) :ffn (set-precision-fn 2)
           :sto (sto-fn 2) :rcl (rcl-fn 2)}
   :n-3   {:ftext "" :mtext "3" :gtext "n!"
           :nfn (num-handler-fn 3) :ffn (set-precision-fn 3)
           :gfn (op-fn unary-op factorial)
           :sto (sto-fn 3) :rcl (rcl-fn 3)}
   :n-sub {:ftext "" :mtext "-" :gtext "←"
           :nfn (op-fn binary-op -) :gfn backspace-handler
           :sto #(swap! state assoc :sto-op -)
           :stofn (constantly nil)}
   :f-on  {:ftext "OFF" :mtext "ON" :gtext ""}
   :f-f   {:draw-fn shift-btn :mtext "f"
           :rect-class :rect.f-btn :shiftfn (toggle-shift-fn :f)}
   :f-g   {:draw-fn shift-btn :mtext "g"
           :rect-class :rect.g-btn :shiftfn (toggle-shift-fn :g)}
   :f-sto {:ftext "" :mtext "STO" :gtext "("
           :shiftfn (toggle-shift-fn :sto)}
   :f-rcl {:ftext "" :mtext "RCL" :gtext ")"
           :shiftfn (toggle-shift-fn :rcl)}
   :f-nop {:draw-fn #(vector :g)}
   :n-0   {:ftext "" :mtext "0" :gtext "x̄"
           :nfn (num-handler-fn 0) :ffn (set-precision-fn 0)
           :sto (sto-fn 0) :rcl (rcl-fn 0)}
   :n-d   {:ftext "" :mtext "." :gtext "S"
           :nfn decimal-pt-handler :ffn (toggle-flag-fn :sci)}
   :n-S   {:ftext "" :mtext "Σ+" :gtext "Σ-"}
   :n-add {:ftext "" :mtext "+" :gtext "LSTx"
           :nfn (op-fn binary-op +) :gfn lastx-handler
           :sto #(swap! state assoc :sto-op +)
           :stofn (constantly nil)}})

(def btn-keys
  {:row-0 [:f-n :f-i :f-pv :f-pmt :f-fv :f-chs :n-7 :n-8 :n-9 :n-div]
   :row-1 [:f-exp :f-inv :f-pctt :f-pctd :f-pct :f-eex :n-4 :n-5 :n-6 :n-mul]
   :row-2 [:f-rs :f-sst :f-run :f-x-y :f-clx :f-entr :n-1 :n-2 :n-3 :n-sub]
   :row-3 [:f-on :f-f :f-g :f-sto :f-rcl :f-nop :n-0 :n-d :n-S :n-add]})

(defn format-prec-float [flags precision n]
  (if (:sci flags)
    (.toExponential (numberz n))
    (pprint/cl-format nil (str "~," precision "f") n)))

(defn get-display-num
  [{:keys [operand-stack flags display-precision raw-input lastx]}]
  (if (string/blank? raw-input)
    (format-prec-float flags display-precision
                       (if lastx (peekz operand-stack) 0.0))
    raw-input))

(defn get-day [{:keys [raw-input day]}]
  (when (and day (= "" raw-input)) day))

(defn lcdisplay [{:keys [flags shift] :as state-info}]
  [:g.lcdisplay
   [:rect {:x 50 :y 10 :width 1300 :height 100}]
   [:text.ntext {:x 55 :y 80} (get-display-num state-info)]
   [:text.stext {:x 1290 :y 40} (when (#{:f :g} shift) (name shift))]
   (when-let [day (get-day state-info)]
     [:g
      [:text.mtext {:x 1230 :y 65} (day-num7 day)]
      [:text.sstext {:x 1270 :y 65} (day-name day)]])
   [:text.sstext {:x 1270 :y 40} (when (#{:sto :rcl} shift) (name shift))]
   [:text.sstext {:x 1270 :y 85} (when (= :dmy (:date-format flags)) "D.MY")]
   [:text.sstext {:x 1270 :y 105}  "RPN"]])

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
   (lcdisplay state-info)
   printed-buttons])

(defn my-component []
  (let [state-info @state]
    [:div {:style {:color "silver" :background-color "black" :font "9pt sans-serif"}}
     [:p "This site is an exercise - for educational purposes only." ]
     [:div (frame state-info)]]))

(rdom/render [my-component] (.getElementById js/document "app"))
