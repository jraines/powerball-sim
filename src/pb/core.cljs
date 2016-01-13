(ns pb.core
  (:require [reagent.core :as r :refer [atom]]))

(enable-console-print!)

(defn draw-standard []
  "draw 5 balls, removing a ball each time"
  (loop [balls (range 1 70)
         draw #{}]
    (if (= 5 (count draw))
      (sort (vec draw))
      (let [ball (rand-nth balls)
            drawn (conj draw ball)
            unavailable #((complement contains?) drawn %)]
        (recur (filter unavailable balls)
               drawn)))))

(defn draw-powerball []
  "draw the power ball"
  (+ 1 (rand-int 16)))

(defn draw []
  "draw all the standard balls plus the powerball; stash a set for checking winning numbers"
  (let [standard (draw-standard)
        standard-set (set standard)]
    {:standard (sort standard)
     :standard-set standard-set
     :powerball (draw-powerball)}))

(def drawn (draw))

(defn pick []
  "Make the pick for a single ticket play"
  (let [standard (draw-standard)
        powerball (draw-powerball)]
    (conj (vec (sort standard)) powerball)))

(defn hit? [num]
  "Check if one of the standard balls matches your one of your picks"
  ((:standard-set drawn) num))

(defn powerball-hit? [num]
  (= num (:powerball drawn)))

(defn hits [picked]
  {:hits (filter hit? (take 5 picked))
   :powerball? (powerball-hit? (last picked))})

(defn payout-with-powerball [hits]
  (condp = (count (:hits hits))
    5 1500000000
    4 50000
    3 100
    2 7
    1 4
    0 4))

(defn payout-without-powerball [hits]
  (condp = (count (:hits hits))
    5 1000000
    4 100
    3 7
    2 0
    1 0
    0 0))

(defn payout [hits]
  (if (:powerball? hits)
    (payout-with-powerball hits)
    (payout-without-powerball hits)))

(def state (r/atom
            {:draw (conj (vec (:standard drawn)) (:powerball drawn))
             :winners '()
             :plays 0
             :simulation nil
             :cost 0
             :total-payout 0}))

(defn play []
  "Compare one ticket to the winning number and update the state of the simulation"
  (let [pick (pick)
        payout (payout (hits pick))
        win? (not= 0 payout)
        s @state
        winners (if win?
                  (cons pick (:winners s))
                  (:winners s))]

    (reset! state {:winners      winners
                   :plays        (inc (:plays s))
                   :draw         (:draw s)
                   :simulation   (:simulation s)
                   :cost         (+ 2 (:cost s))
                   :total-payout (+ payout (:total-payout s))})))

(defn simulate []
  "Run the simulation repeatedly, store the interval in app state"
  (swap! state assoc :simulation (js/setInterval play 25)))

(defn stop-simulation []
  "Clear the interval and remove it from the app state; stop simulating"
  (let [interval (:simulation @state)]
    (js/clearInterval interval)
    (swap! state assoc :simulation nil)))


;-----------------
;; UI Components
;-----------------

(defn start-btn []
  (let [action (if (:simulation @state)
                 stop-simulation
                 simulate)
        text (if (:simulation @state)
               "Stop"
               "Start")]
  [:div
   {:id "start-btn"
    :on-click action}
   text]))


(defn formatted-winner [w]
  "Color each number green if it is a match with a winning number"
  (let [standard-results (for [num (take 5 w)]
                           {:num num
                            :match? (contains? (:standard-set drawn)
                                               num)})
        powerball-result {:num (last w)
                          :match? (= (last w) (:powerball drawn))}
        results (conj (vec standard-results) powerball-result)]
    [:div
     (for [result results]
       ^{:key result}
       [:span
        {:style (if (:match? result)
                  {:color "green"})}
        (:num result) " "])]))


(defn winners-list [winners]
  [:div
   (for [winner winners]
     ^{:key winner}
     [formatted-winner winner])])

(defn winning-number []
  (let [{:keys [draw]} @state]
    [:div "Winning Number: " (clojure.string/join " " draw)]))

(defn results []
  (let [{:keys [plays winners cost total-payout]} @state]
    [:table.results
     [:tr
      [:td.label "Plays: "]
      [:td.value plays]]
     [:tr
      [:td.label "Cost: "]
      [:td.value (.toLocaleString cost js/undefined #js {:style "currency" :currency "USD"})]]
     [:tr
      [:td.label "Wins: "]
      [:td.value (count winners)]]
     [:tr
      [:td.label "Payout: "]
      [:td.value (.toLocaleString total-payout js/undefined #js {:style "currency" :currency "USD"})]]
     [:tr
      [:td.label "Profit: "]
      [:td.value
       {:style {:color (if (< total-payout cost)
                         "red"
                         "green")}}
       (.toLocaleString (- total-payout cost) js/undefined #js {:style "currency" :currency "USD"})]]]))

(defn app []
  [:div
   {}
   [winning-number]
   [start-btn]
   [results]
   [:h2 "Winners"]
   [winners-list (:winners @state)]
   ])

;; -------------------------
;; Initialize app

(r/render [app] (.getElementById js/document "app"))
