(ns thermos-frontend.params.tariffs
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]
            [clojure.string :as string]))

(defn- vconj [v e]
  (conj (vec v) e))

(def standing-charge-unit "¤/yr")
(def capacity-charge-unit "¤/kWp.yr")
(def unit-rate-unit "c/kWh")
(def conn-fixed-unit "¤")
(def conn-var-unit "¤/kWp")

(defn- connection-cost-row
  [*document *connection-costs id]
  (let [get #(get-in @*connection-costs [id %])
        put #(swap! *connection-costs assoc-in [id %1] %2)
        delete-connection-cost #(swap! *document document/remove-tariff id)]
    [:tr 
     [:td [inputs/text
           :style {:width :50%}
           :placeholder (str "Connection cost " id)
           :value (get ::tariff/name)
           :on-change #(put ::tariff/name (target-value %))]]
     [:td [:label
           [inputs/number
            {:title "The fixed part of the capital cost of connecting a building."
             :max 1000
             :min 0
             :value (get ::tariff/fixed-connection-cost)
             :on-change #(put ::tariff/fixed-connection-cost %)
             }]
           conn-fixed-unit]]
     [:td [:label
           [inputs/number
            {:title "The variable part of the capital cost of connecting a building."
             :max 100
             :min 0
             :scale 100
             :step 0.1
             :value (get ::tariff/variable-connection-cost)
             :on-change #(put ::tariff/variable-connection-cost %)
             }]
           conn-var-unit]]
     
     [:td [:button.button {:style {:margin-left :1em}
                            :on-click delete-connection-cost}
            symbols/dustbin]]
     ]))

(defn- tariff-row
  [*document *tariffs id]
  (let [get #(get-in @*tariffs [id %])
        put #(swap! *tariffs assoc-in [id %1] %2)
        delete-tariff #(swap! *document document/remove-tariff id)]
    [:tr 
     [:td [inputs/text
           :style {:width :50%}
           :placeholder (str "Tariff " id)
           :value (get ::tariff/name)
           :on-change #(put ::tariff/name (target-value %))]]
     [:td [:label
           [inputs/number
            {:title "A fixed annual payment from customers on this tariff."
             :max 1000
             :min 0
             :value (get ::tariff/standing-charge)
             :on-change #(put ::tariff/standing-charge %)
             }]
           standing-charge-unit]]
     [:td [:label
           [inputs/number
            {:title "The heat price paid by customers on this tariff."
             :max 100
             :min 0
             :scale 100
             :step 0.1
             :value (get ::tariff/unit-charge)
             :on-change #(put ::tariff/unit-charge %)
             }]
           unit-rate-unit]]
     [:td [:label
           [inputs/number
            {:title "An annual payment per kWp capacity from customers on this tariff."
             :max 1000
             :min 0
             :value (get ::tariff/capacity-charge)
             :on-change #(put ::tariff/capacity-charge %)
             }]
           capacity-charge-unit]
      ]
     [:td [:button.button {:style {:margin-left :1em}
                            :on-click delete-tariff}
            symbols/dustbin]]
     ]))

(defn tariff-parameters
  [doc]
  (reagent/with-let
    [*tariffs          (reagent/cursor doc [::document/tariffs])
     *connection-costs (reagent/cursor doc [::document/connection-costs])
     *market-rate      (reagent/cursor doc [::tariff/market-discount-rate])
     *market-term      (reagent/cursor doc [::tariff/market-term])
     *market-stick     (reagent/cursor doc [::tariff/market-stickiness])
     ]
    [:div.flex-cols {:style {:flex-wrap :wrap}} ;; TODO make this work
     [:div.card
      [:span "Each building can have an associated tariff, which determines the revenue to the network operator."
       (when-not (seq @*tariffs)
         "At the moment you have no tariffs, so connections will produce no revenue or cost. Click 'add' to define a tariff.")
       ]
      
      [:button.button
       {:on-click #(swap! *tariffs
                          (fn [t]
                            (let [id (inc (reduce max -1 (keys t)))]
                              (assoc
                               t
                               id
                               {::tariff/name ""
                                ::tariff/id id ;; urgh? yes? no?
                                ::tariff/standing-charge 0
                                ::tariff/capacity-charge 0
                                ::tariff/unit-charge 0
                                
                                ::tariff/fixed-connection-cost 0
                                ::tariff/variable-connection-cost 0
                                }))))}
       symbols/plus " Add"]

      [:table
       [:thead
        [:tr
         [:th "Tariff name"]
         [:th "Standing charge"]
         [:th "Unit charge"]
         [:th "Capacity charge"]
         [:th]
         ]
        ]
       [:tbody
        (doall
         (for [id (sort (keys @*tariffs))]
           (tariff-row doc *tariffs id)))]]

      [:div {:style {:margin-top :1em}}
       (reagent/with-let [more (reagent/atom false)]
         [:div
          [:span "For buildings on the "[:b "market"]" tariff, the unit rate is chosen to beat the building's best individual system. "]
          [:button {:on-click #(swap! more not)} (if @more "less..." "more...")]
          (when @more
            [:p "To do this, the building's lowest present cost option is calculated, considering insulation, individual systems, or sticking with the counterfactual. The unit rate is then chosen to give a present cost to the building for connecting to the network which is " [:em "stickiness"] " % less."])
          ]
         )
       [:div {:style {:margin-top :1em}}
        [:label
         "Discount rate: "
         [inputs/number
          {:title "The discount rate used when evaluating market options"
           :min 0
           :max 10
           :scale 100
           :step 0.1
           :value-atom *market-rate
           }
          ]
         "%"]

        [:label
         "Period: "
         [inputs/number
          {:title "The time period used when evaluating market options"
           :min 0
           :max 50
           :step 1
           :value-atom *market-term
           }
          ]
         "yr"]

        [:label
         "Stickiness: "
         [inputs/number
          {:title "The amount by which the heat network unit rate should try to beat the best individual system."
           :min 0
           :max 100
           :scale 100
           :step 0.5
           :value-atom *market-stick}
          ]
         "%"]
        ]
       ]
      ]

     
     
     [:div.card
      [:span "Each building also has associated connection costs, which determine the capital costs of connecting the building to the network. These costs are borne by the network operator."]

      [:button.button
       {:on-click #(swap! *connection-costs
                          (fn [t]
                            (let [id (inc (reduce max -1 (keys t)))]
                              (assoc
                               t
                               id
                               {::tariff/name ""
                                ::tariff/cc-id id
                                ::tariff/fixed-connection-cost 0
                                ::tariff/variable-connection-cost 0
                                }))))}
       symbols/plus " Add"]
      
      [:table
       [:thead
        [:tr
         [:th "Connection cost name"]
         [:th "Fixed cost"]
         [:th "Capacity cost"]
         [:th]]]
       [:tbody
        (doall
         (for [id (sort (keys @*connection-costs))]
           (connection-cost-row doc *connection-costs id)))
        ]
       ]
      
      ]

     
     ]))