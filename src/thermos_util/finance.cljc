;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.finance
  (:require [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-util
             :refer [safe-div]
             :refer-macros [safe-div]]))

(defn pv
  "Calculate the NPV of a series of values given a discount rate"
  [npv-rate vals]

  (if (zero? npv-rate)
    (reduce + vals)
    (reduce + (map-indexed
               (fn [i v] (safe-div v (Math/pow (+ 1 npv-rate) i))) vals))))

(defn annualize
  "Convert a capital cost right now into a series of repayments over time"
  [loan-rate loan-term principal]

  (if (zero? loan-rate)
    (repeat loan-term (safe-div principal loan-term))
    
    (let [repayment (safe-div (* principal loan-rate)
                       (- 1 (/ 1 (Math/pow (+ 1 loan-rate)
                                           loan-term))))]
      (repeat loan-term repayment))))

(defn objective-capex-value [doc opts value exists]
  (let [{should-annualize :annualize
         should-recur :recur
         period :period
         loan-rate :rate}
        opts

        period    (max 1 (or period 1))
        loan-rate (or loan-rate 0)

        {npv-rate ::document/npv-rate
         npv-term ::document/npv-term} doc

        npv-rate (or npv-rate 0)
        npv-term (max 1 (or npv-term 1))
        
        payments
        (if should-annualize
          (annualize loan-rate period value)
          (take (max 1 period)
                (concat [value] (repeat 0))))

        payments
        (if should-recur
          (take npv-term (cycle payments))
          payments)

        payments
        (if exists
          (concat (repeat period 0)
                  (drop period payments))
          payments)
        
        total-value (reduce + payments)
        ]
    {:present (pv npv-rate payments)
     :total total-value
     :annual (safe-div total-value period)
     :principal value}))

(defn objective-opex-value [doc value]
  (let [payments (repeat (::document/npv-term doc 1) value)
        total (reduce + payments)]
    {:present (pv (::document/npv-rate doc 0) payments)
     :total total
     :annual value}))

(defn adjusted-value
  "Determine the objective function contribution for a type of cost.
  This is affected by settings in the document."
  [doc type value]
  (-> (case type
        :connection-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :connection])
                               value
                               false)
        
        :supply-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :supply])
                               value
                               false)
        
        :pipe-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :pipework])
                               value
                               false)

        :existing-pipe-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :pipework])
                               value
                               true)
        
        :insulation-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :insulation])
                               value
                               false)
        
        :alternative-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :alternative])
                               value
                               false)

        :counterfactual-capex
        (objective-capex-value doc
                               (get-in doc [::document/capital-costs :alternative])
                               value
                               true)

        (:supply-heat :supply-opex :emissions-cost :heat-revenue :alternative-opex :supply-pumping)

        ;; these things are not capexes so we account for them the other way
        (objective-opex-value doc value))
      (assoc :type type)
      ))

(defn objective-value [doc type value]
  (:present (adjusted-value doc type value)))

(defn emissions-value [doc em kg]
  (let [kg (or kg 0)
        price (get (::document/emissions-cost doc) em 0)]
    (assoc (adjusted-value doc :emissions-cost (* price kg))
           :type em
           :kg kg)))

(defn cash-flows-for-irr
  "Extract cash flows for IRR calculation from document and solution data.
  Includes only network infrastructure costs and revenues: pipework, supply, connections.
  Excludes: alternative heating systems, insulation (building-level improvements).
  Returns a vector of cash flows where negative values are costs and positive are revenues."
  [document]
  (let [solution-members (->> document
                              ::document/candidates
                              vals
                              (filter candidate/in-solution?))
        {paths :path
         buildings :building}
        (group-by ::candidate/type solution-members)

        alts (filter ::solution/alternative buildings)
        supplies (filter candidate/supply-in-solution? buildings)
        demands (filter candidate/is-connected? buildings)
        insulated (filter (comp seq ::solution/insulation) buildings)

        {npv-term ::document/npv-term} document
        npv-term (max 1 (or npv-term 1))

        ;; Start with all zeros for each year
        cash-flows (vec (repeat npv-term 0))

        ;; Helper function to add capital expenditure payments with REPEX
        add-capex-flows (fn [flows [capex-amount cost-type]]
                         (if (and capex-amount (number? capex-amount) (pos? capex-amount))
                           (let [opts (get-in document [::document/capital-costs cost-type] {})
                                 {should-annualize :annualize
                                  should-recur :recur
                                  period :period
                                  loan-rate :rate} opts
                                 period (max 1 (or period 1))
                                 loan-rate (or loan-rate 0)

                                 ;; Calculate payment schedule
                                 payments (if should-annualize
                                           (annualize loan-rate period capex-amount)
                                           (take (max 1 period) (concat [capex-amount] (repeat 0))))

                                 ;; Handle REPEX - recurring payments over NPV term
                                 payments (if should-recur
                                           (take npv-term (cycle payments))
                                           payments)]

                             ;; Add payments to cash flows (negative for costs)
                             (map-indexed
                              (fn [i existing-flow]
                                (+ existing-flow (- (nth payments i 0))))
                              flows))
                           ;; Return unchanged flows if no valid capex amount
                           flows))

        ;; Add all capital expenditures with REPEX (network infrastructure only)
        cash-flows (reduce add-capex-flows cash-flows
                          (concat
                           ;; Pipework capex
                           (map #(vector (get (::solution/pipe-capex %) :principal 0) :pipework) paths)
                           ;; Supply capex
                           (map #(vector (get (::solution/supply-capex %) :principal 0) :supply) supplies)
                           ;; Connection capex
                           (map #(vector (get (::solution/connection-capex %) :principal 0) :connection) demands)))

        ;; Calculate annual operating cash flows (positive for revenues, negative for costs)
        ;; Network only - excluding alternative system costs
        annual-opex (+
                     ;; Supply operating costs (negative)
                     (- (reduce + 0 (mapcat (fn [supply]
                                             (filter number?
                                                     [(get (::solution/supply-opex supply) :annual 0)
                                                      (get (::solution/heat-cost supply) :annual 0)
                                                      (get (::solution/pumping-cost supply) :annual 0)]))
                                           supplies)))
                     ;; Heat revenues (positive)
                     (reduce + 0 (keep (fn [demand]
                                        (get (::solution/heat-revenue demand) :annual 0))
                                       demands))
                     ;; Emissions costs (negative) - network supply and pumping only
                     (- (reduce + 0 (mapcat (fn [supply]
                                             (filter number?
                                                     (concat
                                                      (vals (or (::solution/supply-emissions supply) {}))
                                                      (vals (or (::solution/pumping-emissions supply) {})))))
                                           supplies))))

        ;; Add annual operating cash flows to each year
        cash-flows (mapv #(+ % annual-opex) cash-flows)]

    cash-flows))

(defn irr
  "Calculate Internal Rate of Return (IRR) - the discount rate that makes NPV = 0.
  Uses Newton-Raphson method for numerical solution.
  Returns nil if no solution is found within reasonable bounds."
  [cash-flows]
  (when (and (seq cash-flows)
             (some neg? cash-flows)  ; Must have some negative cash flows
             (some pos? cash-flows)) ; Must have some positive cash flows
    (let [max-iterations 100
          tolerance 1e-8
          max-rate 10.0  ; Maximum rate to consider (1000%)
          min-rate -0.99  ; Minimum rate to avoid division issues

          ;; NPV derivative function for Newton-Raphson
          npv-derivative (fn [rate]
                          (if (zero? rate)
                            (reduce + (map-indexed (fn [i v] (* (- i) v)) cash-flows))
                            (reduce + (map-indexed
                                      (fn [i v]
                                        (/ (* (- i) v) (Math/pow (+ 1 rate) (inc i))))
                                      cash-flows))))

          ;; Newton-Raphson iteration
          newton-raphson (fn newton-raphson [rate iteration]
                          (if (>= iteration max-iterations)
                            nil  ; No convergence
                            (let [npv (pv rate cash-flows)  ; Use existing pv function
                                  npv-prime (npv-derivative rate)]
                              (if (< (Math/abs npv) tolerance)
                                rate  ; Converged
                                (if (zero? npv-prime)
                                  nil  ; Derivative is zero, can't continue
                                  (let [new-rate (- rate (/ npv npv-prime))]
                                    (if (or (< new-rate min-rate) (> new-rate max-rate))
                                      nil  ; Rate out of bounds
                                      (newton-raphson new-rate (inc iteration)))))))))

          ;; Try different starting points if Newton-Raphson fails
          try-starting-points (fn [starting-points]
                               (loop [points starting-points]
                                 (when (seq points)
                                   (if-let [result (newton-raphson (first points) 0)]
                                     result
                                     (recur (rest points))))))]

      ;; Try various starting points
      (try-starting-points [0.1 0.2 0.05 0.15 0.3 0.5 -0.1 -0.2]))))
