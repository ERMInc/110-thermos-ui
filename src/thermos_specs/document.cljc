;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.document
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-specs.measure :as measure]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sr :refer-macros [transform setval]]
            [clojure.test :as test]
            [spec-tools.data-spec :as ds]
            ))

(s/def ::document
  (s/keys :req [::candidates
                ::view/view-state

                ::demand/emissions

                ::npv-term
                ::npv-rate

                ::loan-term
                ::loan-rate

                ::emissions-cost  ;; numbers
                ::emissions-limit ;; {:enabled and :value}
                
                ::mip-gap
                ::param-gap

                ::flow-temperature
                ::return-temperature
                ::ground-temperature

                ::tariffs

                ::insulation
                ::alternatives

                ::maximum-supply-sites

                ::pumping-overhead
                ::pumping-cost-per-kwh
                ::pumping-emissions


                ::pipe-costs
                ::medium
                ]
          :opt [::solution/summary
                ::deletions]))

(defn redundant-key
  "Make a spec which checks a map, so that for every map entry, the
  value of the entry is also map, which contains the entry's key under
  the KEY argument, so e.g.

  (redundant-key :x) would pass {:y {:x :y}} but would fail {:y
  {:x :z}} because :z is not :y
  "
  [key]
  (s/every (fn [[id val]] (and (map? val)
                               (= id (key val)))) :kind map? :into {}))

(s/def ::medium #{:hot-water :saturated-steam})

(defn medium [doc] (::medium doc :hot-water)) ;; to give default value

(comment
  "example pipe costs"

  {:rows
   {50
    {:capacity-kwp 1234
     :losses-kwh 123
     :pipe 100
     0 200 ;; cost of hard dig/m
     }
    }

   :civils ;; could set a default in here or outwith
   {0 "Hard dig"}})


(s/def ::pipe-costs
  (ds/spec
   ::pipe-costs

   {:rows
    {number? ;; diameter =>
     {(ds/opt :capacity-kwp) number?
      (ds/opt :losses-kwh) number?
      (ds/req :pipe) number?
      ;; plus civil-id => number?
      }
     
     }
    :civils
    {int? string?} ;; the names only

    :default-civils
    (ds/maybe int?)
    }))

(s/def ::deletions (s/* ::candidate/id))

(s/def ::tariffs
  (s/and (redundant-key ::tariff/id)
         (s/map-of ::tariff/id ::tariff/tariff)))

(s/def ::candidates
  (s/and
   (redundant-key ::candidate/id)
   (s/map-of ::candidate/id ::candidate/candidate)))

(defn- is-interesting? [candidate]
  (or (::candidate/modified candidate)
      (#{:optional :required} (::candidate/inclusion candidate))))

(defn- keep-interesting-candidates
  "Given a document, remove any candidates which are not either ::candidate/modified,
  or having ::candidate/inclusion :optional or :required"
  [doc]
  {:test #(test/is
           (= (set (keys (::candidates
                          (keep-interesting-candidates
                           {::candidates {"one" {::candidate/inclusion :optional}
                                          "two" {::candidate/inclusion :forbidden
                                                 ::candidate/modified true}
                                          "three" {}
                                          "four" {::candidate/inclusion :forbidden}
                                          "five" {::candidate/modified true}
                                          }}
                           ))))
              #{"one" "two" "five"}))}
  (->> doc
       (sr/setval
        [::candidates sr/MAP-VALS
         (sr/not-selected?
          (some-fn
           ::candidate/modified
           (comp #{:optional :required} ::candidate/inclusion)))]
        sr/NONE)))

(defn- is-transient-key?
  "A key is transient if it's namespaced and not within thermos-specs."
  [x]
  {:test #(test/is (and (is-transient-key? :some.ns/kw)
                        (not (is-transient-key? :some-kw))
                        (not (is-transient-key? ::some-kw))
                        (not (is-transient-key? 1))
                        (not (is-transient-key? "str"))))}
  (and (keyword? x)
       (namespace x)
       (not (str/starts-with? (namespace x) "thermos-specs"))))

(def ALL-MAPS
  "A specter navigator that navigates to all the maps in a structure."
  (sr/recursive-path
   [] p
   (sr/cond-path
    map?
    (sr/continue-then-stay sr/MAP-VALS p)

    coll?
    [sr/ALL p]
    )))

(defn- remove-transient-keys
  "Some values are stored in the document which we don't want to persist.
  They are all stored in maps under namespaced keywords which are not
  the thermos-specs namespace or below.

  This function removes all these keys"
  [doc]
  {:test #(test/is
           (and
            (= (remove-transient-keys {}) {})
            (= (remove-transient-keys {::keep-this 1}) {::keep-this 1})
            (= (remove-transient-keys {::keep-this 1
                                       :keep-this-also 2
                                       :but-remove/this 3})
               {::keep-this 1 :keep-this-also 2})
            (= (remove-transient-keys
                {::recursively {::keep-this 1
                                :keep-this-also 2
                                :but-remove/this 3}})
               {::recursively {::keep-this 1 :keep-this-also 2}})
            (= (remove-transient-keys
                {::in-collections [{:remove/this 1} {:remove/that 2}]})
               {::in-collections [{} {}]})))}
  
  (->> doc
       (sr/setval
        [ALL-MAPS sr/MAP-KEYS is-transient-key?]
        sr/NONE)))

(defn keep-interesting
  "Return a version of document in which only the interesting bits are retained.
  This strips off anything which is not part of one of the specs, and
  anything that cannot participate in the problem."
  [document]

  (->> document
       (keep-interesting-candidates)
       (remove-transient-keys)))

(defn map-candidates
  "Go through a document and apply f to all the indicated candidates."
  ([doc f]
   (sr/transform
    [::candidates sr/MAP-VALS]
    f
    doc))

  ([doc f ids]
   (if (empty? ids)
     doc
     (sr/transform
      [::candidates (sr/submap ids) sr/MAP-VALS]
      f
      doc)
     )))

(defn map-buildings [doc f]
  (sr/transform
   [::candidates sr/MAP-VALS (sr/selected? candidate/is-building?)]
   f doc))

(defn map-paths [doc f]
  (sr/transform
   [::candidates sr/MAP-VALS (sr/selected? candidate/is-path?)]
   f doc))

(defn candidates-by-type
  "Return candidates from the given document, grouped by type"
  [doc]
  (group-by ::candidate/type (vals (::candidates doc))))

(let [solution-ns (namespace ::solution/included)
      is-solution-keyword #(and (keyword? %)
                                (= (namespace %) solution-ns))]
  (defn remove-solution
    "Remove everything to do with a solution from this document"
    [doc]
    (-> (apply dissoc doc (filter is-solution-keyword (keys doc)))
        (map-candidates #(select-keys % (remove is-solution-keyword (keys %)))))))

(defn has-solution? [document]
  (contains? document ::solution/state))

(defn has-supply-solution? [document]
  (contains? document ::solution/supply-solution))

(defn minimum-key [m]
  (when-let [keys (seq (keys m))] (reduce min keys)))

(defn civil-cost-name [doc cost-id]
  (if cost-id
    (-> doc ::pipe-costs :civils (get cost-id "Missing value!"))
    (if-let [cost-id (:default-civils (::pipe-costs doc))]
      (-> doc ::pipe-costs :civils (get cost-id "Missing value!")
          (str " (default)"))
      "None")))

(defn civil-cost-by-name [doc cost-name]
  (when-not (str/blank? cost-name)
    (->> doc ::pipe-costs :civils
         (keep
          (fn [[id nm]] (when (= nm cost-name) id)))
         (first))))

(defn path-cost [path document]
  (if (::path/exists path)
    0
    (let [default (:default-civils (::pipe-costs document))

          cost-id (::path/civil-cost-id path default)
          length  (::path/length path 0)
          rows    (:rows (::pipe-costs document))

          dia     (when (seq rows) (reduce min (keys rows)))

          row     (get rows dia)
          
          cost-per-m (+ (get row :pipe 0)
                        (get row cost-id 0))
          ]

      (* cost-per-m length)
      )))

(defn is-runnable?
  "Tells you if the document might be runnable.
  At the moment checks for the presence of a supply and some demands.
  "
  [document]
  (and (not (empty? (::candidates document)))
       (some (comp #{:building} ::candidate/type) (vals (::candidates document)))
       (some ::supply/capacity-kwp (vals (::candidates document)))))

(defn tariff-for-id [doc tariff-id]
  (if (= :market tariff-id)
    tariff-id
    (let [tariffs (::tariffs doc)]
      (when (seq tariffs)
        (or (get tariffs tariff-id)
            (get tariffs
                 (reduce min (keys tariffs))))))))

(defn tariff-name [doc tariff-id]
  (if (= tariff-id :market)
    "Market"
    (or (::tariff/name (tariff-for-id doc tariff-id)) "None")))

(defn profile-name [doc profile-id]
  (let [heat-profiles (::supply/heat-profiles doc)
        default-profile
        (or (::supply/default-profile doc)
            (minimum-key heat-profiles))]
    (:name (or (get heat-profiles profile-id)
               (get heat-profiles default-profile)))))

(defn remove-tariff
  {:test #(test/is
           (= {::tariffs {1 {}}
               ::candidates {1 {::tariff/id 1}
                             2 {}}}
              (remove-tariff
               {::tariffs {1 {} 2 {}}
                ::candidates {1 {::tariff/id 1}
                              2 {::tariff/id 2}}}
               2)))}
  [doc tariff-id]
  (-> doc
      (update ::tariffs dissoc tariff-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::tariff/id (sr/pred= tariff-id)]
            sr/NONE))))

(defn connection-cost-for-id [doc connection-cost-id]
  (let [connection-costs (::connection-costs doc)]
    (when (seq connection-costs)
      (or (get connection-costs connection-cost-id)
          (get connection-costs
               (reduce min (keys connection-costs)))))))

(defn connection-cost-name [doc connection-cost-id]
  (or (::tariff/name (connection-cost-for-id doc connection-cost-id)) "None"))

(defn remove-connection-cost
  {:test #(test/is
           (= {::connection-costs {1 {}}
               ::candidates {1 {::tariff/cc-id 1}
                             2 {}}}
              (remove-connection-cost
               {::connection-costs {1 {} 2 {}}
                ::candidates {1 {::tariff/cc-id 1}
                              2 {::tariff/cc-id 2}}}
               2)))}
  [doc connection-cost-id]
  (-> doc
      (update ::connection-costs dissoc connection-cost-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::tariff/cc-id (sr/pred= connection-cost-id)]
            sr/NONE
            ))))

(s/def ::insulation
  (s/and
   (redundant-key ::measure/id)
   (s/map-of ::measure/id ::measure/insulation)))

(s/def ::alternatives
  (s/and
   (redundant-key ::supply/id)
   (s/map-of ::supply/id ::supply/alternative)))
  
(defn alternative-for-id [doc alternative-id]
  (get (::alternatives doc) alternative-id))

(defn insulation-for-id [doc insulation-id]
  (get (::insulation doc) insulation-id))

(defn remove-alternative [doc alt-id]
  (-> doc
      (update ::alternatives dissoc alt-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS
             (sr/multi-path
              [::demand/counterfactual (sr/pred= alt-id)]
              [::demand/alternatives (sr/set-elem alt-id)])]
            sr/NONE))))

(defn remove-insulation [doc ins-id]
  (-> doc
      (update ::insulation dissoc ins-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::demand/insulation (sr/set-elem ins-id)]
            sr/NONE))))

(defn remove-candidates [doc candidates]
  (-> doc
      (update ::candidates #(apply dissoc % candidates))
      (update ::deletions concat candidates)))

(defn remove-candidate [doc id]
  (-> doc
      (update ::candidates dissoc id)
      (update ::deletions conj id)))

(defn add-candidates [doc candidates]
  (-> doc
      (update ::candidates
              merge (into {} (for [c candidates] [(::candidate/id c) c])))))

(defn add-candidate [doc candidate]
  (update doc ::candidates
          assoc (::candidate/id candidate) candidate))

(defn is-cooling? [doc]
  (< (::flow-temperature doc) (::return-temperature doc))
  ;; (= :cooling (::mode doc))
  )

(defn mode [doc]
  (if (is-cooling? doc)
    :cooling
    :heating))

(defn delta-t [doc]
  (Math/abs (- (::flow-temperature doc) (::return-temperature doc))))

(defn mean-temperature [doc]
  (/ (+ (::flow-temperature doc 90) (::return-temperature doc 60))
     2.0))

(defn delta-ground [doc]
  (Math/abs
   (- (mean-temperature doc)
      (::ground-temperature doc))))

(defn steam-pressure [doc] (::steam-pressure doc))
(defn steam-velocity [doc] (::steam-velocity doc))

(defn ungroup-candidates
  "Clear the ::demand/group on all these candidates"
  [doc candidates-ids]
  (sr/transform
   [::candidates
    (sr/submap candidates-ids)
    sr/MAP-VALS]
   #(-> % (dissoc ::demand/group) (assoc ::candidate/modified true))
   doc))

(defn- next-group [doc]
  (->> (::candidates doc)
       (vals)
       (keep ::demand/group)
       (filter integer?) ;; we could have non-integer ids from elsewhere
       (reduce max -1)
       (inc)))

(defn group-candidates
  "Given a document, change it so all the given candidates are in a
  single group That means they all have the same ::demand/group value,
  and no other candidates have that value."

  {:test
   #(do
      (test/is
       (-> {::candidates
            {0 {} 1 {}
             2 {} 3 {::demand/group 0}}}
           (group-candidates (list 0 1))
           (= {::candidates
               {0 {::demand/group 1 ::candidate/modified true}
                1 {::demand/group 1 ::candidate/modified true}
                2 {} 3 {::demand/group 0}}})))
      (test/is
       (-> {::candidates
            {0 {} 1 {}
             2 {} 3 {}}}
           (group-candidates (list 0 1))
           (= {::candidates
               {0 {::demand/group 0 ::candidate/modified true}
                1 {::demand/group 0 ::candidate/modified true}
                2 {} 3 {}}})))

      (test/is
       (-> {::candidates
            {0 {::demand/group 0} 1 {::demand/group 0}}}
           (group-candidates (list 0))
           (= {::candidates
               {0 {} 1 {::demand/group 0 ::candidate/modified true}}})))
      )}

  [doc candidates-ids]
  (cond
    (empty? candidates-ids)
    doc

    (empty? (rest candidates-ids))
    (ungroup-candidates doc candidates-ids) ;; grouping 1 thing is the
                                            ;; same as ungrouping it.
    :else
    (let [next-group (next-group doc)]
      (sr/transform
       [::candidates
        (sr/submap candidates-ids)
        sr/MAP-VALS
        (sr/selected?
         [(sr/must ::candidate/type) #{:building}] )
        
        ]
       #(assoc % ::demand/group next-group ::candidate/modified true)
       doc))))

(defn select-group-members [doc]
  (let [selected-groups (set (sr/select
                              [::candidates sr/MAP-VALS
                               (sr/pred ::candidate/selected)
                               (sr/must ::demand/group)]
                              doc))]
    (sr/multi-transform

     [::candidates sr/MAP-VALS
      (sr/multi-path
       [(sr/pred (comp selected-groups ::demand/group))
        ::candidate/selected
        (sr/terminal-val true)]
       [(sr/pred (comp not selected-groups ::demand/group))
        ::candidate/selected
        (sr/terminal-val sr/NONE)]
       )]

     doc)))

(defn remove-civils [doc cid]
  (sr/multi-transform
   (sr/multi-path
    [::pipe-costs
     (sr/multi-path
      [:default-civils (sr/pred= cid) (sr/terminal-val sr/NONE)]
      [:civils cid (sr/terminal-val sr/NONE)]
      [:rows sr/MAP-VALS cid (sr/terminal-val sr/NONE)])]
    [::candidates sr/MAP-VALS
     ::path/civil-cost-id (sr/pred= cid)
     (sr/terminal-val sr/NONE)])
   doc))
