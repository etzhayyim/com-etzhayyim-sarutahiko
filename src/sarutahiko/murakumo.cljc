(ns sarutahiko.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:etzhayyim.com:sarutahiko")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.sarutahiko." name))

(def cell-specs {
  :frame_fabrication {:legacy-cell "frame-fabrication"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "frame_fabrication")]
     :required-gates common-gates
     :trigger "manifest cell frame_fabrication"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :powertrain_assembly {:legacy-cell "powertrain-assembly"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "powertrain_assembly")]
     :required-gates common-gates
     :trigger "manifest cell powertrain_assembly"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :cab_body_forming {:legacy-cell "cab-body-forming"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "cab_body_forming")]
     :required-gates common-gates
     :trigger "manifest cell cab_body_forming"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :final_marriage {:legacy-cell "final-marriage"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "final_marriage")]
     :required-gates common-gates
     :trigger "manifest cell final_marriage"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :paint_finishing {:legacy-cell "paint-finishing"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "paint_finishing")]
     :required-gates common-gates
     :trigger "manifest cell paint_finishing"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :electrical_integration {:legacy-cell "electrical-integration"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "electrical_integration")]
     :required-gates common-gates
     :trigger "manifest cell electrical_integration"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :quality_road_test {:legacy-cell "quality-road-test"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "quality_road_test")]
     :required-gates common-gates
     :trigger "manifest cell quality_road_test"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :emissions_audit {:legacy-cell "emissions-audit"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "emissions_audit")]
     :required-gates common-gates
     :trigger "manifest cell emissions_audit"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :vin_attestation_binder {:legacy-cell "vin-attestation-binder"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "vin_attestation_binder")]
     :required-gates common-gates
     :trigger "manifest cell vin_attestation_binder"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
