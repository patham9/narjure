(ns narjure.memory-management.termlink-utils
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure
     [global-atoms :refer :all]
     [bag :as b]
     [debug-util :refer :all]
     [control-utils :refer :all]
     [defaults :refer :all]
     [budget-functions :refer :all]]
    [narjure.memory-management
     [concept-utils :refer :all]]
    [clojure.core.unify :refer [unifier]]
    [nal.term_utils :refer [syntactic-complexity termlink-subterms]]
    [narjure.memory-management.local-inference
     [local-inference-utils :refer [get-task-id get-tasks]]
     [belief-processor :refer [process-belief]]
     [goal-processor :refer [process-goal]]
     [quest-processor :refer [process-quest]]
     [question-processor :refer [process-question]]]
    [nal.deriver
     [truth :refer [w2c t-or t-and confidence frequency expectation revision]]
     [projection-eternalization :refer [project-eternalize-to]]]
    [clojure.set :as set])
  (:refer-clojure :exclude [promise await]))

(defn forget-termlinks-absolute
  "This one is for absolute forgetting of links: Remove termlinks whose concepts were forgot,
  remove worst termlinks if above max termlink count."
  []
  (doseq [[tl _] (:termlinks @state)]
    (when (not (b/exists? @c-bag tl))
      (set-state! (assoc @state :termlinks (dissoc (:termlinks @state) tl)))))
  (while (> (count (:termlinks @state)) concept-max-termlinks)
    (let [worst (apply min-key (comp first second) (:termlinks @state))]
      (set-state! (assoc @state :termlinks (dissoc (:termlinks @state) (first worst)))))))

(defn forget-termlinks-relative []
  "Forget termlinks relative."
  (set-state! (assoc @state :termlinks (apply merge (for [[tl [p d]] (:termlinks @state)]
                                                      {tl [(* p d) d]})))))

(defn add-termlink
  "Adds a termlink with term tl and strength strength."
  [tl strength]
  (set-state! (assoc @state :termlinks (assoc (:termlinks @state)
                                         tl strength)))
  (forget-termlinks-absolute))

(defn calc-link-strength
  [tl old-strength]
  (let [prio-me (concept-priority (:id @state))
        prio-other (concept-priority tl)]
    (if (and prio-me prio-other)
      (let [init-penalty 0.1
            association (* init-penalty (t-and prio-me prio-other))
            initial-strength [association 0.999]]
        [(max (first initial-strength)
              (first old-strength))
         (max (second initial-strength)
              (second old-strength))])
      old-strength)))

(defn link-feedback-handler
  "Link growth by contextual relevance of the inference and the result, as well as usefulness of the result."
  [from [_ [derived-task belief-concept-id]]]                       ;this one uses the usual priority durability semantics
  (try
    (let [p-d (:budget derived-task)
          old-value ((:termlinks @state) (:statement derived-task))
          old-p-d (if old-value old-value [0.0 0.0])]
      (when (and p-d (:budget derived-task))
        (add-termlink belief-concept-id [(max (first p-d)
                                              (first old-p-d))
                                         (max (second p-d)
                                              (second old-p-d))]))
      )
    (catch Exception e (println "fail"))))

(defn not-outdated-record-entry [[id time]]
  (< (- @nars-time time) 250))

(defn get-termlink-endpoints
  "Get the link endpoints, namely the concepts which the concept links to: their id as well as priority."
  [record]
  (let [initbag (b/default-bag concept-max-termlinks)]
    (try
      (reduce (fn [a b] (b/add-element a b)) initbag (for [[k v] (filter (fn [[k _]] (or (nil? record)
                                                                                         (not (some (fn [[id time]]
                                                                                                      (and
                                                                                                        (not-outdated-record-entry [id time])
                                                                                                        (= id k)))

                                                                                                    record))))
                                                                         (:termlinks @state))]
                                                      {:priority (+ (first v)
                                                                       #_(:priority (first (b/get-by-id @c-bag k))))
                                                       :id       k}))
      (catch Exception e (print "")
                         ;(println (str "error in get-termlink-endpoints: " e)
                         ))))

(defn select-termlink-ref
  "Select the termlink probabilistically, taking link strength and target priorities into account."
  [record]
  ;now search through termlinks, get the endpoint concepts, and form a bag of them
  (let [initbag (b/default-bag concept-max-termlinks)
        resbag (get-termlink-endpoints record)]
    ;now select an element from this bag
    (if (and resbag (pos? (b/count-elements resbag)))
      (let [[beliefconcept _] (b/get-by-index resbag (selection-fn (b/count-elements resbag)))]
        #_(forget-termlink (:id beliefconcept))               ;apply forgetting for termlinks only on selection
        [(:id beliefconcept) (get-ref-from-term (:id beliefconcept))])
      nil)))