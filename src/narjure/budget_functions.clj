(ns narjure.budget-functions
  (:require
    [nal
     [term_utils :refer [precondition-operation-consequent-statement]]]
    [nal.deriver
     [truth :refer [expectation t-or t-and w2c]]]
    [narjure
     [defaults :refer :all]
     [global-atoms :refer :all]
     [control-utils :refer [round2]]
     [debug-util :refer :all]]
    [narjure.memory-management.concept-utils :refer :all]))

(defn truth-to-quality
  "The task quality judged by its truth."
  [t]
  (let [exp (expectation t)
        positive-truth-bias 0.75]
    (max exp (* (- 1.0 exp) positive-truth-bias))))

(defn derived-budget
  "The budget of a by general inference derived task."
  [task derived-task]
  (when (< (:sc derived-task) max-term-complexity)
    (let [truth-quality (if (:truth derived-task) (truth-to-quality (:truth derived-task))
                                                  0.1 #_(w2c 1.0))
          priority (* truth-quality 1.0 #_(first (:budget task)))
          durability (/ (second (:budget task)) (Math/sqrt (:sc derived-task)))
          rescale-factor 0.3 ;should probably not above input belief quality!
          quality (* truth-quality
                     rescale-factor)]
      [priority durability quality])))