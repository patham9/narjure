(ns narjure.general-inference.general-inferencer
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [nal
     [deriver :refer [inference]]
     [term_utils :refer [syntactic-complexity operation? interval?]]]
[nal.deriver :refer [inference]]
[taoensso.timbre :refer [debug info]]
[narjure
     [debug-util :refer :all]
     [budget-functions :refer [derived-budget]]
     [defaults :refer :all]
     [control-utils :refer [make-evidence non-overlapping-evidence?]]])
  (:refer-clojure :exclude [promise await]))

(def display (atom '()))
(def search (atom ""))

(defn do-inference-handler
  "Processes :do-inference-msg:
    generates derived results, budget and occurrence time for derived tasks.
    Posts derived sentences to task creator"
  [from [msg [task-concept-id belief-concept-id task belief debug]]]
  (try
    (when (non-overlapping-evidence? (:evidence task) (:evidence belief))
      (let [pre-filtered-derivations (inference task belief)
            filtered-derivations (filter #(not= (:statement %) (:parent-statement task)) pre-filtered-derivations)
            evidence (make-evidence (:evidence task) (:evidence belief))
            derived-load-reducer (whereis :derived-load-reducer)]
        (when-not (empty? evidence)
          (doseq [derived filtered-derivations]
            (let [sc (syntactic-complexity (:statement derived))
                  derived (assoc derived :sc sc)            ; required for derived-budget
                  budget (derived-budget task derived)
                  derived-task (assoc derived :budget budget
                                              :parent-statement (:statement task)
                                              :evidence evidence)]
              (when (and budget
                         (< sc @max-term-complexity)
                         (> (first budget) priority-threshold)
                         (or (not (:truth derived-task))
                             (> (rand) 0.98)
                             (not= (first (:statement derived-task)) '--)
                             #_(>= (first (:truth derived-task)) 0.5))
                         (let [st (:statement derived-task)]
                           (and
                                (not (and (coll? st)     ;not allows =/> operation   TODO probably revise
                                          (or (= (first st) 'pred-impl)
                                              (= (first st) '=|>))
                                          (operation? (last st))))
                                (not (and (coll? st)   ;not allow interval to be the subject of predicate of ==>
                                          (or (= (first st) 'pred-impl)
                                              (= (first st) '=|>)
                                              (= (first st) '<|>)
                                              (= (first st) '</>)
                                              (= (first st) '==>)
                                              (= (first st) '<=>))
                                          (or (interval? (second st))
                                              (interval? (nth st 2)))))
                                (not (and (coll? st)    ;also don't allow sequence as predicate
                                          (or (= (first st) 'pred-impl)
                                              (= (first st) '=|>)
                                              (= (first st) '==>))
                                          (and (coll? (nth st 2))
                                                   (or (= (first (nth st 2)) 'seq-conj)
                                                       (= (first (nth st 2)) '&|)))))
                                (not (and (coll? st)    ;also don't allow sequence as predicate
                                          (= (first st) 'seq-conj)
                                          (= (:task-type derived-task) :belief)
                                          (operation? (second st))))))
                         #_(coll? (:statement derived-task)))
                #_(when debug
                  (println (str "|||||\n" derived-task)))
                (cast! derived-load-reducer [:derived-sentence-msg [task-concept-id
                                                                    belief-concept-id
                                                                    derived-task]])))))))
    (catch Exception e (debuglogger search display (str "inference error " (.toString e))))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  (debuglogger search display message)
  (case type
    :do-inference-msg (do-inference-handler from message)
    (debug :ge (str "unhandled msg: " type))))

(defn general-inferencer [aname]
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
