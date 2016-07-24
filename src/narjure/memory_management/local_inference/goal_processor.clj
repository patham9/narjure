(ns narjure.memory-management.local-inference.goal-processor
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer [narsese-print]]
    [narjure.bag :as b]
    [narjure.defaults :refer [truth-value]]
    [nal.term_utils :refer [syntactic-complexity termlink-subterms]]
    [nal.deriver.truth :refer [intersection deduction]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [narjure.global-atoms :refer :all]
    [narjure.perception-action.task-creator :refer [get-id]]
    [clojure.core.unify :refer [unify]]
    [nal.term_utils :refer [operation? negation-of-operation? syntactic-complexity]]
    [narjure.memory-management.local-inference.local-inference-utils :refer :all]
    [nal.deriver.truth :refer [t-or frequency confidence expectation desire-strong]]
    [nal.deriver.projection-eternalization :refer [project-eternalize-to]])
  (:refer-clojure :exclude [promise await]))


(defn satisfaction-based-budget-change
  "Budget change based on the satisfaction of the goal by the belief"
  [state goal-task beliefs]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence goal-task) a @nars-time)])
             (filter #(question-unifies (:statement goal-task) (:statement %)) beliefs))]
    (when (not-empty projected-list)
      (doseq [[belief belief-task-projected-to-goal] projected-list]
        (when (better-solution belief goal-task)
          ;update budget and solution
          ;(potential-output-answer state goal-task belief)
          (let [new-goal (reduced-goal-budget-by-belief goal-task belief-task-projected-to-goal)
                new-goal-with-solution (assoc new-goal :solution belief)]
            (update-task-in-tasks state new-goal-with-solution goal-task))
          (let [new-belief (increased-belief-budget-by-goal belief-task-projected-to-goal goal-task)]
            (update-task-in-tasks state (assoc belief :budget (:budget new-belief)) belief)))))))


;how big the truth expectation has to be in order to allow execution.
(def decision-threshold 0.51)                                ;0.6

(defn execute?
  "only execute if desire expectation is above decision threshold"
  [task]
  (> (expectation (:truth task)) decision-threshold))

(defn answer-based-budget-change
  "Budget change based on the answer quality (answering quests, which are questions on desire)."
  [state goal-task quests]
  ;filter goals matching concept content
  ;project-to task time
  ;select best ranked
  (let [projected-list
        (map (fn [a] [a (project-eternalize-to (:occurrence a) goal-task @nars-time)])
             (filter #(question-unifies (:statement %) (:statement goal-task)) quests))]
    (when (not-empty projected-list)
      (doseq [[quest goal-task-projected-to-quest] projected-list]
        (when (better-solution goal-task quest)
          (potential-output-answer state (get-task-id quest) quest goal-task)
          ;update budget and solution
          (let [new-quest (reduced-quest-budget-by-goal quest goal-task-projected-to-quest)
                new-quest-with-solution (assoc new-quest :solution goal-task)]
            (update-task-in-tasks state new-quest-with-solution quest))
          (let [new-goal (increased-goal-budget-by-quest goal-task-projected-to-quest quest)]
            (update-task-in-tasks state (assoc goal-task :budget (:budget new-goal)) goal-task)))))))

(defn process-goal
  "Process a goal task: revise, put into the task bag, check for satisfaction and whether it
   is answered or answers a quest, and see whether it needs to execute."
  [state task cnt]
  ;group-by :task-type tasks

  (let [tasks (get-tasks state)
        groups (group-by :task-type tasks)
        goals (:goal groups)
        beliefs (:belief groups)
        quests (:quest groups)]

    ;also allow revision in subterm concepts! this is why statement is compared to task statement, not to ID!!
    (when-not (and (= (:occurrence task) :eternal)
                   (operation? (:statement task)))
      (let [related-goals (filter (fn [z] (and (same-occurrence-type z task)
                                               (= (:statement z) (:statement task)))) goals)]

        (let [total-revision (reduce (fn [a b] (if (non-overlapping-evidence? (:evidence a) (:evidence b))
                                                 (revise a (project-eternalize-to (:occurrence a) b @nars-time))
                                                 a))
                                     task (shuffle related-goals))]

          ;add revised task to bag
          (add-to-tasks state total-revision)
          ; check to see if revised or task is answer to quest and increase budget accordingly
          ;check whether it is fullfilled by belief and decrease budget accordingly
          (satisfaction-based-budget-change state (:task (first (b/get-by-id (:tasks @state) (get-task-id total-revision)))) (filter #(= (:task-type %) :belief) (get-tasks state)))
          (answer-based-budget-change state (:task (first (b/get-by-id (:tasks @state) (get-task-id total-revision)))) (filter #(= (:task-type %) :quest) (get-tasks state)))
          )))

      ;best operation project goal to current time
      ; if above decision threshold then execute#
      (let [projected-goals (map #(project-eternalize-to @nars-time % @nars-time)
                                 (filter #(= (:statement %) (:statement task))
                                         (filter #(= (:task-type %) :goal) ;re-getting the goals because we also want our just added goal
                                                 (conj tasks task))))] ;needs new task as well
        (if (not-empty projected-goals)
          (let [possible-operations (filter #(and (operation? (:statement %)) (execute? %) (= (:statement %) (:id @state))) projected-goals)
                operation (if (not-empty possible-operations)
                            (apply max-key confidence possible-operations)
                            nil)]
            (when-not (= nil operation)                   ;todo change to - when operation
                #_(println (str (:truth operation) (expectation (:truth operation))))
                ;(println (str  "goal: " operation))
                (when-not (:execution-evidence @state)
                  (set-state! (assoc @state :execution-evidence '())))
                (when (some (fn [z] (not (some #{z} (:execution-evidence @state)))) (:evidence operation))
                  (cast! (whereis :operator-executor) [:operator-execution-msg operation])
                  (set-state! (assoc @state :execution-evidence (take 50 (concat (:evidence operation) (:execution-evidence @state)))))
                  )))))))
