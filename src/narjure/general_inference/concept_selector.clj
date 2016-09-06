(ns narjure.general-inference.concept-selector
  (:require
    [co.paralleluniverse.pulsar
     [core :refer :all]
     [actors :refer :all]]    [narjure.global-atoms :refer [c-bag lense-taskbags lense-termlinks]]
    [narjure.bag :as b]
    [narjure.defaults :refer [max-concept-selections]]
    [clojure.math.numeric-tower :as math]
    [narjure.memory-management.concept-utils :refer [concept-observable]]
    [taoensso.timbre :refer [debug info]]
    [narjure.debug-util :refer :all]
    [narjure.control-utils :refer :all]
    [nal.term_utils :refer [has-common-link-subterm]])
  (:refer-clojure :exclude [promise await]))

(def aname :concept-selector)
(def display (atom '()))
(def search (atom ""))

(defn strengthen-temporal-link
  " creates a term-link between last-selected concept and the currently selected concept"
  [state selected]
  (when-let [last-selected (:last-selected state)] ;the last selected observable concept
    (let [pre-temporal (and (concept-observable (:id selected))
                    (concept-observable (:id last-selected)))
          pre-semantic (has-common-link-subterm (:id selected) (:id last-selected))]
      (when (or pre-temporal pre-semantic)   ; todo need to be able to link to itself here (&/, a, a) is valid sequence
        (cast! (:ref selected) [:termlink-strengthen-msg [(:id last-selected) pre-temporal]])
        (cast! (:ref last-selected) [:termlink-strengthen-msg [(:id selected) pre-temporal]]))))
  (when true #_(concept-observable (:id selected))
    (set-state! (assoc state :last-selected selected))))

(defn inference-tick-handler
  "Select n concepts for inference and post
   inference-request-message to each selected
   concept"
  [from [msg]]
  (doseq [[k v] @lense-taskbags]                            ;is empty if not in debug so can stay here for now since we
    (when-not (b/exists? @c-bag k)                          ;don't want mem to get full just because lense isn't running
      (swap! lense-taskbags (fn [old] (dissoc old k))) ;element doesnt exist anymore
      (swap! lense-termlinks (fn [old] (dissoc old k)))))
  ; (dotimes [n (min (b/count-elements @c-bag) 1)]
  ;one concept for inference is enough for now ^^

  (doseq [selected (select-concepts max-concept-selections @c-bag)]
    (strengthen-temporal-link @state selected)
    (when (sufficient-priority? selected)
      (cast! (:ref selected) [:inference-request-msg (:id selected)])
      (debuglogger search display (list "Concept selected: " [:task selected :priority (:priority selected)])))))

(defn initialise
  "Initialises actor:
      registers actor and sets actor state"
  [aname actor-ref]
  (reset! display '())
  (register! aname actor-ref)
  (set-state! {}))

(defn msg-handler
  "Identifies message type and selects the correct message handler.
   if there is no match it generates a log message for the unhandled message "
  [from [type :as message]]
  ;(debuglogger display message) since tick is uninteresting we use what is selected
  (case type
    :inference-tick-msg (inference-tick-handler from message)
    (debug aname (str "unhandled msg: " type))))

(defn concept-selector []
  (gen-server
    (reify Server
      (init [_] (initialise aname @self))
      (terminate [_ cause] #_(info (str aname " terminated.")))
      (handle-cast [_ from id message] (msg-handler from message)))))
