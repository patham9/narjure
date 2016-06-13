(ns gui.hnav
  (:require [quil.core :as q]
            [gui.globals :refer :all]))


(defn width []
  (try (if (or (= nil q/width) (= nil (q/width)))
         init-size
         (q/width))
       (catch Exception e init-size)))

(defn height []
  (try (if (or (= nil q/height) (= nil (q/height)))
         init-size
         (q/height))
       (catch Exception e init-size)))

(defn mouse-to-world-coord-x [state x]
  (* (/ 1.0 (:zoom state)) (+ x (- (:difx state)) (- (/ (width) 2.0)))))

(defn mouse-to-world-coord-y [state y]
  (* (/ 1.0 (:zoom state)) (+ y (- (:dify state)) (- (/ (height) 2.0)))))

(defn transform [{:keys [difx dify zoom]}]
  (q/translate (+ difx (* 0.5 (width)))
               (+ dify (* 0.5 (height))))
  (q/scale zoom zoom))

;a utility for copy paste todo cleanup
(defn display-string [debugmessage name]
  (clojure.string/replace (str (if (> (count (debugmessage name)) 1)
                                 (if (not= "" (deref (second (debugmessage name))))
                                   (str (deref (second (debugmessage name))) "\n"))
                                 "")
                               ((first (debugmessage name)))) #"§" "\n"))

;HNAV implementation
(defn mouse-pressed [graphs debugmessage state event]                           ;also HGUI click check here
  (when (not= [] graphs)
    (doseq [[V E w h] @graphs]
     (doseq [v V]
       (let [px (:px v)
             py (:py v)
             mousex (mouse-to-world-coord-x state (:x event))
             mousey (mouse-to-world-coord-y state (:y event))]
         (when (and (> mousex px) (> mousey py)
                    (< mousex (+ px w)) (< mousey (+ py h)))
           (when (not (= (:onclick v) nil))
             ((:onclick v) state))
           (when (not= nil (:name v))
             (spit-clipboard (str (:name v))))
           (when (contains? debugmessage (:name v))
             (let [debugentry ((:name v) debugmessage)]
               (when (not= nil debugentry)
                 (spit-clipboard (str (slurp-clipboard) (display-string debugmessage (:name v))))
                 (when (> (count debugentry) 1)
                   (reset! input-string (second debugentry)))))))))))
  (assoc state :savepx (:x event) :savepy (:y event) :md true))

(defn mouse-dragged [state event]
  (-> state
      (assoc :difx (+ (:difx state) (- (:x event) (:savepx state)))
             :dify (+ (:dify state) (- (:y event) (:savepy state)))
             :savepx (:x event)
             :savepy (:y event))))

(def scrollcamspeed 1.1)
(defn mouse-wheel [state mouse-scroll]
  (let [zoom-before (:zoom state)
        state2 (if (> mouse-scroll 0)
                 (assoc state :zoom (/ (:zoom state) scrollcamspeed))
                 (assoc state :zoom (* (:zoom state) scrollcamspeed)))]
    (-> state2
        (assoc :difx (* (:difx state2) (/ (:zoom state2) zoom-before)))
        (assoc :dify (* (:dify state2) (/ (:zoom state2) zoom-before))))))

(def states {:difx   (- (/ (width) 2))
             :dify   (- (/ (height) 2))
             :savepx 0.0
             :savepy 0.0
             :zoom   1.0})
