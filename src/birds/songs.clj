(ns birds.songs
  (:require [clojure.math.combinatorics :as combo]
            [overtone.live :refer :all]
            ;; [overtone.inst.synth :refer [tb303]]
            ))


;; song
;; {:phrases [[1 3] [5] [3 1]]}

  ;; (map #(octave-note 4 (degree->interval :ionian %)) [1 3 5])
  ;; => (60 64 67)

;; (defn make-tune [phrases pattern]
;;   (let [patterns (distinct pattern)]
;;     (if (< (count (:phrases moo))
;; 	   (count (distinct (:pattern moo))))
;;       ())))



;; Repertoire

;; Comparing
;;symmetry
;;repetition
;;duration


;; Building
(let [bank* (ref (sorted-map))]

  (defn gen-deets [n]
    {:occurs n}) ;more to come

  (defn gen-bank
    "song-bank grows ... quickly"
    [phrases]
    (let [songs (map vec (set (mapcat #(combo/selections % (count %))
                                      (remove empty? (combo/subsets phrases)))))]
      songs))

  (defn make-song-bank [phrases]
    (dosync (alter bank* into
		   (zipmap (gen-bank phrases)
			   (repeat (gen-deets 0))))))

  (defn song-bank   [] @bank*)

  (defn random-song []
    (rand-nth (keys @bank*)))

  (defn random-song-bank [n]
    (let [amt (min (count @bank*) n)
	  bank (loop [acc []]
		 (if (= (count acc) amt)
		   (vec acc)
		   (recur (distinct (conj acc (random-song))))))
	  entries (zipmap bank (repeat (gen-deets 1)))]
      (dosync (alter bank* into entries))
      bank)))

;; (make-bird [id body brain])

;; id    ---> unique key across view, physics, reasoning

;; body
;; - pos
;; - vel

;; brain
;; - song-bank
;; - age
;; - state
;;    [self world]
;;    (if (< (:age self) @*ADULTHOOD*)
;;       ;; handle-baby
;;       ;; else handle-adult)

;; (cond (child? bird)
;;       (if (flying-around)
;; 	(if (hear-music?) (slow-down))
;; 	(if ())
;; 	))


(defn ep
  [[{:keys [synth vol pitch dur data]} & elements] t]
  (let [next-t (+ t (int (* 1000 dur)))]
    (at t
	(synth pitch vol dur))
    (when elements
      (apply-at next-t #'ep elements [next-t]))))

(defn epattern
  "input map
   :duration - seconds allotted for the pattern"
  [{:keys [synth dur vol notes]}]
  (let [notes   (flatten notes)
        note-dur (/ dur (count notes))
        note-xf (map (fn [n]
                       (let [rest? ('#{-} n)
                             vol   (if rest? 0.0 vol)
                             pitch (if rest? 60 (octave-note 4 (degree->interval n :major)))]
                         {:synth synth
                          :vol   vol
                          :pitch pitch
                          :dur   note-dur})))]
    (into [] note-xf notes)))

(defsynth tb303-03
  [note     60
   vol      0.8
   dur      1.0
   wave     1
   cutoff   440
   r        0.9
   attack   0.201
   decay    0.13
   sustain  0.2
   release  0.2
   env      1440
   gate     0]
  (let [freq       (midicps note)
        freqs      [freq (* 1.01 freq)]
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :action FREE)
        fil-env    (env-gen (perc :attack 0.35))
        fil-cutoff (+ cutoff (* env fil-env))
        waves      [(* vol-env (saw freqs))
                    (* vol-env [(pulse (first freqs) 0.5) (lf-tri (second freqs))])]]
    (out 0 (* [vol vol] (rlpf (select wave (apply + waves)) fil-cutoff r)))))

(defn tweet
  ([notes]
   (tweet notes 1.0 1))
  ([notes dur]
   (tweet notes dur 1))
  ([notes dur rpt]
   (ep (into [] cat (repeat rpt (epattern {:synth tb303-03 :dur dur :vol 0.6 :notes notes})))
       (now))
   ;; (Thread/sleep (* 1000 dur))
   ))

;; (epattern {:synth tb303 :dur 0.3 :vol 0.9 :notes [1 4 6 _ 8 9]})


(def romeo '[6 8 7 3 - 3 5 3 6 - 6 5 4 5 - 5 4 3 2 - 3 2 1 2 3])

(comment
  ;; Romeo & Juliet
  (tweet '[6 8 7 3 - 3 5 3 6 - 6 5 4 5 - 5 4 3 2 - 3 2 1 2 3]))

(comment
  ;; Romeo & Juliet
  (tweet '[6 - - - 8 - - - 7 - - - 3
           - - - - 3 - - - 5 - - - 3
           - - - 6  - - - 6
           - - - 5 - - - - 4 - - - 5
           - - - - - 5 - - - 4 - - - 3
           - - -  2 - 3 - - - 2 - - - - - 1
           - - - - 2 - - - - 3]
         30
         2))
