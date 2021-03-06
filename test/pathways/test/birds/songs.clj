(ns birds.songs
  (:use [clojure.contrib.combinatorics]
	[overtone.live]))


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
(let [bank* (ref [])]
  
  (defn gen-bank
    "song-bank grows by 2^n ... careful!"
    [phrases]
    (let [songs (map vec (distinct (mapcat #(selections % (count %))
					   (remove empty? (subsets phrases)))))]
      songs))

  (defn make-song-bank [phrases]
    (dosync (ref-set bank* (gen-bank phrases))))

  (defn random-song-bank [n]
    (let [amt (min (count @bank*) n)]
      (loop [acc []]
	(if (= (count acc) amt)
	  acc
	  (recur (distinct (conj acc (rand-nth @bank*))))))))

  (defn song-bank [] @bank*))



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

 

;; (ep [{:synth tb303 :vol 0.90 :pitch 65 :dur 0.3}
;;      {:synth tb303 :vol 0.90 :pitch 66 :dur 0.3}
;;      {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      {:synth tb303 :vol 0.90 :pitch 69 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;;      ;; {:synth tb303 :vol 0.90 :pitch 68 :dur 0.3}
;; 						] 
;; 						  (now))



(defn ep [[{:keys [synth vol pitch dur data]} & elements] t]
  (let [next-t (+ t (int (* 1000 dur)))]
    (at t
	(synth pitch vol dur))
    (when elements
      (apply-at next-t #'ep elements [next-t]))))

(defn epattern [{:keys [synth dur vol notes]}]
  (vec 
   (map (fn [n]
	  (let [rest? (-> n number? not)
		vol   (if rest? 0.0 9.0)
		pitch (if rest? 60  (octave-note 4 (degree->interval :ionian n)))
		dur   (/ dur (count notes))]
	    {:synth synth
	     :vol vol
	     :pitch pitch
	     :dur dur}))
	notes)))


(defsynth tb303 [note 60
		 vol 0.8
		 dur 1.0
		 wave 1
                 cutoff 440
		 r 0.9
                 attack 0.201
		 decay 0.13
		 sustain 0.2
		 release 0.2
                 env 1440
		 gate 0]
  (let [freq (midicps note)
        freqs [freq (* 1.01 freq)]
        vol-env (env-gen (adsr attack decay sustain release)
                         (line:kr 1 0 (+ attack decay release))
                         :action :free)
        fil-env (env-gen (perc :attack 0.35))
        fil-cutoff (+ cutoff (* env fil-env))
        waves [(* vol-env (saw freqs))
               (* vol-env [(pulse (first freqs) 0.5) (lf-tri (second freqs))])]]
    (out 0 (* [vol vol] (rlpf (select wave (apply + waves)) fil-cutoff r)))))

(defn tweet
  ([notes]
     (tweet notes 1.0 1))
  ([notes dur rpt]
     (ep (flatten (repeat rpt (epattern {:synth tb303 :dur dur :vol 0.8 :notes notes}))) (now))))



;; (epattern {:synth tb303 :dur 0.3 :vol 0.9
;; 	   :notes [1 4 6 _ 8 9]})


(comment
  ;; Romeo & Juliet
  (tweet [6 8 7 3 - 3 5 3 6 - 6 5 4 5 - 5 4 3 2 - 3 2 1 2 3]))

  ;;   (update-in {:a {:notes [1 3 5 6] :tutors {:ezus {:count 100 :since 0}}
  ;; 			   :b {:notes [1 3 7 9] :tutors {:ezus {:count 100 :since 0}}}
  ;; 			   :c {:notes [1 9 7 5] :tutors {:ezus {:count 100 :since 0}}}
  ;; 			   :d {:notes [6 1 6 7] :tutors {:ezus {:count 100 :since 0}}}}

  ;; 			  [track :tutors :tutor :count] ;nesting

  ;; 			  #(if (nil? %) 0 inc) ;update-fn





(map #(-> % flatten tweet)
     [[[3 5 7] [3 5 7] [1 5 1]]
      [[2 3] [2 3] [1 5 1] [3 5 7]]
      [[1 5 1] [3 5 7] [1 5 1] [2 3]]
      [[3 5 7] [3 5 7] [2 3] [1 5 1]]
      [[1 1] [3 5 7] [1 1]]
      [[2 3] [2 3] [1 5 1] [1 1]]
      [[1 5 1] [1 1] [1 5 1] [2 3]]
      [[1 5 1] [1 1] [1 5 1] [1 5 1]]
      [[1 1] [3 5 7] [1 1] [1 1]]
      [[2 3] [1 1] [3 5 7] [1 5 1]]])