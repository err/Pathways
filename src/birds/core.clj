(ns birds.core
  (:require [birds.songs :as song]
            [overtone.live :as tone]
            [pathways.socks :as ui-tool]
  	    [clojure.math.combinatorics :as combo]
            [quil.core :as q]
            [quil.middleware :as mw]
            [quil.middlewares.bind-output :refer [bind-output]])
  (:import (pathways DynaFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare anchor wander collision-avoidance velocity-matching cursor-force wind-force age)

(def SLIDERS                 (ref {}))
(def GUI                     (DynaFrame. "PARAMETERS"))
(def WIDTH                   (ref 800))
(def HEIGHT                  (ref 800))

(defmacro defslider
  [ref val label]
  `(let [s# (ui-tool/slider #(dosync (ref-set ~ref %))
                            (range 0 (* 2 ~val) (if (integer? ~val) 1 0.001))
                            ~label
                            ~val)]
     (dosync (alter SLIDERS assoc (count @SLIDERS) s#))))

(defmacro defparam
  [name val]
  `(do
     (def ~name (ref ~val))
     (defslider ~name ~val (name '~name))))

(defmacro defparams
  [binds]
  `(do
     ~@(map (fn [[k v]]
	      `(defparam ~k ~v))
	    binds)))

;; user-adjustable params
(defparams [[*TUTOR-POP*      3]	; ? ;
	    [*STUDENT-POP*    3]	; ? ;

	    [*SONG-BANK-SIZE* 3]        ;initial song-bank
	    ;; [*SONG-LENGTH*   9]

	    [*STEPS-PER-SEASON* 300]	;steps
	    [*MAX-AGE-SEASONS*   30]	;seasons
	    [*ADULTHOOD*          4]	;seasons
	    [*SONG-RADIUS*      400]
	    [*SONG-DUR-STEPS*   120]
	    [*RANDOM-SONG-PROB* 0.005]

	    [*VM-RADIUS*    80]
	    [*CA-RADIUS*    40]

	    [*VM-WEIGHT*   1.0]		;velocity-matching
	    [*CA-WEIGHT*   1.0]		;collision-avoidance
	    [*WA-WEIGHT*   1.0]		;wandering
	    [*ANCHOR-WEIGHT* 3.0]
	    [*CURSOR-WEIGHT* 6.0]

	    [*WIND-WEIGHT*   1.0]
	    [*WIND-DIRECTION* Math/PI]
	    [*WIND-VARATION* 0.2]
	    [*ROAM-FACTOR* 2.0]         ;boys like to roam


	    [*MAX-SPEED*  0.03]
	    ;; [*MAX-ACCEL*  0.04]

	    ;; [*MAX-ENERGY*  200] ;reproduce
	    ;; [*LOW-ENERGY*   40] ;settle
	    ;; [*MIN-ENERGY*    0] ;die

	    [*DT*         0.05]])


;; (defmacro defrc [name func])

(def *forces*
     (ref {:wander {:active true}
	   :avoid  {:active true}
	   :match  {:active true}
	   :settle {:active true}
	   :cursor {:active true}
	   :winds  {:active true}}))

(def phrases [[1 1] [2 3] [3 5 7] [1 5 1]])

(def mute?    (volatile! false))

;; internal vars
(def mouse-pos (volatile! [0 0]))
(def mouse-dn? (volatile! false))

(def attract?  (volatile! true))

(def running?  (volatile! false))
(def time      (volatile! 0))
(def font      (volatile! nil))

(def seasons [:spring :summer :fall :winter])
(def current-season (volatile! (nth seasons @time)))

(def flock     (ref (sorted-map)))
(def vault     (ref (sorted-map))) ;;; Where birds go after they die
(def counter   (volatile! 0))



;; visual experiments
(def visuals    (ref {:birds       (sorted-map)
			;; :cursors     (sorted-map)
			;; :tangibles   (sorted-map)
			;; :sound-cloud (sorted-map)
			;; :backgroud   (sorted-map)
			}))


(defn reset-id-counter!
  ([]
     (reset-id-counter! 0))
  ([id]
     (vreset! counter id)))

(defn new-id! []
  (let [id @counter]
    (vswap! counter inc)
    id))

(defn season
  ([]
     (season @time))
  ([step]
     (let [steps-per-season @*STEPS-PER-SEASON*
	   nth-season (nth seasons
			   (mod (/ step steps-per-season)
				(count seasons)))]
       [(Math/floor (/ step steps-per-season)) nth-season])))

;; Utils
(defn half [x] (/ x 2))

(defn average-age
  "Returns the age in seasons?"
  [flock]
  (/ (transduce (map age) + flock)
     (count flock)))

(defn repertoire
  "Retrieves a bird's song repertoire. Also
   searches the vault if you provide a flag"
  ([id]
   (repertoire id nil))
  ([id & [search-vault?]]
   (let [flock           @flock
         {:as   bird
          :keys [songs]} (get flock id (when search-vault?
                                         (get vault id)))]
     (keys songs))))

;; (defn song-patterns []
;;   (-> (vals flock) vals ))

(defn bird [id]
  (get @flock id))

(defn screen
  ([p]
     (screen p @WIDTH @HEIGHT))
  ([p w h]
     [(* (first p) w) (* (second p) h)]))

(defn lesser-of [x y]
  (or (and (< x y) x) y))

(defn greater-of [x y]
  (or (and (> x y) x) y))

(defn vec-add [v1 v2]
  (let [[x y] v1
	[p q] v2]
    [(+ x p) (+ y q)]))

(defn vec-sub [v1 v2]
  (let [[x y] v1
	[p q] v2]
    [(- x p) (- y q)]))

(defn in-circle? [x y h k r]
  (> (q/sq r)
     (+ (q/sq (- x h))
	(q/sq (- y k)))))

;; Queue
(defn make-queue [] clojure.lang.PersistentQueue/EMPTY)

(defmethod print-method clojure.lang.PersistentQueue [q, w]
	   (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(def q* (ref (make-queue)))

(defn q [] @q*)

(defn push-q [item & items]
  (dosync (ref-set q* (apply conj @q* item items))))

(defn pop-q []
  (let [head (peek @q*)]
    (dosync (alter q* pop))
    head))

(defn reset-q []
  (dosync (ref-set q* (make-queue))))

(defn clear-q [] (reset-q))

(defn send-q [msg & msgs] (apply push-q msg msgs))

(def  *q-alive?* (volatile! true))

(defn start-audio-loop []
  (doto
      (Thread.
       #(try
	  (while @*q-alive?*
	    (if-let [snd (pop-q)]
	      (song/tweet snd))
	    (Thread/sleep 1000))
	  (catch Exception e
	    (.printStackTrace e))))
    .start))

(defn stop-audio-loop [] (vreset! *q-alive?* false))

(defn init-audio []
  (try
    (clear-q)
    ;; (start-audio-loop)
    (catch Exception e
      (println "some bullshit with starting the audio")
      (.printStackTrace e))))

;; Neighbors
(let [width @WIDTH
      height @HEIGHT
      diagonal (q/sqrt (+ (q/sq width) (q/sq height)))]

  (defn- dist*
    "returns the displacement vector between two points on a toroidal surface"
    [p q w h]
    (let [[px py] p
	  [qx qy] q
	  dx (- qx px) adx (q/abs dx)
	  dy (- qy py) ady (q/abs dy)
	  dx (if (< adx (dec (- w adx))) dx (- w adx))
	  dy (if (< ady (dec (- h ady))) dy (- h ady))]
      [dx dy]))

  (defn distance-squared [p q]
    (let [[x y] (dist* p q width height)]
      (+ (q/sq x) (q/sq y))))

  (defn sq-dst [p q] (distance-squared p q))

  (defn distance
    "returns the scalar distance between p and q"
    [p q]
    (let [[x y] (dist* p q width height)]
      (q/sqrt (+ (q/sq x) (q/sq y)))))

  (defn displacement
    "returns the displacement vector from p to q"
    [p q]
    (dist* p q width height))

  (defn- in-range? [pos1 pos2 radius]
    (< (distance-squared pos1 pos2) (q/sq radius)))

  (defn- neighbors*
    "Assumes the world is toroidal"
    [boid flock radius]
    (filter #(and (not= (:id boid) (:id %))
		  (in-range? (screen (:pos boid)) (screen (:pos %)) radius))
	    flock))

  (defn neighbors
    "Returns a seq of boids within radius from pos."
    [boid radius]
    (neighbors* boid (vals @flock) radius)))

(defn age [b]
  (let [now @time]
    ((:age-fn b) now)))

(defn bird-age
  "returns the age in seasons"
  [now]
  (let [ratio @*STEPS-PER-SEASON*]
    (fn [then]
      (float (/ (- then now) ratio)))))

(defn student? [b] (and b (-> b age (< @*ADULTHOOD*))))

(defn tutor?   [b] (and b (-> b student? not)))

(defn dead?
  "Too old."
  [b]
  (let [now @time
	max-age @*MAX-AGE-SEASONS*]
    (or (nil? b)
	(< max-age ((:age-fn b) now)))))

(defn singing? [b] (and b (-> b :singing?)))

(defn bird-force-map []
  {:wander {:f (fn [_] (wander))   :w *WA-WEIGHT*}
   :avoid  {:f collision-avoidance :w *CA-WEIGHT*}
   :match  {:f velocity-matching   :w *VM-WEIGHT*}
   :settle {:f anchor              :w *ANCHOR-WEIGHT*}
   :cursor {:f (fn [b] (cursor-force b mouse-pos))
	    :w *CURSOR-WEIGHT*}
   :winds  {:f wind-force          :w *WIND-WEIGHT*}})


;; bird's song-entries contain info regarding tutor, play-count, age, etc.
(defn song-entry [song-type & opt]
  (let [now @time
	{:keys [notes tutor]} (first opt)]
    {song-type {:notes (sorted-set (or notes song-type))
		:count 1
		:tutors {(or tutor :self) {:count 1 :since now}}}}))

(defn song
  [song-type notes start pos singer-id]
  {:song-type   song-type ;
   :notes       (reduce conj (sorted-set) [(song/random-song)]) ;eventually allows for variation. notes := [[1 2] [1 3]]
   :start       start
   :pos         pos
   :singer      singer-id})


;; Setup
(defn make-random-bird
  ([]
     (make-random-bird (new-id!)))
  ([id]
     (let [pos (wander)
	   vel (wander)
	   stp @*STEPS-PER-SEASON*
	   now @time]
       {:id        id
	:pos       pos
	:vel       vel
	:frc       (bird-force-map)
	:age-fn    (bird-age now)
	:singing?  nil
		   ;; (if (< (rand) 0.5)                         ;; uncomment for rapid testing
		   ;;   (let [pattern (random-song)]
		   ;;     (song pattern pattern now pos id))
		   ;;   nil)
	:home      [-1 -1]
	:songs     (into (sorted-map)                        ;;TODO - go without
			 (map #(song-entry % {:tutor id})
			      (song/random-song-bank @*SONG-BANK-SIZE*)))})))

(defn make-bird
  ([]
     (make-random-bird (new-id!)))
  ([opt]
   (let [now                 @time
         season-length       @*STEPS-PER-SEASON*
         {:keys [id pos vel frc songs
                 home  age-fn singing?
                 bank-size]} opt
         id                  (or id  (new-id!))
         pos                 (or pos (wander))
         vel                 (or vel (wander))
         frc                 (or frc (bird-force-map))
         age-fn              (or age-fn (bird-age now))
         singing?            (or singing? ;; (if (< (rand) 0.5)
				 ;;   (let [pattern (song/random-song)]
				 ;;     (song pattern pattern now pos id))
				 ;;   nil)
                                 )
         home                (or home [-1 -1])
         songs               (or songs (into (sorted-map)
                                             (map #(song-entry % {:tutor id})
                                                  (song/random-song-bank (or bank-size @*SONG-BANK-SIZE*)))))]
     {:id       id
      :pos      pos
      :vel      vel
      :frc      frc
      :age-fn   age-fn
      :singing? singing?
      :home     home
      :songs    songs})))


(defn learn-song [bird song tutors]
  (let [now @time
	num-times (count tutors)
	{:keys [song-type notes]} song
	;; _ (println "bird: " (:id bird) "learned song: " song "from tutor[s]: " tutors)
	]
    (-> bird
	(update-in [:songs song-type]
		   (fn [e]
		     (let [old-notes (apply sorted-set (:notes e))
			   add-notes notes
			   new-notes (into old-notes add-notes)
			   num-count (if e (+ num-times (:count e)) num-times)]
		       {:notes  new-notes
			:count  num-count
			:tutors (zipmap (distinct (concat tutors (keys (:tutors e))))
					(map (fn [k]
					       (let [v (get (:tutors e) k)]
						 {:count (if (nil? (:count v))
							   num-times
							   (+ (:count v) num-times))
						  :since  (if (nil? (:since v))
							    now
							    (:since v))}))
					     (distinct (concat tutors (keys (:tutors e))))))}))))))


(defn init-flock
  ([] (init-flock @*TUTOR-POP*))
  ([size]
   (let [ids (repeatedly size new-id!)
         flk (apply sorted-map
                    (mapcat (fn [id]
                              [id (make-bird {:id id :bank-size 2})])
                            ids))]
     (dosync (ref-set flock flk)))))

(defn display-parameters []
  (.display GUI (apply ui-tool/stack (concat [(ui-tool/label "Params ")]
                                               (vals @SLIDERS)
                                               [(ui-tool/label "Press SPACE to [un]PAUSE")]))))

(defn init-sim
  []
  (try
    (song/make-song-bank phrases)
    (reset-id-counter!)
    (init-flock)
    (display-parameters)
    (catch Exception e
      (.printStackTrace e))))


;; FORCES
(defn anchor
  "This force draws a bird to a specific point."
  ([b]
     (anchor b (:home b)))
  ([b pos]
     (let [force (vec-sub (screen pos) (screen (:pos b)))
	   [dx dy] (if (student? b) [0 0] force)]
       [dx dy])))

(defn cursor-force
  "requires a reference to the cursor"
  [b crsr]
  (let [pos @crsr
	[dx dy] (vec-sub pos (screen (:pos b)))]  ; pos is already in screen a cursor
    (if @mouse-dn?  ;GLOBAL
      (if @attract? ;GLOBAL
	[dx dy]
	[(- dx) (- dy)])
      [0 0])))

(defn wind-force
  [b]
  (let [dir @*WIND-DIRECTION*
	roam (if (student? b) @*ROAM-FACTOR* 1)] ;TODO - refactor this and (anchor)
    [(* roam (q/cos dir)) (* roam (q/sin dir))]))

(defn velocity-matching [b]
  (let [pos (:pos b)
	[vx vy] (:vel b)
	epsilon 0.03
	radius @*VM-RADIUS*
	neighbs (neighbors b radius)]
    (if (zero? (count neighbs))
      [0 0]
      (let [weights (map #(/ 1 (+ epsilon (distance (screen pos) (screen (:pos %)))))
			 neighbs)
	    sum-wts (reduce + weights)
	    [ax ay] (reduce (fn [[sum-x sum-y]
				 [weight [dx dy]]]
			      [(+ sum-x (* weight dx)) (+ sum-y (* weight dy))])
			    [0 0]
			    (map (fn [wt nb]
				   [wt (vec-sub (:vel b) (:vel nb))])
				 weights
				 neighbs))]
	[(/ (- ax vx) sum-wts) (/ (- ay vy) sum-wts)]))))

(defn collision-avoidance [b]
  (let [pos (:pos b)
	epsilon 0.03
	radius @*CA-RADIUS*
	neighbs (neighbors b radius)]
    (if (zero? (count neighbs))
      [0 0]
      (let [weights (map #(/ 1 (+ epsilon (distance (screen pos) (screen (:pos %))))) neighbs)
	    sum-wts (reduce + weights)
	    net-frc (reduce (fn [[sum-x sum-y]
				 [weight [dx dy]]]
			      [(+ sum-x (* weight dx)) (+ sum-y (* weight dy))])
			    [0 0]
			    (map (fn [wt nb]
				   [wt (vec-sub (screen (:pos b)) (screen (:pos nb)))])
				 weights
				 neighbs))]
	[(/ (first net-frc) sum-wts) (/ (second net-frc) sum-wts)]))))


(defn wander
  ([] (wander 1))
  ([max-amt]
     (first (drop-while #(< max-amt (+ (q/sq (first %))
				       (q/sq (second %))))
			(repeatedly #(vector (dec (rand 2))
					     (dec (rand 2))))))))

(defn sum-forces
  "forces => [[f0 w0] .. [fN wN]]"
  [body forces]
  (let [ws (reduce + (map second forces))
	ws (if (zero? ws) 1 ws)
	[xs ys] (reduce (fn [accum [f w]]
			  (let [[ax ay] (f body)]
			    (vec-add accum [(* w ax) (* w ay)]))) ;;TODO: factor out (deRef w)
			[0 0]
			forces)]
    [(/ xs ws) (/ ys ws)]))

(defn f-next [b]
  (let [forces   (:frc b)
	removals (filter #(not (:active (get @*forces* (key %)))) forces)
	forces   (vals (apply dissoc forces (map key removals)))
	weights  (map (comp deref :w) forces)
	forces   (map :f forces)]
    (sum-forces b (map vector forces weights))))

(defn v-next [b accel dt]
  (let [[ax ay] accel
	[vx vy] (:vel b)]
    (map #(cond (< % (- @*MAX-SPEED*)) (- @*MAX-SPEED*)
		(> % (+ @*MAX-SPEED*)) (+ @*MAX-SPEED*)
		:else %)
	 [(+ vx (* ax dt)) (+ vy (* ay dt))])))

(defn p-next [b vel dt]
  (let [[vx vy] vel
	[px py] (:pos b)]
    [(mod (+ px (* vx dt)) 1)
     (mod (+ py (* vy dt)) 1)]))


(defn find-match [bank song]
  (get bank (:song-type song) nil))

(def *player* (agent nil))
;; STEP
(defn update-bird [b]
  ;; (println "T = " @time "update-bird: " (:id b) ;; " - " b)
  (let [ret (let [
		  dt @*DT*
		  now @time
		  radius @*SONG-RADIUS*
		  next-acc (f-next b)
		  next-vel (v-next b next-acc dt)
		  [bx by :as next-pos] (p-next b next-vel dt)
		  flock (vals @flock)
		  flock (remove #{b} flock) ;can't be your own tutor! ... :(
		  current-song (:singing? b)


		  ;; FUGLY
		  new-b {:pos next-pos :vel next-vel}
		  new-b (into new-b (if (and current-song
					     (< @*SONG-DUR-STEPS*
						(- now (:start current-song))))
				      {:singing? nil}	;clears song
				      {}))              ;does nothing
		  crooners   (filter :singing? flock)
		  ;; _          (if (not (empty? crooners))
		  ;; 	       (println "\n[update-bird]\n_\nCROONERS: " crooners))
		  tunes      (filter (fn [{:keys [start pos]}]
				       (let [[h k] (screen pos)
					     [x y] (screen (:pos b))] ;NOTICE:  using the old position
					 (in-circle? x y h k radius)))
				     (map :singing? crooners))

		  ;; _          (if (not (empty? tunes))
		  ;; 	       (println "\n__["now"]__\nTUNEAGE:" tunes))
		  near-tune? (first (sort (fn [a b]
					    (let [[xa ya] (screen (:pos a))
						  [xb yb] (screen (:pos b))]
					      (< (+ (q/sq (- xa bx))             ;PAY ATTENTION!  [px py]
						    (q/sq (- ya by)))
						 (+ (q/sq (- xb bx))
						    (q/sq (- yb by))))))
					  tunes)) ;;TODO -> prioritize by distance / familiarity, not start-time
		  new-b      (into b new-b)]



	      ;; (if near-tune?
	      ;; 	(println "bird: " (:id new-b) "hears songs by " (map :singer tunes)
	      ;; 		 "\nchooses to reply to " near-tune?))

	      (if near-tune?
		(cond (student? new-b)
		      (learn-song new-b near-tune? [(:singer near-tune?)])

		      :tutor
		      (if-let [response (find-match (:songs new-b) near-tune?)] ;;poke around?
			(if (not (:singing? new-b))
			  (do
			    (when (not @mute?)

			      (println "bird: " (:id b) " responding to " (:singer near-tune?)
				       " heard type: "   (:song-type near-tune?)
				       " playing: " (:notes response))

			      (send *player* (fn [s args] (song/tweet args)) (first (:notes response)))

			      ;; (send-q (:notes response)) ;queue
			      ;; (send (agent nil)          ;anon-agent
			      ;; 	    (fn [_] (tweet (first (:notes response)))))
			      )
			    (into new-b {:singing? (song (:song-type response)
							 (:notes response)
							 now (:pos near-tune?)
							 (:id new-b))}))
			  new-b) ;; singing

			(do ;; (println "NO RESPONSE FOUND by " (:id new-b) "for tune: " near-tune?)
			    new-b))) ;; no response found

		;;doesn't hear tune
		(do
		  (if (and (not (student? b)) (not (:singing? b)) ;can't already be singing
			   (< (rand) @*RANDOM-SONG-PROB*)) ;sing w/ some probability
		    (let [song-type  (rand-nth (keys (:songs new-b)))
			  song-notes (first (get-in new-b [:songs song-type :notes] nil))
			  song-notes (or song-notes [- - - -])]
		      (when (not @mute?)
			(send *player* (fn [s args] (song/tweet args)) song-notes)
			;; (send-q song-notes)  ;queue
			;; (send (agent nil) (fn [_] (tweet song-notes))) ;anon-agent
			)
		      ;; (println "bird: " (:id new-b) "uses random SING!")
		      (into new-b {:singing? (song song-type song-notes
						   now (:pos new-b) (:id new-b))}))
		    new-b))))]
    (when (nil? ret)
      (println "\n\n______\nDORK RETURNED NIL!")
      (println "retval: " ret
	       "time: " @time
	        "bird: " b ;; (count (:songs b)) "\nflock: " (count (remove nil? (vals flock)))
	       ))
    ret))


(defn remove-oldtimers
  [flock]
  (let [removals (remove #{:live}
			 (map (fn [[k b]]
				(if (or ;; (nil? b)
				     (dead? b)) k :live))
			      flock))
	dead-birds   (reduce (fn [m k] (into m {k (get flock k)})) {} removals)
	pruned-flock (apply dissoc flock removals)
	sorted-flock (reduce conj (sorted-map) pruned-flock)]
    (dosync (alter vault into dead-birds))
    sorted-flock))

(defn update-birds
  [flock]
  (apply sorted-map
	 (mapcat (fn [[k b]] [k (update-bird b)])
		 (seq flock))))

(defn add-hatchling []
  (let [chick (make-random-bird)]
    (println "BABY!")
    (dosync (alter flock assoc (:id chick) chick))))

(defn add-hatchlings
  [flock]
  (let [now @time
	[n season] (season now)
	med-pop    (+ @*TUTOR-POP* @*STUDENT-POP*)
	cur-pop    (count flock)
	diff       (max (- med-pop cur-pop) 1)
	odds (case season
		   :spring 0.7
		   :summer 0.6
		   :fall   0.4
		   :winter 0.3)
	num-bs (if (or (< n 5) (< odds (rand))) 0 diff)	;no hatchlings until we have a mature tutor population
	chicks (mapcat (fn [id] {id (make-random-bird id)})
		       (repeatedly num-bs new-id!))]
    (println "Lots of babies : " (keys chicks) ;; (count chicks)
	     "!")
    (reduce conj flock chicks)))


(defn remove-bird
  ([]
     (remove-bird (rand-nth (keys @flock)) []))
  ([id]
     (remove-bird id nil))
  ([id & ids]
     (let [flock @flock
	   rems (apply concat [id] ids)
	   save (reduce (fn [m k]
			  (into m {k (get flock k)})) {} rems)]

       (dosync (alter vault into save)
	       (alter flock (partial apply dissoc) rems)))))

(defn clear []
  (dosync (alter vault merge @flock)
	  (ref-set flock (sorted-map))))


(defn update-environment []
  (let [mag @*WIND-VARATION*
	amt (- (rand mag) (half mag))]         ;; TODO - fugly
    (dosync (alter *WIND-DIRECTION* #(+ % amt)))))

(defn update-flock
  ;; The colllision-aviodance and vel-matching functions
  ;; that are called within the update-birds function
  ;; reference global state. that explains some of the mess.
  ([]
     (update-flock flock))
  ([f]
     (let [now @time
	   steps (/ @*STEPS-PER-SEASON* 2)]
       (dosync (alter f remove-oldtimers)
	       (alter f update-birds)
	       (when (zero? (mod now steps)) ;; two breeding periods per season
		 (alter f add-hatchlings))))))

(defn update-time []
  (let [sps @*STEPS-PER-SEASON*
	now @time
	next (inc now)]
    (when (zero? (mod next sps))
      (vswap! current-season (second (season next))))
    (vswap! time inc)))

(defn sim-step []
  (update-environment)
  (update-flock)
  (update-time))

;; draw
(defn draw-background []
  ;; (let [season (season)])
  (q/background-float 200 200 255))

(let [angls (apply concat (repeat 3 (range 0 q/TWO-PI (/ q/PI 12))))
      trans (concat (range 5 15 1) (range 15 5 -1))]

  (defn draw-bird [b]
    (let [[vx vy :as vel] (:vel b)
	  [px py :as pos] (:pos b)
	  radius          (if (student? b) 4 8)
	  snd-radius      @*SONG-RADIUS*
	  [mx my]         @mouse-pos
	  flock           (vals @flock)]

      (q/with-translation (screen pos)
	;; Sound-Cloud
	(when (singing? b)
	  (q/fill 215 100 200 130)
	  (q/stroke 40 210 220 255)
	  (q/ellipse-mode :radius)
	  (q/ellipse 0 0 snd-radius snd-radius))

	;; Bird
	;; (color-mode HSB 255) ;drrool
	(if (singing? b)
	  (q/fill 230 90 210 180)
	  (q/fill 30 220 210 81))
	(q/stroke 40 210 220 255)		;colors
	(q/ellipse-mode :radius)		;<-- this was CENTER
	(q/ellipse 0 0 radius radius)

	(let [ang (nth angls (mod (* (:id b) @time) 32))
	      t   (nth trans (mod (* (:id b) @time) 20))]
	  (q/with-rotation [ang]
	    (q/with-translation [(- t) t]
	      (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	    (q/with-translation [ t t]
	      (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	    (q/with-translation [ (- t) (- t)]
	      (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	    (q/with-translation [ t (- t)]
	      (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	    (q/stroke 40 210 220 155)
	    (q/line 0 0 (- t)    t)
	    (q/line 0 0    t     t)
	    (q/line 0 0 (- t) (- t))
	    (q/line 0 0    t  (- t))))

	;; Plain Jane
	;; (q/with-rotation [] ;; [(+ HALF_PI (Math/atan2 (- vy) vx))]
	;;   (q/with-translation [-15 15]
	;;     (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	;;   (q/with-translation [ 15 15]
	;;     (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	;;   (q/with-translation [ -15 -15]
	;;     (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	;;   (q/with-translation [ 15 -15]
	;;     (q/ellipse 0 0 (/ radius 2) (/ radius 2)))
	;;   (q/stroke 40 210 220 155)
	;;   (q/line 0 0 -15  15)
	;;   (q/line 0 0  15  15)
	;;   (q/line 0 0 -15 -15)
	;;   (q/line 0 0  15 -15))

	;; Show Bird-ID, Current Song, Age, Learning History etc. on Mouse-Over
	)


      ;;TODO - get rid of this
      (q/with-translation (screen pos)
	(q/text-align :left)
	(q/text-font @font 12)
	(q/fill 80 210 225 205)
	(q/text (str "id: " (:id b)
                     (if (student? b) "[S]" "[T]")) 15 -20))


      ;; MOUSE-OVER
      (when (> 80 (sq-dst [mx my] (screen pos)))
	(let [{:keys [id songs singing?]} b ;;watch this b like a hawk bro
	      tunes                       (and singing? (:song-type singing?))
	      age                         (format "%.1f" (age b))
	      repertoire                  (keys songs)
	      num-songs                   (count repertoire)
	      repertoire                  (apply str (interpose "\n" (keys songs)))]
	  (q/with-translation (screen pos)
	    (q/text-align :left)
	    (q/text-font @font 12)
	    (q/fill 80 210 225 205)
	    (q/text (str "\n";; "id: " id
                                 "\n""age:" age
                                 "\n""tunes:" num-songs
                                 "\n_\n" (or tunes repertoire)) 15 -20)))




	(let [crooners     (remove #{b} (filter :singing? flock))
	      tunes        (and crooners                                               ;;reary?
                                (filter (fn [{:keys [start pos]}]
                                          (let [[h k] (screen pos)
                                                [x y] (screen (:pos b))]
                                            (in-circle? x y h k radius)))
                                        (map :singing? crooners)))
	      nearest-tune (and tunes                                                ;;reary?
				(first (sort (fn [a b]
					       (let [[xa ya] (screen (:pos a))
						     [xb yb] (screen (:pos b))]
						 (< (+ (q/sq (- xa px))
						       (q/sq (- ya py)))
						    (+ (q/sq (- xb px))
						       (q/sq (- yb py))))))
					     tunes)))]

	  ;;on mouse-over, show audible
	  (q/fill 40 90 190 255)
	  (q/stroke 40 90 190 255)
	  (q/stroke-weight 2)
	  (doseq [t tunes]
	    (let [[x y] (:pos t)]
	      (q/ellipse x y 4 4)
	      (q/line px py x y)))
	  (q/ellipse 0 0 3 3))))))

(def *draw-background?* (volatile! true))
(def *draw-clock?*      (volatile! true))

(defn draw-clock []
  (let [now @time
	[n season] (season now)
	st (str (.toUpperCase (name season))
		" : Season #" n "\nSTEP: " now)]
    (q/with-translation [(screen [0.5 0.03])]
      (q/fill 125 55 255 135)
      (q/text-font @font 22)
      (q/text-align :center)
      (q/text st 0 0))))

(defn draw []
  (when @running?
    (when @*draw-background?* (draw-background))
    (when @*draw-clock?* (draw-clock))
    (doseq [[k b] @flock] (draw-bird b))
    (if-let [snd (pop-q)]
      (song/tweet snd))
    (sim-step)))

;; (defn stir []
;;   ;; (-> (vals @flock))
;;   )

;; (defn settle []
;;   ;; (-> (vals @flock))
;;   )

(defn scatter []
  (let [homes (for [i (keys @flock)]
		 [i [(rand) (rand)]])]
    (dosync (ref-set flock (reduce (fn [m [k pos]] (assoc-in m [k :home] pos))
				     @flock
				     homes)))))

(defn pause []
  (do (vswap! running? not)
      (if (= @running? true)
	(println "unpaused.")
	(println "paused."))))

;; input
(defn mouse-moved
  []
  (vreset! mouse-pos [(q/mouse-x) (q/mouse-y)]))

(defn mouse-dragged
  []
  (vreset! mouse-pos [(q/mouse-x) (q/mouse-y)]))

(defn mouse-released
  []
  (vswap!  mouse-dn? not)
  (vreset! mouse-pos [(q/mouse-x) (q/mouse-y)]))

(defn mouse-pressed
  []
  (let [now   @time
	btn   @mouse-dn?
	mx    (q/mouse-x)
        my    (q/mouse-y)
	radii 20 ; BIRD-RADIUS
	flock (vals @flock)
	colls (filter (fn [{:keys [pos]}]
			(let [[h k] (screen pos)]
			  (in-circle? mx my h k radii))) ;collision/mouse-over
		      flock)]

    (vswap!  mouse-dn? not)
    (vreset! mouse-pos [mx my])

    (if colls
      (let [
	    ;; _          (println "\n___\nCOLLISIONS: " colls)
	    nearest (first
                     (sort (fn [a b]
                             (let [[xa ya] (screen (:pos a))
                                   [xb yb] (screen (:pos b))] ;standard fare
                               (< (+ (q/sq (- xa mx))
                                     (q/sq (- ya my)))
                                  (+ (q/sq (- xb mx))
                                     (q/sq (- yb my))))))
                           colls))
	    _       (println "\n___\nNEAREST-COLLISION: " nearest)

	    [nx ny] (:pos nearest)
	    flock   (remove #{nearest} flock)

	    crooners (filter :singing? flock)

	    ;; _          (if (not (empty? crooners))
	    ;; 	     (println "CROONERS: \n" crooners))

	    tunes (if nearest
                    (filter (fn [{:keys [pos]}]
                              (let [[h k] (screen pos)
                                    [x y] (screen [nx ny])]
                                (in-circle?  x y h k @*SONG-RADIUS*)))
                            (map :singing? crooners)))

	    _ (println "\n___\nTUNES: " tunes)

	    nearest-tune (first
			  (sort (fn [a b]
				  (let [[xa ya] (screen (:pos a))
					[xb yb] (screen (:pos b))]
				    (< (+ (q/sq (- xa nx))
					  (q/sq (- ya ny)))
				       (+ (q/sq (- xb nx))
					  (q/sq (- yb ny))))))
				tunes))
	    _            (println "\n___\nNEAREST-TUNE: " nearest-tune)]

	(when nearest
	  (let [{:keys [id pos singing?
			home songs]} nearest]
	    (println "\n_____T=" now "_____\n" "Bird: " id
		     " at pos: " pos "."

		     "\nSong Repertoire: \n" (interpose "\n" (keys songs))
		     )
	    (dosync (alter visuals assoc-in [:birds id :events] {:event :touch :time now}))))

	(when tunes
	  (println "\nBird " (:id nearest) " tunes overheard: " (vec tunes)
		   "\n(:pos b) " (:pos nearest)
		   " positions: "   (map (fn [t] [(:pos t) (:singer t)]) tunes)))))))


(defn key-typed
  []
  (let [char (char (q/raw-key))
        #_
        (.getKeyChar evt)]
    (case char
	  (\b \B)  (vswap! *draw-background?* not)
	  ;; (\c \C)  (clear)  	  ;; all birds to the vault
	  ;; (\r \R)  (radio-silence)
	  ;; (\m \M)  (forced-mutation)
	  ;; (\a \A)  (attraction/repulsion)  attract?
	  ;; (\o \O)  (sort)
	  (\s \S)  (scatter)
	  (\space) (pause)
	  (\1) :nada			;collision avoidance
	  (\2) :nada			;velocity-matching
	  (\3) :nada			;anchor-points
	  (\4) :nada			;wandering
	  (\m \M) (vswap! mute? not)
	  (\= \+) (add-hatchling)
	  (\- \_) (remove-bird)
	  (\t \T) (vswap! *draw-clock?* not)
	  :unrecognized-command)))


;; applet
(defn setup
  "executes once."
  []
  (println ";;;;;;;;;;;;;;;;____________;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;____________;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;____________;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;____________;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|= Musicc =|;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|__________|;;;;;;;;;;;;;;;;")
  (init-sim)
  (draw-background)
  (q/smooth)
  (q/frame-rate 60)
  (vreset! time 0)
  (vreset! running? true)
  (vreset! font (q/load-font "Monaco-48.vlw")))

(q/defsketch birds
  :title "musical birds!"
  :setup setup
  :draw draw
  :size [@WIDTH @HEIGHT]
  :mouse-moved mouse-moved
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged
  :key-typed key-typed
  :middleware [bind-output mw/pause-on-error]
  )

;; Legacy `rosado.processing`
#_(applet/defapplet birds
  :title "musical birds!"
  :setup setup
  :draw draw
  :size [@WIDTH @HEIGHT]
  :mouse-moved mouse-moved
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged
  :key-pressed key-pressed
  ;; :key-released key-released
  )

;; (applet/stop birds)
;; (applet/run birds :interactive)
