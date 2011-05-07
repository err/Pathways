(ns birds.core
  (:use [rosado.processing]
        [rosado.processing.applet]
	[pathways.socks]
	[birds.songs])
  (:require ;; [overtone.live :as tone]
  	    [clojure.contrib.combinatorics :as combo])
  (:import pathways.DynaFrame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declare anchor wander collision-avoidance velocity-matching)

(def *SLIDERS*                  (ref {}))
(def *GUI*     (DynaFrame. "PARAMETERS"))
(def *WIDTH*                   (ref 800))
(def *HEIGHT*                  (ref 800))

(defmacro defslider [ref val label]
  `(let [s# (slider #(dosync (ref-set ~ref %))
		   (range 0 (* 2 ~val) (if (integer? ~val) 1 0.01))
		   ~label
		   ~val)]
     (dosync (alter *SLIDERS* assoc (count @*SLIDERS*) s#))))

(defmacro defparam [name val]
  `(do
     (def ~name (ref ~val))
     (defslider ~name ~val (name '~name))))

(defmacro defparams [binds]
  `(do
     ~@(map (fn [[k v]]
	      `(defparam ~k ~v))
	    binds)))

;; user-adjustable params
(defparams [[*TUTOR-POP*     10]	; ? ;
	    [*STUDENT-POP*   10]	; ? ;
	    
	    ;; [*SONG-MEMORY*   5]
	    ;; [*SONG-LENGTH*   9]
	    
	    [*STEPS-PER-SEASON* 900]	;steps
	    [*MAX-AGE-SEASONS*   14]	;seasons
	    [*ADULTHOOD*          4]	;seasons
	    [*SONG-RADIUS*      100]
	    
	    [*VM-RADIUS*    80] 
	    [*CA-RADIUS*    40] 
	    
	    [*VM-WEIGHT*   1.0]		;velocity-matching
	    [*CA-WEIGHT*   1.0]		;collision-avoidance
	    [*WA-WEIGHT*   1.0]		;wandering
	    [*ANCHOR-WEIGHT* 3.0]
	    
	    [*MAX-SPEED*  0.03]
	    
	    ;; [*MAX-ENERGY*  200] ;reproduce
	    ;; [*LOW-ENERGY*   40] ;settle
	    ;; [*MIN-ENERGY*    0] ;die
	    
	    [*DT*         0.05]])


;; (defmacro defrc [name func])

(def *forces*
     (ref {:wander {:active true}
	   :avoid  {:active true}
	   :match  {:active true}
	   :settle {:active true}}))

;; internal vars
(def *running?* (atom false))
(def *time*         (atom 0))
(def *font*       (atom nil))

(def *SEASONS* [:spring :summer :fall :winter])

(def *current-season* (atom (nth *SEASONS* @*time*)))

(defn season
  ([]
     (season @*time*))
  ([step]
     (let [steps-per-season @*STEPS-PER-SEASON*
	   nth-season (nth *SEASONS*
			   (mod (/ step steps-per-season)
				(count *SEASONS*)))]
       [(Math/floor (/ step steps-per-season)) nth-season])))

(def *flock* (ref (sorted-map)))

;; utils
(defn screen
  ([p]
     (screen p @*WIDTH* @*HEIGHT*))
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


;; neighbors
(let [width @*WIDTH*
      height @*HEIGHT*
      diagonal (sqrt (+ (sq width) (sq height)))]

  (defn- dist*
    "returns the displacement vector between two points on a toroidal surface"
    [p q w h]
    (let [[px py] p
	  [qx qy] q
	  dx (- qx px) adx (abs dx)
	  dy (- qy py) ady (abs dy)
	  dx (if (< adx (dec (- w adx))) dx (- w adx))
	  dy (if (< ady (dec (- h ady))) dy (- h ady))]
      [dx dy]))

  (defn distance-squared [p q]
    (let [[x y] (dist* p q width height)]
      (+ (sq x) (sq y))))

  (defn distance
    "returns the scalar distance between p and q"
    [p q]
    (let [[x y] (dist* p q width height)]
      (sqrt (+ (sq x) (sq y)))))

  (defn displacement
    "returns the displacement vector from p to q"
    [p q]
    (dist* p q width height))

  (defn- in-range? [pos1 pos2 radius]
    (< (distance-squared pos1 pos2) (sq radius)))
  
  (defn- neighbors*
    "Assumes the world is toroidal"
    [boid flock radius] 
    (filter #(and (not= (:id boid) (:id %))
		  (in-range? (screen (:pos boid)) (screen (:pos %)) radius))
	    flock))

  (defn neighbors
    "Returns a seq of boids within radius from pos."
    [boid radius]
    (neighbors* boid (vals @*flock*) radius)))

(defn running? [] @*running?*)

(defn age [b]
  (let [now @*time*]
    ((:age-fn b) now)))

(defn student? [b] (-> b age (< @*ADULTHOOD*)))

(defn tutor? [b] (-> b student? not))

(defn singing? [b] (-> b :singing?))

(defn bird-force-map []
  {:wander {:f (fn [_] (wander))   :w *WA-WEIGHT*}
   :avoid  {:f collision-avoidance :w *CA-WEIGHT*}
   :match  {:f velocity-matching   :w *VM-WEIGHT*}		 
   :settle {:f anchor              :w *ANCHOR-WEIGHT*}
   })

;; setup
(defn make-random-bird [id]
  (let [pos (wander)
	vel (wander)
	;; chi @*MAX-ENERGY*
	stp @*STEPS-PER-SEASON*
	now @*time*]
    {:id        id
     :pos       pos
     :vel       vel
     :frc       (bird-force-map)
     ;; :energy    chi
     :age-fn    #(/ (- % now) stp)
     :settled?  false
     :singing?  false
     :learning? false
     :home      [(rand) (rand)]
     :songs     {:a {:notes #{[1 3 5 6]} :count 1 :tutors {:ezus {:count 1 :since 0}}}
		 :b {:notes #{[1 3 7 9]} :count 1 :tutors {:ezus {:count 1 :since 0}}}
		 :c {:notes #{[1 9 7 5]} :count 1 :tutors {:ezus {:count 1 :since 0}}}
		 :d {:notes #{[6 1 6 7]} :count 1 :tutors {:ezus {:count 1 :since 0}}}}}))


;; (reduce (fn [m [k v]] (assoc m v k)) {} {:a 1 :b 2 :c 3 :d 4})

(defn learn-song [bird song tutors]
  (let [now @*time*
	num-times (count tutors)
	{:keys [type notes]} song]
    (-> bird
	(update-in [:songs type]
		   (fn [e]
		     {:notes  (conj (:notes e) notes)
		      :count  (if e (+ num-times (:count e)) num-times)
		      :tutors (zipmap (distinct (concat tutors (keys (:tutors e))))
				      (map (fn [k]
					     (let [v (k (:tutors e))]
					       ;; (println "count: " (:count v)
					       ;; 		"since: " (:sinve v))
					       {:count (if (nil? (:count v)) num-times (+ (:count v) num-times))
						:since  (if (nil? (:since v)) now (:since v))}))
					   (distinct (concat tutors (keys (:tutors e))))))})))))


(defn init-flock [& size]
  (let [size (or (first size) @*TUTOR-POP*)
	ids (range size)
	flock (apply sorted-map
		     (mapcat (fn [id]
			       [id (make-random-bird id)])
			     ids))]
    (dosync (ref-set *flock* flock))))

(defn display-parameters []
  (.display *GUI* (apply stack (concat [(label "Params ")]
				       (vals @*SLIDERS*)
				       [(label "Press SPACE to [un]PAUSE")]))))

(defn init-sim []
  (init-flock)
  (make-song-bank )
  ;; (display-parameters)
  )


;; forces
(defn anchor
  ([b]
     (anchor b (:home b)))
  ([b pos]
     (let [w @*ANCHOR-WEIGHT*
	   [dx dy] (vec-sub (screen pos) (screen (:pos b)))] ;;TODO-assumes
       [dx dy])))

(defn velocity-matching [b]
  (let [pos (:pos b)
	epsilon 0.03
	radius @*VM-RADIUS*
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
				   [wt (vec-sub (:vel b) (:vel nb))])
				 weights
				 neighbs))]
	[(/ (first net-frc) sum-wts) (/ (second net-frc) sum-wts)]))))

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
  ([]
     (wander 1))
  ([max-amt]
     (first (drop-while #(< max-amt (+ (sq (first %))
				       (sq (second %))))
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

(defn e-next
  "returns the energy of the bird at the next sim-step.
   Lose energy: moving (1:1), living (1:1)
   Gain energy: singing (depends on audience), listening (1 pt for every song it hears at present)."
  [b]
  ;; (let [e-now (:energy b)
  ;; 	standard-loss 1
  ;; 	movement-loss (reduce + (map sq (:vel b)))
  ;; 	teaching-gain (if (:singing? b) (audience b) 0)
  ;; 	;; learning-gain (if (:learning? b) )
  ;; 	]
  ;;   )
  )


;; step
(defn update-bird [b]
  (let [dt @*DT*
	;; now @*time*
	next-acc (f-next b)
	next-vel (v-next b next-acc dt)
	next-pos (p-next b next-vel dt)
	;; energy   (e-next b)
	]
    (assoc b :pos next-pos :vel next-vel ;; :energy (dec energy)
	   )))

(defn update-birds [bs]
  (let [max-age @*MAX-AGE-SEASONS*
	now @*time*]
    (map (fn [[k b]] [k (update-bird b)])
	 (filter (fn [[k b]] (< ((:age-fn b) now) max-age))
		 bs))))

(defn update-flock
  ([]
     (dosync (ref-set  *flock* (into {} (update-birds @*flock*)))))
  ([f]
     (dosync (ref-set *flock* f))))

(defn update-time
  []
  (let [sps @*STEPS-PER-SEASON*
	now @*time*
	next (inc now)]
    (when (zero? (mod next sps))
      (swap! *current-season* (second (season next))))
    (swap! *time* inc)))

(defn sim-step []
  ;; update-pos
  (update-flock)  
  ;; chirp
  (update-time))

;; draw
(defn draw-background []
  (background-float 200 200 255))

(defn draw-bird [b]
  (let [student? (student? b)
	radius   (if student? 15 20)
	singing? (singing? b)]


    (with-translation (screen (:pos b))
      ;; soundcloud
      (when singing?
	(fill 215 100 200 130)
	(ellipse-mode RADIUS)
	(ellipse 0 0 @*SONG-RADIUS* @*SONG-RADIUS*))

      ;; bird
      (if singing?
	(fill 230 90 210 180)
	(fill 30 220 210 81))
      (stroke 40 210 220 255)
      (ellipse-mode CENTER)
      (ellipse 0 0 radius radius)
      (with-translation [-15 15]
	(ellipse 0 0 (/ radius 2) (/ radius 2)))
      (with-translation [ 15 15]
	(ellipse 0 0 (/ radius 2) (/ radius 2)))
      (stroke 40 210 220 155)
      (line 0 0 -15 15)
      (line 0 0  15 15))))

(def *draw-background?* (atom true))

(defn draw []
  (when @*running?*
    (when @*draw-background?* (draw-background))
    (doseq [[k b] @*flock*] (draw-bird b))
    (sim-step)))

;; stop 

;; shutdown


(defn stir []
  ;; (-> (vals @*flock*))
  ) 

(defn settle []
  ;; (-> (vals @*flock*))
  )

(defn scatter []
  (let [homes (for [i (keys @*flock*)]
		 [i [(rand) (rand)]])]
    (dosync (ref-set *flock* (reduce (fn [m [k pos]] (assoc-in m [k :home] pos))
				     @*flock*
				     homes)))))

;; input
(defn key-pressed [evt]
  (let [char (.getKeyChar evt)]
    (case char
	  (\b \B)  (swap! *draw-background?* not)
	  (\s \S)  (scatter)
	  (\space) (do (swap! *running?* not)
		       (if (= @*running?* true)
			 (println "unpaused.")
			 (println "paused.")))
	  :unrecognized-command)))

;; applet
(defn setup
  "executes once."
  []
  (println ";;;;;;;;;;;;;;;;____________;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|= Musicc =|;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|__________|;;;;;;;;;;;;;;;;")
  (init-sim)
  (draw-background)
  (smooth)
  (framerate 60)
  (reset! *time* 0)
  ;; (reset! *running?* true)
  (reset! *font* (load-font "Monaco-48.vlw")))

(defapplet birds
  :title "musical birds!"
  :setup setup
  :draw draw
  :size [@*WIDTH* @*HEIGHT*]
  ;; :mouse-moved mouse-moved
  ;; :mouse-pressed mouse-pressed
  ;; :mouse-released mouse-released
  ;; :mouse-dragged mouse-dragged
  :key-pressed key-pressed
  ;; :key-released key-released
  )


;; (stop birds)
 (run birds :interactive)