(ns pathways.core
  (:use [clojure.contrib.math :only [floor]]
	;; [clojure.contrib.graph]
	[rosado.processing :exclude [cursor]]
	[rosado.processing.applet]
	[pathways.util]
	[pathways.event]
	[pathways.logo]
	;; [pathways.graph]
	)
  (:require [pathways.tuio :as tuio]
	    ;; [overtone.live :as tone]
	    ;; [incanter [core :as incanter] [charts :as charts]]
	    [lamina.core :as comm]
	    )
  (:import (java.awt MouseInfo))
  (:gen-class))

(def *framerate*             60)
(def *time*            (atom 0))
(def *previous-time* (atom  -1))
				
(def *screen-width*  1680)
(def *screen-height* 1050);     825)
(def *bg-color*         51)
(def *acceleration*  [0.1 0.1])
(def *token-mass*    1)
(def *token-radius* 25)
(def *token-font* (atom nil))
(def *debug-font* (atom nil))
(def *mouse-pos* (atom [0 0]))
(def *mouse-button* (atom nil))

(def *max-speed* 5)
;; (def *min-velocity* 0)

(def *cursor-path-length* 20)
(def *cursor-timeout*  10) ;; delay threshold (frames prior to now) after which motion events are disregarded
(def *inner-path-timeout*  10)
(def *total-path-timeout*  10)

(def draw-circle? (atom true))

(def *current-edge* (atom nil))

(def *edge-making-mode* (atom false)) ; there should be one of these for every cursor



(def *tuio-port* 3333)
(def *tuio* (tuio/client *tuio-port*))

(defn mouse-x [] (first @*mouse-pos*))
(defn mouse-y [] (second @*mouse-pos*))



;;; Bresenham's line algorithm
;;; courtesy of Rosetta Code
(defn bresenham-line
  "returns a seq of points forming a line
   from x1,y1 to x2,y2 using Bresenham's line algorithm." 
  [x1 y1 x2 y2]
  (let [dist-x (abs (- x1 x2))
	dist-y (abs (- y1 y2))
	steep (> dist-y dist-x)]
    (let [[x1 y1 x2 y2] (if steep [y1 x1 y2 x2] [x1 y1 x2 y2])]
      (let [[x1 y1 x2 y2] (if (> x1 x2) [x2 y2 x1 y1] [x1 y1 x2 y2])]
	(let  [delta-x (- x2 x1)
	       delta-y (abs (- y1 y2))
	       y-step (if (< y1 y2) 1 -1)]
 
	  (loop [x x1 y y1 error (floor (/ delta-x 2)) res []]
	    (if (< x x2)
	      ;; Rather then rebind error,
	      ;; test that it is less than
	      ;; delta-y rather than zero
	      (if (< error delta-y) 
		(recur (inc x) (+ y y-step) (+ error (- delta-x delta-y)) (conj res [x y]))
		(recur (inc x) y            (- error delta-y) (conj res [x y])))
	      res)))))))


(defn destructive-pop [seq]
  (let [h (first @seq)]
    (swap! seq next)
    h))


;; some random floatiness to the molecules
(let [dx (atom (apply concat (iterate shuffle (shuffle (range -3 3 0.5)))))
      dy (atom (apply concat (iterate shuffle (shuffle (range -3 3 0.5)))))]

  (defn x-drift-peek []
    (first @dx))

  (defn x-drift []
    (destructive-pop dx))

  (defn y-drift-peek []
    (first @dy))

  (defn y-drift []
    (destructive-pop dy)))


;; (defrecord mole   [id name amt]   )
(defrecord node   [id x y r amt held type in out])
(defrecord edge   [id src sink x1 y1 x2 y2 type rate])
(defrecord cursor [id pos vel path])
(defrecord mass   [id mass pos vel path]) ;; forces (list of attached forces)


;; TEST branch
(defrecord mole [id name amt])
(defrecord reac [id name rate])

(defn make-molecule [id name concentration]
  (mole. id name concentration))

(defn make-reaction [id name rate]
  (reac. id name rate))

;; (defrecord force  [id acc time acc-fn]
;;   ;; id     - id of force (is this necessary?)
;;   ;; acc    - [xacc yacc] component vectors
;;   ;; time   - time at which force began
;;   ;; acc-fn - function for updating the acc vectors at each time-step


(def *cursors* (ref {:mouse (cursor. :mouse @*mouse-pos* [0 0] [[(mouse-x) (mouse-y) @*time*]])}))
(def *nodes*   (ref {}))
(def *edges*   (ref {}))

(def *moles*   (ref {}))
(def *reacs*   (ref {}))

(def *masses*  (ref {}))
(def *entity-counter* (atom 0))


;;;; Utils
(defn in-quad?
  "returns true when x and y are within the quad of 
   whose top-left corner is (qx, qy) and dimensions are qh qw"
  [x y qx qy qw qh]
  (and (> x qx) (< x (+ qx qw))
       (> y qy) (< y (+ qy qh))))


(defn in-circle? [x y h k r]
  (> (sq r)
     (+ (sq (- x h))
	(sq (- y k)))))

(defn magnitude [vec]
  (let [[x y] vec]
    (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2)))))

(defn mag [vec] (magnitude vec))

(defn heading [vel]
  (let [[x y] vel]
    (atan2 y x)
    ;; (* -1 (atan2 (* -1 y) x))
    ))
      
(defn div [vec scalar]
  (doall (map #(/ % scalar) vec)))

(defn normalize [vec]
  (let [mag (mag vec)
	scaled (or (and (not= mag 0)
			(not= mag 1)
			(div vec mag))
		   mag)]
    scaled))


(defn approx
  "returns true when (abs (- x y)) is less than 0.01 .
   optional third argument specifies the order of magnitude
   for the comparison threshold."
  ([x y]
     (approx x y -2))
						  
  ([x y p]
     (<= (abs (- x y))
	 (Math/pow 10 (or (and (neg? p) p) (- p))))))


(defn edges []
  (doall (filter #(= (:type %) :edge) (vals @*edges*))))

(defn nodes []
  (doall (filter #(= (:type %) :node) (vals @*nodes*))))

(defn masses [] (vals @*masses*))

(defn cursors [] (vals @*cursors*))

(defn now []
  (System/currentTimeMillis))

;; (defn over-node? [x y coll]
;;   ;; (println "over-node: " x ","  y)
;;   (first (filter
;; 	  #(in-circle? x y h k r)
;; 	  (fn [e]
;; 	    (and e
;; 		 ;; (= (:type e) :node)
;; 		 (in-circle? x y (:x e) (:y e) (:r e))))
;; 	  coll)))

(def *frame-counter* (atom 0)) 
(def *fps* (atom 0.0))


(defn reset-frame-count []
  (let [now (now)
	prev @*previous-time*
	count @*frame-counter*]
    (when (= count 120)
      (reset! *fps* (* 1000.0 (/ count (- now prev))))
      (reset! *previous-time* now)
      (reset! *frame-counter* 0))))


(let [extra 8]
  (defn mass-at
    "hit detection / nearest collision"
    ([pos] (mass-at (first pos) (second pos)))
    ([x y]
    ;; (when @*current-edge*
    ;;   (println "entering node-at: " x ", " y "\n"))

       (let [masses-at-point
	     (filter (fn [m]
		       (let [[h k] (:pos m)]
			 (in-circle? x y h k (+ extra *token-radius*))))
		     (vals @*masses*))]
      
	 (first
	  (sort (fn [a b]
		  (let [[xa ya] (:pos a)
			[xb yb] (:pos b)]
		    (< (+ (sq (- xa x))
			  (sq (- ya y)))
		       (+ (sq (- xb x))
			  (sq (- yb y))))))
		masses-at-point))))))

(defn cursor-above?
  ([id]
     ;; (println "\n_____\ncursor-above: " id)
     (when-let [target-mass (get @*masses* id)]
       (when-let [mass-at-mouse (mass-at @*mouse-pos*)]
	 (= (:id target-mass) (:id mass-at-mouse))))))



(defn dist-fn
  "calculates next position of an object located at [pos] with velocity [vel]."
  [pos vel dt]
  (let [[x y]   pos
	[vx vy] vel
	vx (cond (< vx 0) (or (and (> (abs vx) *max-speed*) (* -1 *max-speed*)) vx)
		 :else    (or (and (> (abs vx) *max-speed*) *max-speed*)       vx))

	vy (cond (< vy 0) (or (and (> (abs vy) *max-speed*) (* -1 *max-speed*)) vy)
		 :else    (or (and (> (abs vy) *max-speed*) *max-speed*)       vy))

	vxn (or vx (and (< (abs vx) 0.5) 0) vx)
	vyn (or vy (and (< (abs vy) 0.5) 0) vy)
	sx-new (+ x (* dt vxn))	;dt == 1 here
	sy-new (+ y (* dt vyn))]	;dt == 1 here
    ;; (println "\n~~~~~~~~~~\ndist:" pos vel [sx-new sy-new] )
    [sx-new sy-new]))


(defn veloc-fn
  "calculates next velocity of an object,
   given initial-velocity, acceleration, dt."
  [vel accel dt]
  (let [[vx vy] vel
	[ax ay] [(or (and (approx (abs vx) 0) 0)
		     (first accel))
		 (or (and (approx (abs vy) 0) 0)
		     (second accel))]
	vx-new   (or (and (pos? vx) (- vx (* ax dt)))
		     (+ vx (* ax dt)))
	vy-new   (or (and (pos? vy) (- vy (* ay dt)))
		     (+ vy (* ay dt)))
	vxn (or (and (< (abs vx-new) 0.1) 0) vx-new)
	vyn (or (and (< (abs vy-new) 0.1) 0) vy-new)]
    ;; [vx-new vy-new]
    [vxn vyn]
    ))



(defn add-to-path
  "point is actually a [x y t] triple
    , where [x y] mark the position of cursor at time t."
  [path pos]
  (if (> *cursor-path-length* (count path))
    (cons pos path)
    (cons pos (butlast path))))


(defn update-mass [id]
  (let [accel *acceleration*
	now @*time*
	mass (get @*masses* id)
	[xo yo] (:pos mass)
	[vxn vyn] (veloc-fn (:vel mass) accel 1.0) ;; (avg-velocity-along-path (:path mass))
	[vxn vyn] [(or (and (approx (abs vxn) 0) 0) vxn)
		   (or (and (approx (abs vyn) 0) 0) vyn)]

 	[xn yn] (dist-fn (:pos mass) [vxn vyn] 1.0)]
    (mass. id (:mass mass)
	   [xn yn] [vxn vyn]
	   (add-to-path (:path mass) [(- xn xo) (- xn yo) now]))))



(defn update []
  (or (reset-frame-count)
      (swap! *frame-counter* inc))

  ;; update cursors
  ;; (dosync
  ;;  (alter *cursors*
  ;; 	  (fn [m]
  ;; 	    (merge m
  ;; 		   (zipmap (keys m)
  ;; 			   (map (fn [{id :id, pos :pos}]
  ;; 				  (update-cursor-pos id pos))
  ;; 				(vals m)))))))

  ;; update masses
  (dosync
   (alter *masses*
	  (fn [m]
	    (merge m
		   (zipmap (keys m)
			   (map (fn [{id :id, pos :pos, vel :vel}]
				  (update-mass id))
				(vals m))))))
  
   ;; update graph-nodes
  
   (alter *nodes*
	  (fn [m]
	    (merge m
		   (zipmap (keys m)
			   (map (fn [node ;; {id :id, r :r, amt :amt, held :held,
				     ;;  in :in, out :out :as node}
				     ]
				  (let [[x y] (:pos (get @*masses* (:id node)))]
				    (assoc node :x x :y y)
				    ;; (node. id x y r amt held :node in out)
				    ))
				(vals m)))))))
  (swap! *time* inc))




(defn velocity [x y last-x last-y dt]
 ;; (println "~~~~~~~~~\n\nvelocity: " x " " y " " last-x " " last-y " in " dt)
  (if (zero? dt)
    [0 0]
    (let [dx-dt (/ (- x last-x) 1.0 ) ;dt
	  dy-dt (/ (- y last-y) 1.0 )] ;dt
      (vector (if (< dx-dt 0.1) 0 dx-dt)
	      (if (< dy-dt 0.1) 0 dy-dt)))))


(defn lesser-of [a b]
  (or (and (< a b) a) b))

(defn greater-of [a b]
  (or (and (< a b) b) a))

(defn- crsr-velocity [cid]
  (let [txt (println ";;;;;;;;;;;;;BEGIN!;;;;;;;;;;;;")
	
	now @*time*
	crsr (get @*cursors* cid)
	path (:path crsr)
	[xn yn] [91 91]
	[xo xo start dx dy]
	(reduce (fn [[xn yn tn xacc yacc] [xo yo to]]
		  (println "\n_________\n\n"
			   xn " . " yn "   ; x/y-now\n" 
			   xacc " . " yacc "  ; x/y-acc ")
		  (if (or (> (abs (- tn  to)) 100 ;; *inner-path-timeout*
			     )			  ; clip
			  (> (abs (- now to)) 100 ;; *total-path-timeout*
			     ))
		    (do
		      (println "burn one! " xo " " yo " " to)
		      [xn yn tn xn yn]) ; disregard events from long ago
		    (do
		      (println 
		       "[now-x,y]:\t"
		       [xn yn]
							    
		       "\n[pre-x,y]:\t"
		       [xo yo]  
							    
		       "\n  [dx,dy]:\t"
		       [ (- xn xo)  (- yn yo)])
		      [xo yo
		       (lesser-of tn to)
		       (+ xacc (- xn xo))
		       (+ yacc (- yn yo))])))
		[xn yn now 0 0]
		path
		;; (cons [xn yn now] path)
		)
	dt (greater-of (- now start) 1.0)
	vel [(/ dx dt) (/ dy dt)]]

    ;;
    (println "x/y-now: " [xn yn] "time-now: " now "start-time: " start "dt: " dt
	     "\ndisplacement: " [dx dy]
	     "\nvelocity: "  vel)
    ;; (velocity xn yn xo yo dt)
    vel))

(defn cursor-velocity
  ([cid]
     (let [now @*time*
	   cursor (get @*cursors* cid)
	   path   (:path cursor)
	   prev-t (last (first path))] ;the time is stored as the third element in a path [x y t]
       (if (> (- now prev-t) *cursor-timeout*)
	 [0 0]
	 (let [[xn yn]  (:pos cursor)
	       [dx dy start ;; xo yo start dx dy
		]
	       (reduce (fn [[xn yn tn ;; xacc yacc
			     ] [xo yo to]]
			 ;; (println "\n_________\n\n"
			 ;; 	      xn " . " yn "   ; x/y-now\n" 
			 ;; 	      xacc " . " yacc "  ; x/y-acc ")
			 (if (or (> (abs (- tn  to)) *inner-path-timeout*)
				 (> (abs (- now to)) *total-path-timeout*))
			   (do
			     ;; (println "burn one! " [xo yo] " " to)
			      [xn yn tn xn yn]) ; disregard events from long ago
			   (do
			     ;; (println 
			     ;;  "[now-x,y]:\t" [xn yn]
			     
			     ;;  "\n[pre-x,y]:\t" [xo yo]  
			     
			     ;;  "\n  [dx,dy]:\t" [ (- xn xo)  (- yn yo)])
			     [(+ xn xo) (+ yn yo)
			      (lesser-of tn to)
			      ;; (+ xacc (- xn xo))
			      ;; (+ yacc (- yn yo))
			      ])))
		       [0 0 now ;; 0 0
			]
		       path)
	       dt (greater-of (- now start) 1.0)
	       vel [(/ dx dt) (/ dy dt)]]

	   ;; (println "cursor velocity-avg x/y-now: " [xn yn] " , time-now: " now " , dt: " dt
	   ;; 	    "\nvelocity vector: [ "   (first vel) " " (second vel) " ]")
	   vel)))))



(defn update-cursor-pos [cid pos]
  (let [csr (get @*cursors* cid)
	[xo yo] (:pos csr)
	[xn yn] pos
	now @*time*] 
    (cursor. cid pos (cursor-velocity cid) (add-to-path (:path csr) [(- xn xo) (- yn yo) now]))))


;; [id mass pos vel path]
(defn drop-token
  "either places a new token on the board
   or drops a currently held token at x y "
  ([cursor-id x y]
     (let [now @*time*
	   id @*entity-counter*
	   crsr (get @*cursors* cursor-id)]
       (dosync
	(alter *nodes*  assoc id (node. id x y *token-radius* 10 true :node {} {}))
	(alter *masses* assoc id (mass. id *token-mass* [x y] (cursor-velocity cursor-id) [[x y now]]))
	(alter *moles*  assoc id (make-molecule id "no-name" 1.0))
	;; (ref-set *current-edge*  (edge. (inc id) id :mouse src-x src-y mx my :edge 1)
											) 
       (swap! *entity-counter* inc)))

  ([cursor-id x y tokes]
     (when-let [toke (first tokes)]
       (let [now @*time*
	     crsr (get @*cursors* cursor-id)]
	(dosync
	 (alter *nodes* assoc
		(:id toke) (node. (:id toke) x y *token-radius* (:amt toke) false :node (:in toke) (:out toke)))
	 (alter *masses* assoc
		(:id toke) (mass. (:id toke) *token-mass* [x y] (cursor-velocity cursor-id) [[x y now]])))))))



(defn draw-debug []
  (let [x (first @*mouse-pos*)
	y (second @*mouse-pos*)
	nearest (mass-at x y);; (first )
	]
   (text-align RIGHT)
    (text-font @*debug-font* 10)
    (fill 255)
    (string->text
     (str ""
	  ;; mouse-position
          "mouse-X: " x
	  "    "
	  "mouse-Y: " y

	  "\n"

	  "FPS: " (format "%.2f" @*fps*)

	  "\n"


	  ;; hit detection
	  "collisions: "
	  (when-let [toke (first (filter
				  (fn [n]
				    (and n (in-circle? x y (:x n) (:y n) (:r n))))
				  (vals @*nodes*)))]
	    (str "id: " (:id toke) " [" (:x toke) " , " (:y toke)"]  ; out: "(:out toke)))
	  "\n"


	  ;; tokes
	  "*nodes*: "
	  (doall
	   (apply str
		  (map #(str "id: " (:id %) " [" (:x %) " , " (:y %)"]" "  held: " (:held %) " vel: " (:vel (get @*masses* (:id %))) "\n")
		       (filter #(= (:type %) :node) (vals @*nodes*)))))
	  "\n"
	  
	  

	  "\n"
	  (when nearest
	    (str "Nearest-mass: " "id: " (:id nearest) " pos: " (:pos nearest)		 
		 "  held: " (:held (get @*nodes* (:id nearest)))))

	  "\n"

	  (str "edge-making-mode:  " @*edge-making-mode*)

	  "\n"
	  (when @*current-edge*
	    (str "current-edge: " (:idx @*current-edge*)
		 "source: " (:src @*current-edge*)
		 " [ " (:x1 @*current-edge*) ", "
		 (:y1 @*current-edge*) " ] "
		 "sink: " (:sink @*current-edge*)
		 " [ " (:x2 @*current-edge*) ", "
		 (:y2 @*current-edge*) " ] "
		 ))

	  "\n"
	  ;; (when (vals @*cursors*)
	  ;;   (apply str
	  ;; 	   (map #(str "cursor: " (:id %) "  pos: " (:pos %) "   calculated vel: "
	  ;; 		      (let [[vx vy] (cursor-velocity (:id %))]
	  ;; 			(format "%.2f %.2f" (float vx) (float vy))))
	  ;; 		(vals @*cursors*))))
	  "\n"
	  
	  )
     (- *screen-width* 8)
     (- *screen-height* 400)))) 



(defn dist-between [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2
	dx (- x2 x1)
	dy (- y2 y1)]
    (sqrt (+ (sq dx) (sq dy)))))



;;;;; DRAWING HYPNOSIS thing
(def *num-leaves* (ref 12))

(defn draw-hypno [x y]
  (let [nleaves @*num-leaves*]
    (with-translation [x y]
      ;; (fill-float 140 141 163 200)
      ;; (ellipse 0 0 65 65)
      (with-rotation [(mod @*time* TWO_PI)]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [ang]
	    (with-translation [0 (/ (+ 50 (abs (- 60 (mod @*time* 120)))) 2)]
	      (fill 20 100 190 100)
	      (no-stroke)
	      (ellipse 0 0 10 10)))))

      (with-rotation [(- (mod @*time* TWO_PI))]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [(- HALF_PI ang)]
	    (with-translation [0 (/ (+ 40 (abs (- 60 (mod @*time* 120)))) 4)]
	      (fill 100 20 190 100)
	      (no-stroke)
	      (ellipse 0 0 6 6)))))

	

      (with-rotation [(mod @*time* TWO_PI)]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [ang]
	    (with-translation [0 (/ (+ 20 (abs (- 60 (mod @*time* 120)))) 8)]
	      (fill 100 190 20 100)
	      (no-stroke)
	      (ellipse 0 0 3 3)))))	
	
      (with-rotation [(/ PI 4)]
	(fill 220 0 90 90)
	(rect-mode CORNER)
	;; (rect  0 0 20 20)

	;; (shape :triangles
	;;      [ 0     30]
	;;      [-15   -30]
	;;      [ 15   -30])
	))))


;;;;; DRAWING EDGES

(let [bezier-midpoints (range 0 1 0.02)]
  (defn draw-edge [entry]                                                             
    (when entry
      ;; (println "entry: " entry)
      (let [edge  entry ;; (val entry)
	    time @*time*
	    entities @*nodes*
	    masses   @*masses*

	    src-node (get entities (:src edge))
	    src-mass (get masses (:src edge))
	    [x y]    (:pos src-mass)
	    
	    ;; draw edge to crsr holding edge 
	    [mx my] @*mouse-pos*
	    src-x (if (:held src-node)
		    mx
		    x ;; (:x src-node)
		    )
	    src-y (if (:held src-node)
		    my
		    y 
		    ;; (:y src-node)
		    )

	    sink-id (:sink edge)
	    sink-mass (get masses sink-id)
	    ;; [sx sy] (:pos sink-mass)
	  
	    sink-x (or (and (or (= sink-id :mouse) (:held (get entities sink-id))) mx)
		       (first (:pos sink-mass)) ;; (:x (get entities sink-id))
		       (:x2 edge))
	    sink-y (or (and (or (= sink-id :mouse) (:held (get entities sink-id))) my)
		       (second (:pos sink-mass))
		       ;; (:y (get entities sink-id))
		       (:y2 edge))
	  
	    ;; test (println "src-x: " src-x "\tsrc-y: " src-y
	    ;; 		"\nsink-x: " sink-x "\tsink-y: " sink-y)
	  
	    ;; cp1-x src-x
	    ;; cp1-y (* src-y (cos (radians time)))

	    ;; cp2-x sink-x
	    ;; cp2-y (* sink-y (cos (radians time)))
	    ]
      
	;; (println "_________"
	;; 	       "sink: " sink "; (get entities sink): " (get entities sink)
	;; 	       "\nsink-x: " sink-x "\tsink-y: " sink-y)

	(when (and src-x src-y sink-x sink-y) ;;TODO: fix the problem, not the consequence
	  (let [[x1 y1] [src-x   src-y]
		[x2 y2] [sink-x sink-y]
		;; _ (when (or x1 y1 x2 y2) 
		;;     (println (str  "src:" [x1 y1] "\t snk:" [x2 y2])))
		;;TODO: still blows up
		[cx cy] (vec-add [x2 y2] (map #(* 1/7 %) (vec-sub [x1 y1] [x2 y2])))
		[dx dy] (vec-sub [x1 y1] (map #(* 1/7 %) (vec-sub [x1 y1] [x2 y2])))
		ris (- y2 y1)
		run (- x2 x1)
		m  (/ ris (if (zero? run) 0.01 run))
		pm (if (zero? m) 1 (/ -1 m))
		d  (dist x1 y1 x2 y2)
		dL 40 ;chosen blindly for now. eventually will be a fn of (dist src sink)
		theta (atan (abs pm))	;

		
		;; CONTROL POINTS
		;; 
		px (if  (< src-x sink-x)
		     (- cx (* dL (cos theta)))
		     (+ cx (* dL (cos theta))))
		py (if (< src-x sink-x)
		     (- cy (* dL (sin theta)))
		     (+ cy (* dL (cos theta))))

		qx (if (< src-x sink-x)
		     (- dx (* dL (cos theta)))
		     (+ dx (* dL (cos theta))))
		qy (if (< src-x sink-x)
		     (- dy (* dL (sin theta)))
		     (+ dy (* dL (sin theta))))
		]
	    ;; (fill-float 220 110 10)
	    ;; (no-stroke)
	    ;; (ellipse (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) 8 8)
	    
	    ;; MIDPOINTS
	    ;; (ellipse cx cy 8 8)
	    ;; (ellipse dx dy 8 8)

	    ;; DRAW CONTROL POINTS
	    ;; (fill-float 10 220 220)
	    ;; (ellipse px py 4 4)
	    ;; (ellipse qx qy 4 4)
	
	

	    (fill-float 220 110 10)
	    (text-font @*debug-font* 14)
	    ;; (string->text (apply str (concat "slope: " (str  m))) 800 800)
	    ;; (string->text (apply str (concat "perpd: " (str pm))) cx cy)
	    ;; (string->text (apply str (concat "dist:  " (str  d))) 800 840)
	    ;; (string->text (apply str (concat "[px py]:  " (str [px py]))) 800 860)
	    ;; (string->text (apply str (concat "[qx qy]:  " (str [qx qy]))) 800 880)
	    ;; (string->text (apply str (concat "P: " (str (vec (map floor  [px py])))
	    ;; 				   ", "
	    ;; 				   "Q: " (str  (vec (map floor [qx qy]))))) px py)
	    ;; (string->text (apply str (concat "Q: " (str  [qx qy]))) qx qy)
	    ;; (string->text (apply str (concat ":  " (str  d))) 800 900)
	    ;; (string->text (apply str (concat ":  " (str  d))) 800 900)
	    ;; (string->text (apply str (concat ":  " (str  d))) 800 900)
	
	

	    (no-fill)
	    ;; (stroke-weight 4)
	    (stroke 120 150 190)
	    ;; (line src-x src-y sink-x sink-y)

	    ;; KEEP THIS
	    (stroke-weight (+ 4 (abs (* 4 (sin (/ @*time* 100))))))
	    (bezier src-x src-y
		    qx qy
		    px py
		    sink-x sink-y)
	
	    ;; src-x src-y      p0
	    ;; qx qy            p1
	    ;; px py            p2
	    ;; sink-x sink-y    p3

	    (let [t (nth (cycle bezier-midpoints) @*time*  ;; (mod @*time* 50)
			 )]
	      (let [[px0 py0] [src-x src-y]
		    [px1 py1] [qx qy]
		    [px2 py2] [px py]
		    [px3 py3] [sink-x sink-y]
		    bx (+ (* (pow (- 1 t) 3) px0)
			  (* (pow (- 1 t) 2) px1 t 3)
			  (* (pow (- 1 t) 1) px2 t t 3)
			  (* t t t px3))
		    by (+ (* (pow (- 1 t) 3) py0)
			  (* (pow (- 1 t) 2) py1 t 3)
			  (* (pow (- 1 t) 1) py2 t t 3)
			  (* t t t py3))]
		(fill 200 220 210 255)
		;; (draw-hypno bx by)
		(ellipse bx by 5 5)
		))	    
	    


            ;;;; DRAWS ellipses along curve
	    ;; (doseq [t (range 0 1 0.05)]
	    ;;   (let [[px0 py0] [src-x src-y]
	    ;; 	[px1 py1] [qx qy]
	    ;; 	[px2 py2] [px py]
	    ;; 	[px3 py3] [sink-x sink-y]
	    ;; 	bx (+ (* (pow (- 1 t) 3) px0)
	    ;; 	      (* (pow (- 1 t) 2) px1 t 3)
	    ;; 	      (* (pow (- 1 t) 1) px2 t t 3)
	    ;; 	      (* t t t px3))
	    ;; 	by (+ (* (pow (- 1 t) 3) py0)
	    ;; 	      (* (pow (- 1 t) 2) py1 t 3)
	    ;; 	      (* (pow (- 1 t) 1) py2 t t 3)
	    ;; 	      (* t t t py3))]
	    ;;     (fill 200 220 210 255)
	    ;;     ;; (draw-hypno bx by)
	    ;;     (ellipse bx by 5 5)
	    ;;     ))

	    ;; (curve 	px py
	    ;; 	src-x src-y
	    ;; 	sink-x sink-y
	    ;; 	qx qy)
	
	
	    ;; (line src-x src-y sink-x sink-y)

	    ;; (let [heading  (atan2 (- sink-x src-x) (- sink-y src-y))]
	    ;;   (with-translation [sink-x sink-y]
	    ;; 	(with-rotation [(- PI heading)]
	    ;; 	  (with-rotation [QUARTER_PI]
	    ;; 	    (line 0 0 0 10))
	    ;; 	  (with-rotation [(- QUARTER_PI)]
	    ;; 	    (line 0 0 0 10)))))
	    ))
      

	;; (curve (+ src-x (* src-x (cos (radians time)))) ;cp1-x
	;; 	     (+ src-y (* src-y (cos (radians time))))	      ;cp1-y
	;; 	     src-x  src-y
	;; 	     sink-x sink-y 
	;; 	     sink-x sink-y 
	;; 	     )
      

	;; draw the control points
	;; (fill 120 150 190)
	;; (ellipse orb-x ; (+ src-x (* src-x (cos (radians time))))
	;; 	       orb-y ; (+ src-y (* src-y (cos (radians time))))
	;; 	       4 ;src-size
	;; 	       4 ;src-size
					;)
	;;(ellipse cp2-x cp2-y 4 4)
	))))



(defn draw-node-edges [toke]
  ;; draw edges
  (doseq [e (vals (:out toke))] (draw-edge e);;  all
   ;; (map draw-edge (vals (:out toke)))
   ))


(defn draw-edges []
  (when-let [edges (vals @*edges*)]
    (doseq [e edges] (draw-edge e))))


(defmacro shape [type & body]
  `(do
     (begin-shape ~type)
     ~@(map (fn [[x# y#]] `(vertex ~x# ~y#)) body)
     (end-shape CLOSE)))

(defn draw-token [toke]
  (when (and toke (get @*masses* (:id toke)))
    ;; (println "nodes: " (nodes)
    ;; 	     "\ntoke: "  toke
    ;; 	     "\nmass:" (get @*masses* (:id toke)))
    (let [[mx my] @*mouse-pos*
	  mass (get @*masses* (:id toke))
	  held (:held toke)
	  time @*time*
	  x (or (and held mx) (first (:pos mass)) (:x toke))
	  y (or (and held my) (second (:pos mass)) (:y toke))
	  r   (:r toke)
	  amt (:amt toke)
	  out (:out toke)
	  [vx vy] (:vel mass)
	  heading (atan2 vy vx)]
      
      ;; Draw a Background 
      (fill 0 145 63)
      (stroke 54)
      (stroke-weight 2)
      (ellipse-mode RADIUS)
      (ellipse x y r r)
      
      ;; HYPNO
      ;; (draw-hypno x y)
      
      ;;  Molecule
      (fill 0 187 63 63)
      (stroke 54)
      (stroke-weight 2)
      (ellipse-mode RADIUS)

      ;; (ellipse x y r r)
      ;; (with-translation [x y]
      ;; 	(with-rotation [(first (drop (mod @*time* (/ TWO_PI 0.03)) (cycle (range 0 TWO_PI 0.03))))]
      ;; 	  (fill-float 0 187 63 255)
      ;; 	  (shape :triangles
      ;; 		 [ 0     25]
      ;; 		 [-35   -25]
      ;; 		 [ 35   -25])))

      ;;  pipes
      ;; (stroke 187 220 173)
      ;; (stroke-weight 8)




      ;; velocity vector
      ;; (stroke 210)
      ;; (stroke-weight 2)
      ;; (with-translation [x y]
      ;; 	(fill-float 220)
      ;; 	;; (text-font @*token-font* 12)
      ;; 	;; (string->text (apply str (concat "heading: " (str heading))) 80 80)
      ;; 	(when (or (< 0 (abs vx)) (< 0 (abs vy)))

      ;; 	  (with-rotation [(+ HALF_PI heading)]
	  
      ;; 	    (line 0 0 0 (- (* 2 *token-radius*)))
      ;; 	    (with-translation [0 (- (* 2 *token-radius*))]
      ;; 	      (with-rotation [QUARTER_PI]
      ;; 		(line 0 0 0 10))
      ;; 	      (with-rotation [(- QUARTER_PI)]
      ;; 		(line 0 0 0 10))))))
      
      ;; text on mouse-over
      (when (cursor-above? (:id toke))
	(let [mol (get @*moles* (:id toke))]
	 ;; (println "\n\n~~\n\ncrsr: " (cursor-above? (:id toke)) "\n\n~~")
	 (text-font @*token-font* 18)
	 (text-align LEFT)
	 (fill 200)
	 (string->text (str "Node: " (:id toke) "\n"
			    "Name: " (:name mol) "\n"
			    "Concentration: " (:amt mol))
		       ;; (str amt)
		       (- x 200 r) (- y (/ r 3))))))))


(defn draw-tokens []
  (when-let [tokes (vals @*nodes*)]
    (doseq [t tokes] (draw-token t))))

(defn draw-walker [x y]
  (with-translation [x (- y 25)]
    (fill-float 120 90 110)
    (stroke-weight 2)
    (stroke-float 70)
    (ellipse 0 0 15 15)
    (curve 0 0
	    10 0 
	    0 40
	    0 50)
    (ellipse 0 50 3 3)
    )
  (with-translation [x (+ y 25)]
    (fill-float 190 110 190 80)
    (ellipse 0 0 15 15)))

(defn draw-cursor [c]
  (let [[x y] (:pos c)]
    (with-translation [x y]
      (fill-float 190 110 190 80)
      (ellipse-mode CENTER)
      (ellipse 0 0 15 15))))

(defn draw-cursors []
  (dorun (map draw-cursor (vals (dissoc @*cursors* :mouse)))))

(defn draw-kbd-key [k]
  (let [pos (:pos k)
	s  (with-out-str (prn (:form k)))]
    (with-translation [pos]
      (text-font  @*token-font* 22 )
      (no-fill)
      (stroke-weight 3)
      (stroke-float 255 89 80)
      ;; (rect 0 0 (+ 5 (text-width (:str k))) 50)
      (text-align LEFT)
      (string->text s 2 20))))


(def atoms* (ref {0 {:pos [200 200] :form 'let  :frc {}}
		  1 {:pos [600 600] :form 'cond :frc {}}
		  2 {:pos [349 349] :form '()   :frc {}}}))

(defn draw-atom [a]
  (let [sexp (with-out-str (pr (:form a)))]
    (with-translation [(:pos a)]
      (no-fill)
      (rect-mode CORNER)
      (rect -2.5
	    -21.5
	    (+ 4.5 (text-width sexp))
	    (+ 28.5 (* 28.5 (count (filter #{\newline} sexp)))))
      (color-mode HSB 1.0)
      (fill-float 0.62 0.75 0.98)
      (text-align LEFT)
      (string->text sexp 0 0)
      (no-fill)
      (stroke-float 255 89 80)
      (rect -2.5
	    -21.5
	    (+ 4.5 (text-width sexp))
	    (+ 28.5 (* 28.5 (count (filter #{\newline} sexp))))))))


(defn draw-component [c]
  (let [render-fn (:render c)]
    (render-fn c)))

(defn draw-time [t]
  (text-font @*token-font* 36)
  (text-align CENTER)
  (fill 255)
  (string->text (str "Time: " (Math/floor (/ t 60)))
		(- *screen-width* (/ *screen-width* 8))
		80))


(defn draw []
  (color-mode RGB)
  ;; (background 70 111 213)
  ;; (background 90 111 113)

  (background 0)

  (draw-time (if (pos? (count @*nodes*)) @*time* 0))


  ;; (doseq [i (range) :while (< (* i 15) *screen-width*)]
  ;;   (with-translation [(* 15 i) 0]
  ;;     (stroke-weight 1)
  ;;     (stroke-float 55)
  ;;     (line 0 0 0 *screen-height*)))

  ;; (doseq [j (range) :while (< (* j 15) *screen-height*)]
  ;;   (with-translation [0 (* 15 j)]
  ;;     (stroke-weight 1)
  ;;     (stroke-float 55)
  ;;     (line 0 0 *screen-width* 0)))
  ;; (background 0)

  ;; (let [kbd-key {:pos [(mouse-x) (mouse-y)] :form '(let [x 5] x)}]
  ;;   (draw-kbd-key kbd-key))


  ;; (dorun (map draw-atom (vals @atoms*)))
  (color-mode RGB 255)

  ;; (draw-walker 500 500)
  
  
  
  (draw-edges)

  ;; draw currently HELD edges
  ;;     TODO - this would ideally be done ACROSS all CURSORS,
  ;;            but for now we're only tracking the mouse
  
  (when @*current-edge*
    (let [[x y] @*mouse-pos*
  	  masses (vals @*masses*)]
      ;; (println "drawring current edge: " @*current-edge*)
      (when-let [mass (mass-at x y)]
  	;; (println "when-let node :" mass)

	;; this was commented out for the single-click executable. needs to be factor'd out somehow
  	;; (let [[mass-x mass-y] (:pos mass)]
  	;;   (fill 191 82 104)
  	;;   (ellipse mass-x mass-y (/ *token-radius* 5) (/ *token-radius* 5)))
	))
    (draw-edge @*current-edge*))

  (draw-tokens)

  (draw-cursors)


  ;;  shit hack --- TODO: map across all cursors
  (when @*current-edge*
    (let [[x y] @*mouse-pos*
  	  masses (vals @*masses*)]
      (when-let [mass (mass-at x y)]
  	(let [[mass-x mass-y] (:pos mass)]
  	  (fill 191 82 104)
	  (stroke  191 82 104)
  	  (ellipse mass-x mass-y (/ *token-radius* 5) (/ *token-radius* 5))))))

    

  

  (try ;; (draw-debug)
       (catch Exception e (.printStackTrace e)))
  ;; (try
  ;;   (doseq [i (range 5) j (range 5)]
  ;;     (if (and (< 4 (mod @*time* 10) 8))
  ;; 	(string->text "Synlab"
  ;; 		      (mod (+ (rand-int 50) (* i 80) (Math/floor (/ @*time* 40))) *screen-width*)
  ;; 		      (mod (+ (rand-int 50) (* j 30) (Math/floor (/ @*time* 40))) *screen-height*)))
  ;;     (if (and (< 6 (mod @*time* 12) 11))
  ;; 	(string->text "Fund GVU"
  ;; 		      (mod (+ (rand-int 60) (* i 180) (Math/floor (/ @*time* 80))) *screen-width*)
  ;; 		      (mod (+ (rand-int 80) (* j 200) (Math/floor (/ @*time* 80))) *screen-height*)))
  ;;     (if (and (< 0 (mod @*time* 20) 10))
  ;; 	(string->text "Synlab <3's Steelcase"
  ;; 		      (mod (+ (rand-int 20) (* i 180) (Math/floor (/ @*time* 100))) *screen-width*)
  ;; 		      (mod (+ (rand-int 10) (* j 200) (Math/floor (/ @*time* 100))) *screen-height*))))
  ;;   (catch Exception e (.printStackTrace e)))


  ;; (doseq [x (range 400 1200 80)]
  ;;   (with-translation [x 500]
  ;;     (stroke-float 0 120 225 255)  ;;TRON
  ;;     (stroke-weight 4)
  ;;     (line 0 0 60 0)
  ;;     (with-translation [70 0]
  ;; 	(fill-float 0 120 225 255)
  ;; 	(no-stroke)	
  ;; 	(ellipse 0 -10 4 4)
  ;; 	(ellipse 0   0 4 4)
  ;; 	(ellipse 0  10 4 4))))
  ;; (stroke-float 0 120 225 255)
  ;; (stroke-weight 4)
  ;; (line 1200 500 1260 500)

  ;; (let [x (nth (range 400 1260) (mod @*time* (- 1260 400)))
  ;; 	r (if (zero? (mod x 80)) 10 6)]
  ;;   (no-stroke)
  ;;   (fill-float 245 10 10 255)
  ;;   (ellipse x 500 r r))

  (doseq [x (range 0 TWO_PI )])
  (update))


(defn clear []
  (dosync (ref-set *edges*  {})
	  (ref-set *nodes*  {})
	  (ref-set *masses* {}))
  (reset! *entity-counter* 0)
  (reset! *time* 0))




;;;; Mouse Events
(defn mouse-moved [evt]
  (let [x (.getX evt)
	y (.getY evt)
	mouse (get @*cursors* :mouse)]

    ;; (println "moved: " x ", " y "\telapsed: " (- (now) @last*))
    (dosync
     (alter *cursors* assoc :mouse (update-cursor-pos :mouse [x y])))
    (reset! *mouse-pos* [x y])))



(defn pickup-token
  "attaches token to cursor located at position [x y]"
  [x y & t]
  (when-let [mass (or (first t) (mass-at x y))]
    (let [node (get @*nodes* (:id mass))]
      (dosync
       (alter *nodes* assoc
	      (:id node) (node. (:id node) (first @*mouse-pos*) (second @*mouse-pos*) *token-radius*
				(:amt node) true :node (:in node) (:out node)))))))



(defn best-point-between
  "finds a point that is collinear w.r.t the line
   passing through the two given points.
   x1 y1 - token
   x2 y2 - mouse "
  [x1 y1 x2 y2]
  (let [offset 30
	dy (- y2 y1)
	dx (if (= x2 x1) 0.01 (- x2 x1))
	slope (/ dy dx)
	theta (atan slope)
	x-nu (round (- x2 (* offset (sin (radians theta)))))
	y-nu (round (- y2 (* offset (cos (radians theta)))))]

    [x-nu y-nu]))




(defn start-edge
  "adds an edge to the :out field for
   the token at [x y]."
  [x y]
  (when-let [mass (mass-at x y)]
    (let [id @*entity-counter*
	  [cursor-x cursor-y]  @*mouse-pos*
	  [mass-x mass-y] (:pos mass)
;;	  [src-x src-y] (best-point-between mass-x mass-y cursor-x cursor-y)
	  edge (edge. id (:id mass) :mouse mass-x mass-y cursor-x cursor-y :edge 1)]
    
      (swap! *entity-counter* inc)
      (reset! *current-edge* edge)
    
      ;(println "start-edge~~~\ntoke: " toke " mouse: " mx " , " my)

      ;; this will add the edge to the global *nodes* ref... not sure we want that
      ;(dosync (alter *nodes* (fn [tbl] (assoc tbl id edge))))

      ;; (let [res (assoc-in toke [:out id] edge)] 
      ;; 	(println "res: " res)
      ;; 	res)
      )))





(defn held-tokens
  "returns a seq of currently held tokens"
  [tokes]
  (filter :held tokes))



(defn attach-edge
  "alters global *edges*, and sets edge's sink to node"
  [edge node]
  (when (and edge
	     node
	     (not (= (:id node) (:src edge)))
	     (not (= (:id node) (:sink edge))))
    (let [ x2 (:x node) y2 (:y node)
					; (best-point-between (:x1 edge) (:x2 edge) (:x node) (:y node))
	  old-src (get @*nodes* (:src edge)) 
	  
	  edge  (edge.  (:id edge)
			(:src edge)(:id node)
			(:x1 edge) (:y1 edge)
			x2 y2
			:edge
			1)		;rate

	  sink  (node. (:id node)
		       (:x node) (:y node)
		       (:r node)
		       (:amt node) (:held node)
		       :node		;type
		       (assoc (:in node) (:id edge) edge)
		       (:out node))
												  
	  new-src (node. (:id old-src)
			 (:x old-src) (:y old-src)
			 (:r old-src)
			 (:amt old-src) (:held old-src)
			 :node		;type
			 (:in old-src)
			 (assoc (:out old-src) (:id edge) edge))]
								     
								     
      ;; (println "ATTACH-EDGE"
      ;; 	       "\nedge: " edge
      ;; 	       "\n~_~__~~"
      ;; 	       "\nnode: " node
      ;; 	       "\n~_~__~~"
      ;; 	       "\nsink x,y: [ " x2 ", " y2 " ] "
      ;; 	       "\n~_~__~~"
      ;; 	       "\nold-src: " old-src
      ;; 	       "\n~_~__~~"
      ;; 	       "\nedge : " edge
      ;; 	       "\n~_~__~~"
      ;; 	       "\nsink: " sink
      ;; 	       "\n~_~__~~"
      ;; 	       "\nnew-src: " new-src)
      (dosync
       (alter *nodes*
	      (fn [tbl]
		(assoc tbl (:id sink) sink

		       (:id new-src) new-src))))

      (dosync
       (alter *edges*
	      (fn [tbl]
		(assoc tbl (:id edge) edge)))))))



(defn update-edges [toke])


(defn mouse-released [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]
    (when-let [tokes (held-tokens (vals @*nodes*))]
      (drop-token :mouse x y tokes)
      (update-edges tokes))

    ;; exit edge-making-mode on mouse release
    (when @*edge-making-mode*
      (when-let [mass (mass-at x y);; (first )
		 ]
	(attach-edge @*current-edge* (get @*nodes* (:id mass))))
      (compare-and-set! *edge-making-mode* true false)
      (reset! *current-edge* nil))
        
    (swap! *mouse-button* not)
    (swap! draw-circle? not)
    (reset! *mouse-pos* [x y])))

(defn mouse-dragged [evt]
  (let [x (.getX evt)
	y (.getY evt)]
    ;; (println "mouse-dragged.")
    (dosync
     (alter *cursors* assoc :mouse (update-cursor-pos :mouse [x y])))
    (reset! *mouse-pos* [x y])))

(defn key-pressed [evt]
  (let [char (.getKeyChar evt)
	code (.getKeyCode evt)]
    (println "key-pressed: " code)
    (cond (= code 16) (compare-and-set! *edge-making-mode* false true))))

(defn key-released [evt]
  (let [[x y] @*mouse-pos*
	char (.getKeyChar evt)
	code (.getKeyCode evt)]
    (println "key-released: " code)

    (cond (= char \space) (clear)

	  (= code 16) (when @*edge-making-mode*
			(when-let [mass (mass-at x y) ;; (first )
				   ]
			  (attach-edge @*current-edge* (get @*nodes* (:id mass)))
			  (reset! *current-edge* nil)
			  (compare-and-set! *edge-making-mode* true false))))))




(defn draw-tuio-cursors []
  (map draw-cursor (tuio/cursors *tuio*)))

(def *tuio-ch* (comm/channel ))

(defn init-tuio-client []
  (try (tuio/connect! *tuio*)
       (doto *tuio*
	 ;; (tuio/on-add-object!
	 ;;  (println "add object"))
	 
	 ;; (tuio/on-update-object!
	 ;;  (println "update object"))

	 ;; (tuio/on-remove-object!
	 ;;  (println "remove object"))

	 (tuio/on-add-cursor! curs
          (do
	    ;; (println "cursor added:   \n"
	    ;; 	     [(.getCursorID curs)
	    ;; 	      (.getScreenX curs *screen-width*)
	    ;; 	      (.getScreenY curs *screen-height*)]
	    ;; 	     "___")
	    (dosync (alter *cursors* assoc (.getCursorID curs)
			   {:pos [(.getScreenX curs  *screen-width*)
				  (.getScreenY curs *screen-height*)]
			    :vel [0 0] :path nil}))
	    ;; (event ::add-cursor
	    ;; 	   :id (.getCursorID)
	    ;; 	   :pos [(.getScreenX curs *screen-width*)
	    ;; 		 (.getScreenY curs *screen-height*)])
	    ))

	 (tuio/on-update-cursor! curs
          (do ;; (println "cursor updated: \n"
	      ;; 	       [(.getCursorID curs)
	      ;; 		(.getScreenX curs *screen-width*)
	      ;; 		(.getScreenY curs *screen-height*)
	      ;; 		(.isMoving  curs)
	      ;; 		(.getXSpeed curs)
	      ;; 		(.getYSpeed curs)]
	      ;; 	       "___")
	      (dosync (alter *cursors* assoc (.getCursorID curs) {:pos [(.getScreenX curs *screen-width*)
									(.getScreenY curs *screen-height*)]
								  :vel [(.getXSpeed curs)  (.getYSpeed curs)]}))

	      (comm/enqueue *tuio-ch* {:crsr (.getCursorID curs)
				       :pos  [(.getScreenX curs *screen-width*)
					      (.getScreenY curs *screen-height*)]
				       :time @*time*})

	      ;; (event ::update-cursor
	      ;; 	     :id  (.getCursorID curs)
	      ;; 	     :pos [(.getScreenX curs *screen-width*)
	      ;; 		   (.getScreenY curs *screen-height*)]	
	      ;; 	     :vel [(.getXSpeed curs)  (.getYSpeed curs)])
	      ))

	  (tuio/on-remove-cursor! curs
           (do (println "remove cursor")
	       (dosync
		(alter *cursors* dissoc (.getCursorID curs)))))

	 (tuio/on-refresh! tobj () ;; (println tobj)
			   )
	 )
       (catch Exception e (.printStackTrace e))))

;; (on-event ::cursor-added (fn [evt]
;; 			   (when-let [near-syms (filter (fn [atm]
;; 							  (let [[px py] (:pos atm)
;; 								[cx cy] (:pos evt)]
;; 							    (< (+ (sq (- px cx))
;; 								  (sq (- py cy)))
;; 							       25)))
;; 							@atoms*)
;; 				      sorted-syms (sort (fn [a b]
;; 							  (let [[ax ay] (:pos a)
;; 								[bx by] (:pos b)
;; 								[cx cy] (:pos evt)]
;; 							    (< (+ (sq (- ax cx))
;; 								  (sq (- ay cy)))
;; 							       (+ (sq (- bx cx))
;; 								  (sq (- by cy))))))
;; 							near-syms)]
;; 			     (event ::add-force
;; 				    :obj (first sorted-syms)
;; 				    :frc {:name (str "tuio-cursor-" (:id evt))}))))
;; (on-event ::cursor-moved )


 (defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (comm/close *tuio-ch*)
   (println "tuio client disconnected"))


 (defn setup
   "executes once."
   []
   ;; (print-logo "Pathways")
   ;; (loop [n 10]
   ;;   (when (> n 0)
   ;;     (do (println "\n") (recur (dec n)))))
   ;; (println ";;;;;;;;;;;;;;;;PATHWAYS;;;;;;;;;;;;;;;;")
   ;; (println ";;;;;;;;;;;;;;;;|______|;;;;;;;;;;;;;;;;")
   (smooth)
   ;; ; (background-float 200 200 255)

   (framerate *framerate*)
   (reset! *time* 0)
   (reset! *token-font* (load-font "Monaco-48.vlw"))
   (reset! *debug-font* (load-font "Monaco-10.vlw"))
   (reset! *previous-time* @*time*)
   (reset! *frame-counter* 0)
   (reset! *current-edge* nil)
   (init-tuio-client)
   (clear))


 (defn mouse-entered [evt]
   (let [x (.getX evt)
	 y (.getY evt)]))


 (defapplet pathways
   :title "Pathways"
   :setup setup
   :draw draw
   :size [*screen-width*  *screen-height*]
   :mouse-moved mouse-moved
   :mouse-pressed mouse-pressed
   :mouse-released mouse-released
   :mouse-dragged mouse-dragged
   :mouse-entered mouse-entered
   :mouse-exited mouse-exited
   :key-pressed key-pressed
   :key-released key-released)



 ;;   What does this *really* mean???
 ;;;; F = ma


 ;;   Final velocity, given acceleration, initial velocity, time
 ;;;; Vf = Vo + at

 ;;   Displacement at a time, given acceleration, initial velocity
 ;;;; Sf = So + Vi*t + (/ (* a (* t t)) 2)

 ;;   Displacement at a time, given velocity
 ;;;; Sf = V*t

 ;;   
 ;;;; Vavg = (/ (+ Vf Vo) t)

 ;; amt stored is squared magnitude of vector
 ;; (defrecord position [amt dir])

 ;; amt stored is squared magnitude of vector
 ;; (defrecord velocity [amt dir])

 ;; amt stored is squared magnitude of vector
 ;; (defrecord acceleration [amt dir])

 ;; (defrecord force [point-mass accel time])


 ;; public void mouseEntered( MouseEvent e ) {
 ;;       // called when the pointer enters the applet's rectangular area
 ;;    }
 ;;    public void mouseExited( MouseEvent e ) {
 ;;       // called when the pointer leaves the applet's rectangular area
 ;;    }
 ;;    public void mouseClicked( MouseEvent e ) {
 ;;       // called after a press and release of a mouse button
 ;;       // with no motion in between
 ;;       // (If the user presses, drags, and then releases, there will be
 ;;       // no click event generated.)
 ;;    }

;; (defn -main [& args]
;;   (run pathways))

;; (stop pathways)
;; (run pathways :interactive)