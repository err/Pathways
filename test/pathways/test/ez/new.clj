(ns new.core
    (:use [clojure.contrib.math :only [floor]]
	  [clojure.contrib.core :only [dissoc-in]]
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
	    [incanter.stats :as stats]
	    [lamina.core :as comm])
  (:import (java.awt MouseInfo)))


(declare *world*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;__GLOBALS__;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;;;;;;;;;;;;;;;;;
;;   DIMENSIONS                                                               ;;;;;;;;;;;;;;;;;;
(def *screen-width*        1920;; 1680
     )                                              ;;;;;;;;;;;;;;;;;;
(def *screen-height*       1200;; 1050
     )                                              ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTILS
(defn half [n]
  (/ n 2))

(defn get-body [id]
  (get-in @*world* [:bodies (long id)]))

(defn get-actor [id]
  (get-in @*world* [:actors (long id)]))

(defn screen [pos]
  (let [[x y] pos]
    [(* x *screen-width*) (* y *screen-height*)]))


;;   FONTS                                                                    ;; FONTS
(def *font*          (atom nil))                  

;;   TIME                                                                     ;; TIME
(def *framerate*             60)                  
(def *time*            (atom 0))                  
(def *running?*    (atom false))                  


 
;;   MOUSE                                                                    ;; MOUSE
(def *mouse-pos*          (atom [0 0]))                                       
(def *mouse-button*         (atom nil))

                                       
(defn mouse-x []  (first @*mouse-pos*))                                       
(defn mouse-y [] (second @*mouse-pos*))                                       
(defn mouse-pos [] (get-in @*world* [:crsr :mouse] [0 0]))


;;   WORLD STATE                                                              ;; WORLD
(def *world*                                                                 
     (ref {:time 0				                             
	   :crsr {:mouse {:id :mouse                                         
			  :pos [0 0]	                                     
			  :etc {:dn false
				:start 0}}}
	   :actors  {}                                     ; actors - agency - (forms)
	   :bodies  {};; {-1 {:id -1                            ; bodies - collision
		    ;; 	 :pos  [0.4 0.4]
		    ;; 	 :size [60 60] ; shouldn't be in pixels, shouldn't be in body?
		    ;; 	 :bounds-fn #(let [[a b] (screen (:pos %1))
		    ;; 			   [x y] (screen  %2)
		    ;; 			   [w h] (:size %1)]
		    ;; 		       (and (<= a x)
		    ;; 			    (<= x (+ a w))
		    ;; 			    (<= a y)
		    ;; 			    (<= y (+ a h))))}}
	   :visuals {};; {-1 {:id -1
		    ;; 	 :draw (fn [this]
		    ;; 		 (let [t 0
		    ;; 		       body (get-body (:id this))]
		    ;; 		   (when body
		    ;; 		     (if (> (- (.getTotalMilliseconds (tuio/tuio-time)) t)
		    ;; 			    5000)
		    ;; 		       (fill 200 100 0 100)
		    ;; 		       (fill 90 90 90 255))
		    ;; 		     (with-translation (screen (:pos body))
		    ;; 		       (no-stroke)
		    ;; 		       (rect-mode CENTER)
		    ;; 		       (rect 30 30 (first (:size body)) (second (:size body)))
		    ;; 		       (when ((:bounds-fn body) body [(mouse-x) (mouse-y)])
		    ;; 			 (fill 00 0 200 255)
		    ;; 			 (ellipse 30 30 10 10))))))
		    ;; 	 :etc {:start 0}}}
	   }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                mouse                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mouse-pos [] ())

(defn add-entity [e]
  (dosync (alter *world* assoc-in [:actors (type e) (:id e)] e)))

(defn rem-entity [e]
  (let [id (or (:id e) e)]
    (dosync (alter *world* dissoc-in [:actors (type e) id]))))

(defn tick []
  (dosync (alter *world* update-in [:time] inc)))

(defn entities [w]
  (vals (:bodies w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                mouse                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;pathway;;;;;;;;;;;;;;;;;;;;;
;; [raf* [mek ack]]                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;_records_;;;;;;;;;;;;;;;;;;;

;; structural
(defrecord Node   [id in out etc])
(defrecord Edge   [id src sink etc])
(defrecord Cursor [id pos etc])

;; ODE
;; (defrecord ODE [nodes edges])

;; visual
(defrecord Molecule [id pos draw]) 
(defrecord Reaction [id pos draw]) 
(defrecord CrsrIcon [id pos draw]) 


;; constructors
(defn make-node [map]
  (Node. (:id map) (:in map) (:out map) (:etc map)))

(defn make-edge [map]
  (Edge. (:id map) (:src map) (:sink map) (:etc map)))

(defn make-cursor [map]
  (Cursor. (:id map) (:pos map) (:etc map)))

(defn make-molecule [id name concentration]
  (Molecule. id name concentration))

(defn make-reaction [id name rate]
  (Reaction. id name rate))

(defn make-graph
  ([]
     (make-graph (hash-map :nodes (sorted-map)
			   :edges (sorted-map))))
  ([nodes edges]
     (hash-map :nodes nodes
	       :edges edges)))

(defn node-type [n]
  (or (:type (:etc n)) :unspecified-node-type))

;; (defrecord force  [id acc time acc-fn]
;;   ;; id     - id of force (is this necessary?)
;;   ;; acc    - [xacc yacc] component vectors
;;   ;; time   - time at which force began
;;   ;; acc-fn - function for updating the acc vectors at each time-step


(def *graph*   (ref (make-graph {0 (make-node {:id 0 :in #{3} :out #{2} :etc {:type :molecule}})
				 1 (make-node {:id 1 :in #{2} :out #{3} :etc {:type :molecule}})}

				{2 (make-edge {:id 2 :src 0 :sink 1 :etc {}})
				 3 (make-edge {:id 3 :src 1 :sink 0 :etc {}})})))
     
(def *cursors* (ref {:mouse (Cursor. :mouse @*mouse-pos*{})}))
;; (def *nodes*   (ref {}))
;; (def *edges*   (ref {}))

(def *moles*   (ref {}))
(def *reacs*   (ref {}))

(def *masses*  (ref {}))
(def *entity-counter* (atom 0))

;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |         RENDER         | ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;
;;;;;;;; ;;;;;;;; ;;;;;;;; |;;;;;;; ;;;;;;;; ;;;;;;;| ;;;;;;;; ;;;;;;;; ;;;;;;;;

(defn draw []
  (background 20)
  (no-stroke)

  ;; (doseq [x (range 0 *screen-width* 40)
  ;; 	  y (range 0 *screen-height* 40)]
  ;;   (fill 225)
  ;;   (with-translation [x y]
  ;;     (ellipse-mode CENTER)
  ;;     (ellipse 0.5 0.5 2 2)))
  
  (let [cursors (get @*world* :crsr)]
    (doseq [[k c] cursors]
      (let [w 40
	    [x y] (:pos c)
	    px (* w (Math/floor (/ x w)))
	    py (* w (Math/floor (/ y w)))]
	;; fills surrounding panel
	;; (with-translation [px py]
	;; 	(fill 110)
	;; 	(rect-mode CORNER)
	;; 	(rect 0.5 0.5 w w))

	;; cursor 
	(fill 245 40 225)
	(with-translation [(* *screen-width* x) (* *screen-height* y)]
	  (with-rotation [(- QUARTER_PI)]
	    (rect-mode CENTER)
	    (rect 0.5 0.5 20 20)))

	;; (let [mincx (apply min (map #(-> % :pos first) cursors))])
	)))
  
  (doseq [[id v] (get-in @*world* [:visuals])]
    ((:draw v) v)))


(def *tuio-port* 3333)
(def *tuio* (tuio/client *tuio-port*))
(def *tch* (comm/channel))

(def *threshold* (ref {:make-node 185
		       :grab-node 215}))

(defn threshold
  "Returns the threshold (in milliseconds) for the given action"
  [action]
  (get @*threshold* action))



(defn draw-char [b]
  (let [now (.getTotalMilliseconds (tuio/tuio-time))
	start (:start (:etc b))
	body (get-body (:id b))
	[r1 r2] (:size body)]

    (try
      (no-stroke)
      (ellipse-mode RADIUS)
      (text-align CENTER)
      (with-translation (screen (:pos body))
	(case 
	 false (do
		 (fill 40 80 230 155)
		 (text-font @*font* 44)
		 (string->text  "(" 0 0)
		 (rect-mode CENTER)
		 (rect 0 -10 (text-width "(") 50))
	 true (do
		(stroke 240 90 90 255)
		(no-fill)
		(rect 0 -10 (* 2 (text-width "(")) 50)
	      
		(fill 50 90 240 205)
		(text-font @*font* 44)
		(string->text  ")" 0 0)

		(stroke 240 90 90 255)
		(rect-mode CENTER)
		(no-fill)
		(rect 0 -10 (text-width "(") 50)
		))
	(when ((:bounds-fn body) body @*mouse-pos*)
	  (fill 50 200 10 227)
	  (ellipse 0 0 5 5))

	;; print id
	(fill 200 200 0 255)
	(text-font @*font* 22)
	(string->text (str (:id b)) 20 20))
      (catch Exception e
	 ;; (println "shitty: " [:id (:id b) :pos (:pos b)])
	))))


(defn draw-button [b]
  (let [now (.getTotalMilliseconds (tuio/tuio-time))
	start (:start (:etc b))
	body (get-body (:id b))
	[r1 r2] (:size body)]
    (no-stroke)
    (ellipse-mode RADIUS)
    (with-translation (screen (:pos body))
      (if (< (- now start)
	     (threshold :make-node))
	(do
	  (fill 40 80 230 155)
	  (ellipse 0 0 (- r1 15) (- r2 15)))
	(do
	  (fill 50 90 240 205)
	  (ellipse 0 0 r1 r2)))
      (when ((:bounds-fn body) body @*mouse-pos*)
	(fill 50 200 10 227)
	(ellipse 0 0 5 5)))))

(defn in-circle? [cir pos]
  (let [body (get-body (:id cir))
	[h k] (screen (:pos body))
	[x y] (screen pos)
	rsqd (Math/pow (first (:size body)) 2)]
    (< (+ (Math/pow (- x h) 2)
	  (Math/pow (- y k) 2))
       rsqd)))

(defn world-at [pos]
  (first (filter #((:bounds-fn %) % pos) (entities @*world*))))

(defn init-tuio-client []
  (try (tuio/connect! *tuio*)
       (doto *tuio*
	 ;; (tuio/on-add-object! obj
	 ;;   (println "add object"))
	 
	 ;; (tuio/on-update-object! obj
	 ;;   (println "update object"))
	   
	 ;; (tuio/on-remove-object! obj
	 ;;   (println "remove object"))


	 ;; (defmacro with-tuio [c]
	 ;;   `(let []
	 ;;      ))

	 (tuio/on-add-cursor!
	  curs
	  (let [id  (.getSessionID curs)
		pos [(.getX curs) (.getY curs)]
		vel [(.getXSpeed curs) (.getYSpeed curs)]
		now (.getTotalMilliseconds (tuio/tuio-time))
		moving? (.isMoving curs)
		thing (world-at pos)]
	    (dosync
	     (alter *world* update-in [:crsr id] merge {:id id
							:pos pos
							:etc {:vel vel
							      :moving? moving?
							      :start now
							      :holding id}})
	     (alter *world* update-in [:bodies id] merge {:id id
							  :pos pos
							  :size [10 10]
							  :bounds-fn in-circle?})

	     (alter *world* update-in [:visuals id] merge {:id id
							   :draw draw-char ;; draw-button
							   :etc {:start now}}))
	    (comm/enqueue *tch* {:crsr id
				 :pos pos
				 :vel vel
				 :moving? moving?
				 :time now
				 :event :add})))

	 (tuio/on-update-cursor!
	  curs
	  (let [id  (.getSessionID curs)
		pos [(.getX curs) (.getY curs)]
		vel [(.getXSpeed curs)  (.getYSpeed curs)]
		now (.getTotalMilliseconds (tuio/tuio-time))
		start (-> @*world* :crsr (get id) :etc :start)
		moving? (.isMoving curs)]
	    (when 
	      (dosync
	       (alter *world* update-in [:crsr id] merge {:pos pos :etc {:vel vel :moving? moving? :start start}})
	       (alter *world* update-in [:bodies id] merge {:pos pos}))

	      (comm/enqueue *tch* {:crsr id
				   :pos pos
				   :time now
				   :event :update}))))

	 (tuio/on-remove-cursor!
	  curs
	  (let [id (.getSessionID curs)
		now (.getTotalMilliseconds (tuio/tuio-time))
		start (-> @*world* :crsr (get id) :etc :start)] ;; could lead to inconsistent state error?
	    (dosync
	     (alter *world* dissoc-in [:crsr id])
	     (when (< (- now start) (threshold :make-node))
	       (alter *world* dissoc-in [:visuals id])
	       (alter *world* dissoc-in [:bodies id])))
	    (comm/enqueue *tch* {:crsr id
				 :time now
				 :event :remove})))
	 
	 ;; (tuio/on-refresh!
	 ;;  ;; (dosync (alter *world* assoc :tuio-time ))
	 ;;  )
	 )
  (catch Exception e (.printStackTrace e))))

 (defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (comm/close *tch*)
   (println "tuio client disconnected"))


(defn mouse-moved [evt]
  (let [x (/ (.getX evt) *screen-width*)
	y (/ (.getY evt) *screen-height*)]
    (dosync (alter *world* assoc-in [:crsr :mouse :pos] [x y])
	    (reset! *mouse-pos* [x y]))))

(defn setup
   "executes once."
   []
   (smooth)
   (framerate *framerate*)
   (reset! *time* 0)
   (reset! *font* (load-font "Monaco-48.vlw"))
   (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
   (init-tuio-client))

(defapplet creativity
   :title "Creativity"
   :setup setup
   :draw draw
   :size [*screen-width*  *screen-height*]
   :mouse-moved mouse-moved
   ;; :mouse-pressed mouse-pressed
   ;; :mouse-released mouse-released
   ;; :mouse-dragged mouse-dragged
   ;; :mouse-entered mouse-entered
   ;; :mouse-exited mouse-released
   ;; :key-pressed key-pressed
   ;; :key-released key-released
   )


(run creativity :interactive)
;; (do (kill-tuio) (stop creativity))