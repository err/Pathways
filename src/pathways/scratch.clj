(ns grokboard.core
  (:use [grokboard.mass]
	[grokboard.utils]
	[grokboard.counters]
	[rosado.processing.applet]
	[rosado.processing :exclude [cursor]])
  (:require [tuio]
	    [clojure.contrib.graph :as grph]))

;;;; ;;;; ;;;;;;;;;; ;; ;;
;;;; Data Structures ;; ;;
;;
;; MASS
;; {:id
;;  :mass
;;  :pos
;;  :vel
;;  :acc
;;  :frc }
;; -------
;;
;; FORCE
;; {:id
;;  :born    ;timestep when the force was born
;;  :alive?  (fn [world])
;;  :weight  (fn [world])
;;  :step    (fn [world])
;;  :acc     ;[ax ay]
;;                       }
;; -------
;;
;; SEXP
;; {:id     
;;  :form   ;literal code
;;  :parent ;id of prev sexp
;;  :childs ;id of next sexp
;; 	                  }
;; -------
;;
;; CURSOR [TUIOCursor, mouse, OpenCV finger blobs]
;; {:id 
;;  :pos 
;;  :vel 
;;  :path } ;need something for holding/clicking/dragging/releasing/hovering
;;
;; -------
;;
;; WORLD
;; {:time
;;  :forms
;;  :masses
;;  :forces 
;;  ;:components }

;;
;;;;; /end Data Structures ;; ;;
;;;;; ;;;; ;;;; ;;;;;;;;;; ;; ;;



;;; Globals

(def *world*     (ref {}))
(def *dt*             1.0)
(def *screen-width*  1900)
(def *screen-height* 1200)

(defn update []
  )

(defn draw-grid []
  (doseq [i (range) :while (< (* i 15) *screen-width*)]
    (with-translation [(* 15 i) 0]
      (stroke-weight 1)
      (stroke-float 55)
      (line 0 0 0 *screen-height*)))

  (doseq [j (range) :while (< (* j 15) *screen-height*)]
    (with-translation [0 (* 15 j)]
      (stroke-weight 1)
      (stroke-float 55)
      (line 0 0 *screen-width* 0))))

(defn draw  []
  (background 0)
  (draw-grid))

(defn init-world []
  (dosync
   (ref-set *world* {:time 0
		     :forms  
		     :masses 
		     :forces 
		     :components })))

(defn setup []
  (println ";;;;;;;;;;;;;;;;|GROKBOARD|;;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|_________|;;;;;;;;;;;;;;;;;")
  (init-world)
  ;; (init-tuio)
  (reset-counters))


(defapplet grok
  :title "Grokboard!"
  :setup setup
  :draw  draw
  :size  [*screen-width* *screen-height*]
  ;; :mouse-moved mouse-moved
  ;; :mouse-pressed mouse-pressed
  ;; :mouse-released mouse-released
  ;; :mouse-dragged mouse-dragged
  ;; :mouse-entered mouse-entered
  ;; :mouse-exited mouse-released
  ;; :key-pressed key-pressed
  ;; :key-released key-released
			       )


;(run grok :interactive)
;(stop grok)

;;TUIOUpdate---.
;;              \
;;               |
;;               |
;;             |/
;;;; The case for channels ;;;;
;;                           ;;
;; -  Gesture Recognition  - ;;
;; -  Collision Detection  - ;;
;; -  Force Application    - ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - id                                     ;;
;; - pos                                    ;;
;; - preferred-size                         ;;
;; - current-size  (derive a SCALE factor)  ;;
;; - render-fn                              ;;
;; - event-handlers                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gesture-Recognition ;;;;
;; - id ;;;;;;;;;;;;;;;;;;;;;
;; - pattern
;; - timeout mechanisms (don't timeouts vary from  pattern to pattern?)
;; - tracking
;; - ;;
;; - ;;
;; - ;;
;; - ;;






;; -
;; -
;; -
;; -
;; -
;; -




;; -
;; -
;; -
;; -
;; -
;; -
