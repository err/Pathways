(ns creative.core
  (:use [clojure.contrib.math :only [floor]]
	[rosado.processing    :exclude [cursor]]
	[rosado.processing.applet]
	[pathways.util]
	[pathways.event])
  ;; (:require [incanter [core :as incanter] [charts :as charts]] )
  (:import (java.awt MouseInfo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;__GLOBALS__;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   DIMENSIONS                                                               ;;
(def *screen-width*        1680)                                              ;;
(def *screen-height*       1050)                                              ;;
;;                                                                            ;;
;;   FONTS                                                                    ;;
(def *font*          (atom nil))                                              ;;
;;                                                                            ;;
;;   TIME                                                                     ;;
(def *framerate*             60)                                              ;;
(def *time*            (atom 0))                                              ;;
(def *running?*    (atom false))                                              ;;
;;                                                                            ;;
;;   MOUSE                                                                    ;;
(def *mouse-pos*   (atom [0 0]))                                              ;;
(def *mouse-button*  (atom nil))                                              ;;
(defn mouse-x []  (first @*mouse-pos*))                                       ;;
(defn mouse-y [] (second @*mouse-pos*))                                       ;;
;;                                                                            ;;
;;   WORLD STATE                                                              ;;
(def *world*                                                                  ;;
     (ref {:time 0				                              ;;
	   :crsr {:mouse {:pos [0 0]		                              ;;
			  :dwn false}}		                              ;;
	   :elts {}}))				                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn setup
   "executes once."
   []
   
   (smooth)
   (framerate *framerate*)
   (reset! *time* 0)
   (reset! *font* (load-font "Monaco-48.vlw")))


(defn draw []
  )



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


 (defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (comm/close *tuio-ch*)
   (println "tuio client disconnected"))


(defapplet creativity
   :title "Creativity"
   :setup setup
   ;; :draw draw
   :size [*screen-width*  *screen-height*]
   ;; :mouse-moved mouse-moved
   ;; :mouse-pressed mouse-pressed
   ;; :mouse-released mouse-released
   ;; :mouse-dragged mouse-dragged
   ;; :mouse-entered mouse-entered
   ;; :mouse-exited mouse-released
   ;; :key-pressed key-pressed
   ;; :key-released key-released
   )


;; (run creativity :interactive)
;; (stop creativity)