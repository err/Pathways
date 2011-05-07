;; UNDERSCORES, not hyphens
(ns first-touch.core
  (:import [TUIO TuioClient]
       [TUIO TuioContainer]
       [TUIO TuioCursor]
       [TUIO TuioListener]
       [TUIO TuioObject]
       [TUIO TuioPoint]
       [TUIO TuioTime]))

(defmacro on-add-object! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] (~cb)
               )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-update-object! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject
	 [d#]
	 ;; (println "\nx: " (.getScreenX d# 1024)
	 ;; 	  " y: " (.getScreenY d# 768))
	 )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-remove-object! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] (~cb) )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-add-cursor! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] (~cb) )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-update-cursor! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor
	 [d#] (println "~~*\nx: " (.getScreenX d# 1024)
		       " y: " (.getScreenY d# 768)
		       " ID: " (.getCursorID d#))
	 )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-remove-cursor! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] (~cb))
        (refresh [d#]))))

(defmacro on-refresh! [client cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#]  )
        (refresh [d#] (~cb) ))))

(defn remove-all-listeners! [c]
  (.removeAllTuioListeners c))

(defn client  
  ([] (TuioClient.))
  ([port] (TuioClient. port)))

(defn connect!
  [c] (.connect c))

(defn disconnect!
  [c] (.disconnect c))

(defn connected? [c]
  (.isConnected? c))

(defn cursors [c]
  (.getTuioCursors c))

(defn objects [c]
  (.getTuioObjects c))

(defn cursor
  "Gets a TUIO cursor on ID."
  [c id]
  (first (filter (fn [c]
                   (let [id2 (.getCursorID c)]
                     (= id2 id)))
                 (cursors c))))


(comment
  (def c (client))

  (doto c
    (on-add-object! #(println "add object"))
    (on-update-object! #(println "update object"))
    (on-remove-object! #(println "remove object"))
    (on-add-cursor! #(println "add cursor"))
    (on-update-cursor! #(println "\nx: " (.getScreenX d# 1024)
				 " y: " (.getScreenY d# 768)) ;; (println "update cursor")
	       )
    (on-remove-cursor! #(println "remove cursor"))
    (on-refresh! #(println "refresh"))
    )

  (connect! c))

