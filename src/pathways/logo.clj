(ns pathways.logo)

(defn- pad-hlpr [s amt]
  (if (even? amt)
    (apply str (concat
		(repeat (/ amt 2) \space)
		s
		(repeat (/ amt 2) \space)))
    (apply str (concat
		(repeat (Math/floor (/ amt 2)) \space)
		s
		(repeat (inc (Math/floor (/ amt 2))) \space)))))

(defn- pad "pad string w/ spaces so it is len chars long."
  [strn len]
  (let [extra (- len (count strn))]
    (if-not (pos? extra)
      (.substring (.trim strn) 0 len)
      (pad-hlpr strn extra))))

(defn print-logo
  ([] (print-logo "UNTITLED"))
  ([title]
     (let [title (if-not (string? title) (str title) title)]

       (dotimes [n 1]
	 (println "========================================"))
       (println "       BOREDOM ENTERTAINMENT PRESENTS") ;; (flush)
       (dotimes [n 1]
	 (println "========================================"))
       (println ";;;;;;;;;;;;;;;;;;;^;;;;;;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;;;;;;/.\\;;;;;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;;;;;/...\\;;;;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;;;;/.....\\;;;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;;;/.......\\;;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;;/.........\\;;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;;/...........\\;;;;;;;;;;;;;")
       (println ";;;;;;;;;;;;/.............\\;;;;;;;;;;;;")
       (println ";;;;;;;;;;;/.......^.......\\;;;;;;;;;;;")
       (println ";;;;;;;;;;/......!/=\\!......\\;;;;;;;;;;")
       (println ";;;;;;;;;/......T.E.C.H......\\;;;;;;;;;")
       (println ";;;;;;;;|=====================|;;;;;;;;")
       (println ";;;;;;;;||~*~*~*._._._.*~*~*~||;;;;;;;;")
       (println ";;;;;;;;;\\\\     |_|-|_|     //;;;;;;;;;")
       (println ";;;;;;;;;;||    |_|-|_|    ||;;;;;;;;;;")
       (println ";;;;;;;;;;||    |_|-|_|    ||;;;;;;;;;;")
       (println ";;;;;;;;;;||               ||;;;;;;;;;;")
       (println ";;;;;;;;;;||               ||;;;;;;;;;;")
       (println ";;;;;;;;;;||      .;.      ||;;;;;;;;;;")
       (println ";;;;;;;;;;||     /...\\     ||;;;;;;;;;;")
       (println ";;;;;;;;;;||    /.....\\    ||;;;;;;;;;;")
       (println ";;;;;;;;;;||   /.......\\   ||;;;;;;;;;;")
       (println ";;;;;;;;;;||  /..Eric...\\  ||;;;;;;;;;;")
       (println "_________.||_/..Caspary..\\_||._________")
       (println "=========:|/=|-----+-----|=\\|:=========")
       (println "I_I_I_I_I:|  |.---. .---.|  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  | |o|   |o| |  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  | |_| | |_| |  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  |   .___.   |  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  |  //   \\\\  |  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  |" (pad title 9)  "|  |:I_I_I_I_I")
       (println "I_I_I_I_I:|  |  \\\\___//  |  |:I_I_I_I_I")
       (println "I_I_I_I_I:\\\\ |           | //:I_I_I_I_I")
       (println "I_I_I_I_I: \\\\|_~_~_~_~_~_|// :I_I_I_I_I")
       (println "I_I_I_I_I:  \\\\           //  :I_I_I_I_I")
       (println "I_I_I_I_I:   \\\\~_~_~_~_~//   :I_I_I_I_I")
       (println "I_I_I_I_I:    \\\\       //    :I_I_I_I_I")
       (println "I_I_I_I_I:     \\\\_~_~_//     :I_I_I_I_I")
       (println "I_I_I_I_I:      \\\\ . //      :I_I_I_I_I")
       (println "I_I_I_I_I:       \\\\_//       :I_I_I_I_I")
       
       (dotimes [n 1]
	 (println "========================================"))
       (println "(C)opyright Boredom Entertainment, 2011.")
       (println "           All rights reserved.         ")
       (dotimes [n 1]
	 (println "========================================"))))) 