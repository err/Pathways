(defproject io.boredom/pathways "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [
                 ;;
                  [org.clojure/clojure "1.9.0"]

                 ;; combinatorics
                 [org.clojure/math.combinatorics "0.1.4"]

                 ;; Quil
		 ;; [org.clojars.automata/rosado.processing "1.1.0"]
                 [quil "2.7.1"]

                 ;; ;; Touch I/O
                 [clj-tuio "0.0.4-SNAPSHOT"]

                 ;; Sounds
                 [overtone "0.10.3"]
                 [leipzig "0.10.0"]

                 ;; Stats
                 [incanter "1.5.7"]


                 ;; UDP Data
		 ;; [aleph ]
		 ;; [gloss ]

                 ;; processing opengl stuff
		 ;; [net.java.dev/gluegen-rt "git"]
		 ;; [jogl2 "0.1.0"]
		 ]
  ;; :main pathways.core

  :jvm-opts ["-Xms1g" "-Xmx6g" "-XX:+UseConcMarkSweepGC"])



    ;; never do this
    (require 'cemerick.pomegranate.aether)
    (cemerick.pomegranate.aether/register-wagon-factory!
     "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))
