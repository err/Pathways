(ns birds.mutate
  (:require [clojure.contrib.combinatorics :as combo]))


;; song-types are [[1 3 5] [5 6 7]]
(defn mutate-song-type [notes & op]
  (let [op (or (first op) :reverse-segment)]
    (case op
	  :reverse-segment (reverse-segment notes)
	  :add-repeat      (add-repeat notes))))
