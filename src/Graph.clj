(defrecord Graph [nodes edges]
  java.lang.Comparable
  (compareTo [this that]
  	     (let [result (compare (sort nodes) (sort (:nodes that)))]
  	       (if (zero? result)
  		 (compare (sort edges) (sort (:edges that)))))))