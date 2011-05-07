(defrecord Node [id type in out]
  java.lang.Comparable
  (compareTo [this that] (compare id (:id that))))