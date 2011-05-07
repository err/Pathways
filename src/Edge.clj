(defrecord Edge [id source sink]
  java.lang.Comparable
  (compareTo [this that] (compare id (:id that))))