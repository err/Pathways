(ns pathways.util)

(defn immigrate
 "Create a public var in this namespace for each public var in the
 namespaces named by ns-names. The created vars have the same name, value
 and metadata as the original except that their :ns metadata value is this
 namespace."
 [& ns-names]
 (doseq [ns ns-names]
   (doseq [[sym var] (ns-publics ns)]
     (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
       (if (.isBound var)
         (intern *ns* sym (var-get var))
         (intern *ns* sym))))))



(defn vec-add [a b]
  [(+ (first  a) (first  b)) (+ (second a) (second b))])

(defn vec-sub [a b]
  [(- (first  a) (first  b)) (- (second a) (second b))])

;; (defn vec-mult [a b]
;;   )



;; (defmacro shape [type & body]
;;   `(do
;;      (begin-shape ~type)
;;      ~@(map (fn [[x# y#]] `(vertex ~x# ~y#)) body)
;;      (end-shape CLOSE)))