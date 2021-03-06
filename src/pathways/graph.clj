(ns pathways.graph
  (:use [pathways.util]
	[pathways.event]
	[clojure.contrib.graph]))


;;;; TODO
;;
;; - TEST conj-node, conj-edge, disj-edge
;;
;; - Remove dependence on global *graph*
;;   \_in the accessor/add/remove fuctions
;;
;; - implement Neighbors fn
;;
;;
;; - EVENTUALLY -- settle the ID dilemna
;;   \_currently using ints and a global counter
;;    \_could use "unique-ids" which vary by datatype.
;;
;;;;


;;;; Globals 
(def *graph*   (ref {}))
(defn graph [] @*graph*)
(defn nodes [] (get @*graph* :nodes))
(defn edges [] (get @*graph* :edges))


;;; Data Structures
(defrecord Graph [nodes edges]
  java.lang.Comparable
  (compareTo [this that]
  	     (let [result (compare (sort nodes) (sort (:nodes that)))]
  	       (if (zero? result)
  		 (compare (sort edges) (sort (:edges that)))))))

(defrecord Node [id in out etc]
  java.lang.Comparable
  (compareTo [this that] (compare id (:id that))))

(defrecord Edge [id src snk]
  java.lang.Comparable
  (compareTo [this that] (compare id (:id that))))


;;; Constructors
(defn make-graph
  "A graph is a nested hash-map with the outer map keyed by :nodes and :edges,
    and the inner maps keyed by node-ids and edge-ids.

   If supplied no arguments, creates the empty graph - {:nodes {} :edges {}}.
   Otherwise, expects two seqable arguments of Nodes and Edges."
  ([]
     (Graph. (sorted-map) (sorted-map)))
  ([nodes edges]
     (Graph. (apply sorted-map (interleave (map :id nodes) nodes))
	     (apply sorted-map (interleave (map :id edges) edges)))))

(defn make-node
  "One  arg - Creates a new Node with no in/out edges.
   Mult args- In/Out should be vectors of integers representing edge-ids."
  ([id] (Node. id #{} #{} {}))
  ([id in out etc]
     (let [in  (if (not (vector?  in)) (vector  in)  in)
	   out (if (not (vector? out)) (vector out) out)]
       (Node. id (set in) (set out) {}))))

(defn make-edge
  "Creates an new Edge from the provided symbols/ints for id, source, and sink.
    NOTICE: If no src/sink specified, default to nil.
    __TODO: Consider letting src/sink default to  -1.
    __TODO: Then again, an Edge cannot exist without a src and sink" 
  ([id]
     (Edge. id -1 -1))
  ([id src snk]
     (Edge. id src snk)))



;;; Obtaining Nodes/Edges from IDs
(defn node
  "Returns the Node with the given id."
  ([graph id]
     (get-in graph [:nodes id])))

(defn edge
  "Returns the Edge with the given id."
  ([graph id]
     (get-in graph [:edges id])))

(defn entity
  "Returns the graph entity [either node OR edge] with the given id."
  ([graph id]
     (or (get-in graph [:nodes id])
	 (get-in graph [:edges id]))))

(defn source-id
  "Returns the id of the source node for the edge with the provided id"
  ([graph edge-id]
     (get-in graph [:edges edge-id :src])))

(defn sink-id
  "Returns the id of the sink node for the edge with the provided id"
  ([graph edge-id]
     (get-in graph [:edges edge-id :snk])))

(defn source
  "Returns the Node that is the source node of the edge with the provided id"  
  ([graph edge-id]
     (let [id (source-id graph edge-id)]
       (entity graph id))))

(defn sink-node
  "Returns the Node that is the sink node of the edge with the provided id"
  ([graph edge-id]
     (let [id (sink-id graph edge-id)]
       (entity graph id))))

(defn inbound-edges
  "Returns the set of all edges whose sink is the node specified by node-id."
  ([graph node-id]
     (get-in graph [:nodes node-id :in])))

(defn outbound-edges
  "Returns the set of all edges leading out from the node specified by node-id."
  ([graph node-id]
     (get-in graph [:nodes node-id :out])))



;;; Adding/Removing Elements from a Graph
(defn add-node-edge
  "conj's edge-id to the set of (in/out) node referenced by node-id.
   Must specify the direction as either :in or :out"
  [graph node-id edge-id dir]
  (update-in graph [:nodes node-id dir] conj edge-id))

(defn remove-node-edge
  [graph node-id edge-id dir]
  (if (not (contains? (get graph :nodes) node-id))
    graph
    (update-in graph [:nodes node-id dir] disj edge-id)))

;;TODO - add edges as necessary (also make sure they're valid)
(defn conj-node
  [graph node]
  (let [node-id (:id node)]
    (-> graph
	(assoc-in [:nodes node-id] node))))

;;TODO - ensure that the src and sink nodes are actually in the graph. (and if not, add them?)
(defn conj-edge
  [graph edge]
  (let [edge-id (:id edge)
	src-id (:src edge)
	snk-id (:snk edge)]
    (-> graph
	(assoc-in [:edges edge-id] edge)    ; add to :edges
	(add-node-edge src-id edge-id :out) ; add to src
	(add-node-edge snk-id edge-id :in)) ; add to snk
    ))


(defn disj-edge
  "Removes the edge referenced by edge-id. Returns the new graph."
  ([graph edge-id]
     (let [src-id (source-id graph edge-id)
	   snk-id (sink-id   graph edge-id)]
       (-> graph
	   (update-in [:edges] dissoc edge-id)     ; rem from :edges
	   (remove-node-edge src-id edge-id :out)  ; rem from src
	   (remove-node-edge snk-id edge-id  :in)) ; rem from snk
       )))


(defn disj-node
  "Provided a node-id, removes node and it's reliant edges from graph."
  ([graph node-id]
     (let [rem-edges (into (inbound-edges graph node-id) (outbound-edges graph node-id))
	   new-graph (update-in graph [:nodes] dissoc node-id)]
       (reduce #(disj-edge %1 %2) new-graph rem-edges))))

(defn neighbor-molecules [graph id])

(defn neighbor-reactions [graph id])



;;; Init fns [PATHWAYS-specific]
(defn make-test-graph []
  (make-graph ))

(defn init-graph [g]
  (dosync (ref-set *graph* g)))

(init-graph (make-test-graph))