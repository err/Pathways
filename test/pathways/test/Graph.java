//package edu.gatech.synlab;

import java.util.ArrayList;

// import /* edu.gatech.synlab. */ Edge;
// import /* edu.gatech.synlab. */ Node;
// import /* edu.gatech.synlab. */ NodeType;

public class Graph {

    //Member variables
    private ArrayList<Node> nodes;
    private ArrayList<Edge> edges;

    //Constructors
    public Graph(){
	nodes = new ArrayList<Node>();
	edges = new ArrayList<Edge>();
    }

    public Graph(ArrayList<Node> nodes, ArrayList<Edge> edges){
	this.nodes = nodes;
	this.edges = edges;
    }

    //Getters
    public ArrayList<Node> getNodes(){
	return this.nodes;
    }

    public ArrayList<Edge> getEdges(){
	return this.edges;
    }

    public Node getNode(int id){
	for (int i = 0; i < nodes.size(); ++i){
	    if (id == nodes.get(i).getID()){
		return nodes.get(i);
	    }
	}
	return null;
    }
    
    public Edge getEdge(int id){
	for (int i = 0; i < edges.size(); ++i){
	    if (id == edges.get(i).getID()){
		return edges.get(i);
	    }
	}
	return null;
    }
    

    /**
     *   Returns either an edge or a node.
     */
    public Object getEntity(int id){
	Object obj = this.getNode(id);
	if (obj == null){
	    obj = this.getEdge(id);
	}
	return obj;
    }


    public ArrayList<Edge> getIncomingEdges(int nodeID){
	// declarations
	Node node;
	ArrayList<Integer> inEdgeIDs;
	ArrayList<Edge> inEdges = new ArrayList<Edge>();

	// logic
	node = getNode(nodeID);
	if (node != null){
	    inEdgeIDs = node.getIncomingEdgeIDs();  // get IDs
	    for (Integer i : inEdgeIDs){            
		inEdges.add(getEdge(i));            // get Edges
	    }
	}
	return inEdges;                             // returns null if nodeID does not identify a node in Graph
    }


    public ArrayList<Edge> getOutgoingEdges(int nodeID){
	// declarations
	Node node;
	ArrayList<Integer> outEdgeIDs;
	ArrayList<Edge> outEdges = new ArrayList<Edge>();
	
	// logic
	node = getNode(nodeID);
	if (node != null){
	    outEdgeIDs = node.getOutgoingEdgeIDs();  // get IDs
	    for (Integer i : outEdgeIDs){            
		outEdges.add(getEdge(i));             // get Edges
	    }
	}
	return outEdges;                             // returns null if nodeID does not identify a node in Graph
    }


    //Add Node
    public void addNode(Node node){
	int id = node.getID();
	Object obj = getEntity(id);
	// ensure that nodeID does not reference another entity in the Graph
	if (obj == null){
	    nodes.add(node);
	}
	else{
	    System.err.println("Attempt to add node failed. (ID "+id+") already present in graph.");
	}
    }
    
    //Remove Node
    public void removeNode(int nodeID){
	Node node = getNode(nodeID);
	if (node != null){
	    removeNode(node);
	}
	else{
	    System.err.println("Attempted to remove node (ID "+nodeID+"). Node not present in graph."); 
	}
    }

    public void removeNode(Node node){
	// gather connected edges for removal
	ArrayList<Integer> removeEdges = new ArrayList<Integer>();
	removeEdges.addAll(node.getIncomingEdgeIDs());
	removeEdges.addAll(node.getOutgoingEdgeIDs());

	int id = node.getID();
	// remove node from graph.nodes
	for (int i = 0; i < nodes.size(); ++i){
	    if (id == nodes.get(i).getID()){
		nodes.remove(i);
		break; //exits for loop, I think.
	    }
	}

	// remove edges from graph.edges
	for (int i = 0; i < removeEdges.size(); i++){
	    removeEdge(removeEdges.get(i));
	}
    }


    //Add Edge
    public void addEdge(Edge edge){
	int id = edge.getID();
	Object obj = getEntity(id);
	// ensure that edge.id does not reference another entity in the Graph
	if (obj == null){
	    edges.add(edge);                     //add to Graph.edges
	    int srcID = edge.getSourceID();
	    int snkID = edge.getSinkID();
	    getNode(srcID).addOutgoingEdge(id);  //add to srcNode.out
	    getNode(snkID).addIncomingEdge(id);  //add to sinkNode.in
	    
	}
	else{
	    System.err.println("Attempt to add node failed. (ID "+id+") already present in graph.");
	}

    }
    
    //Remove Edge
    public void removeEdge(int edgeID){
	Edge edge = getEdge(edgeID);
	if (edge != null){
	    removeEdge(edge);
	}
	else{
	    System.err.println("Attempted to remove edge (ID "+edgeID+"). Edge not present in graph.");
	}
    }

    public void removeEdge(Edge edge){
	int id = edge.getID();
	// remove edge from graph.edges
	for (int i = 0; i < edges.size(); ++i){
	    if (id == edges.get(i).getID()){
		edges.remove(i);
		break; //exits for loop, I think.
	    }
	}

	Node tmpNode;
	// remove edge from source node
	tmpNode = getNode(edge.getSourceID());
	if (tmpNode != null){
	    tmpNode.removeOutgoingEdge(id);
	}

	// remove edge from sink node
	tmpNode = getNode(edge.getSinkID());
	if (tmpNode != null){
	    tmpNode.removeIncomingEdge(id);
	}

    }
        

    //Get Neighbors  
    public ArrayList<Node> getNeighborNodes(int nodeID){
	Node node = getNode(nodeID);
	ArrayList<Node> neighbors = new ArrayList<Node>();
	if (node != null){
	    for (Integer i : node.getOutgoingEdgeIDs()){
		Node sinkNode = getNode(getEdge(i).getSinkID()); //whoa
		if (sinkNode != null){
		    neighbors.add(sinkNode);
		}
	    }
	}
	else{
	    System.err.println("Node (ID "+nodeID+") has no neighbors.");
	}
	return neighbors;
    }

    public ArrayList<Node> getNeighborReactions(int nodeID){
	ArrayList<Node> neighbors = getNeighborNodes(nodeID);
	Node tmpNode;
	for (int i = 0; i < neighbors.size(); i++){
	    tmpNode = neighbors.get(i);
	    if (tmpNode.getNodeType() != NodeType.REACTION){
		neighbors.remove(i);
	    }
	}
	return neighbors;
    }

    public ArrayList<Node> getNeighborMolecules(int nodeID){
	ArrayList<Node> reactionNeighbors = getNeighborReactions(nodeID);
	ArrayList<Node> result = new ArrayList<Node>();
	for (Node reaction : reactionNeighbors){
	    ArrayList<Integer> reactionOutEdgeIDs = reaction.getOutgoingEdgeIDs();
	    if (reactionOutEdgeIDs.size() != 1){
		//WTF do we do if a reaction has two outbound edges?
		System.err.println("ReactionNode has multiple outgoing Edges!"
				   +"(nodeID"+reaction.getID()+".");
	    }
	    else{
		Edge sinkEdge   = getEdge(reactionOutEdgeIDs.get(0));
		int  sinkNodeID = sinkEdge.getSinkID();
		Node sinkNode   = getNode(sinkNodeID);
		if (sinkNode.getNodeType() == NodeType.MOLECULE){
		    result.add(sinkNode);
		}
	    }
	}
	return result;
    }

    //returns an int representing the type of connection between nodeA and nodeB
    // 0 : no edges
    // 1 : edge  from A->B
    // 2 : edge  from B->A
    // 3 : edges from A->B , B->A
    //
    // [This naive implementation is oblivious to multiple direct connections between the nodes.]
    private int connectionType(int nodeA, int nodeB){
	ArrayList<Node> aNeighbors = getNeighborMolecules(nodeA);
	ArrayList<Node> bNeighbors = getNeighborMolecules(nodeB);
	int numEdges = 0;
	for (Node n : aNeighbors){
	    if (n.getID() == nodeB){
		numEdges = 1;
	    }
	}
	for (Node n : bNeighbors){
	    if (n.getID() == nodeA){
		if (numEdges == 1){ //if already  A->B, set to 3 (indicates A<->B)
		    numEdges = 3;   
		}
		else{
		    numEdges = 2;
		};
	    }
	}
	System.err.println("NumEdges between (node "+nodeA+") and (node "+nodeB+") -> "+numEdges);
	return numEdges;
    }

    public boolean areNeighbors(int nodeA, int nodeB){
	return (connectionType(nodeA, nodeB) > 0);
    }

    public boolean edgeFrom(int nodeA, int nodeB){
	int retVal = connectionType(nodeA, nodeB);
	return (retVal == 1 || retVal == 3);
    }

    //Setters
    public void setNodes(ArrayList<Node> nodes){
	this.nodes = nodes;
    }

    public void setEdges(ArrayList<Edge> edges){
	this.edges = edges;
    }

    //TODO
    public void setNode(int id, Node node){
    }

    public void setEdge(int id, Edge edge){
    }

    //Printing the graph
    public String toString(){
	String tmp = new String("~~GRAPH~~\n");
	tmp += "Nodes: [\n";
	for (Node n : nodes){
	    tmp += n + "\n";
	}
	tmp += "]\n";
	tmp += "Edges: [\n";
	for (Edge e : edges){
	    tmp += e + "\n";
	}
	tmp += "]\n";
	return tmp;
    }

}