//package edu.gatech.synlab;

import java.util.ArrayList;
// import /* edu.gatech.synlab. */ NodeType;
// import /* edu.gatech.synlab. */ EntityCounter;

public class Node {
    //Member variables
    private int id;
    private ArrayList<Integer> in;
    private ArrayList<Integer> out;
    private NodeType type;

    //Constructors
    //TODO: call setter methods in constructors
    public Node(){
	this.id   = -1;
	this.in   = new ArrayList<Integer>();
	this.out  = new ArrayList<Integer>();
	this.type = NodeType.NO_TYPE;
    }

    public Node(ArrayList<Integer> in, ArrayList<Integer> out, NodeType type){
	EntityCounter count = EntityCounter.getEntityCounter();
	this.id   = count.newID();
	this.in   = in;
	this.out  = out;
	this.type = type;
    }

    public Node(int id, ArrayList<Integer> in, ArrayList<Integer> out, NodeType type){
	this.id   = id;
	this.in   = in;
	this.out  = out;
	this.type = type;
    }

    //Getters
    public int getID(){
	return this.id;
    }

    public ArrayList<Integer> getIncomingEdgeIDs(){
	return this.in;
    }

    public ArrayList<Integer> getOutgoingEdgeIDs(){
	return this.out;
    }

    public NodeType getNodeType(){
	return this.type;
    }


    //Setters
    public void setID(int id){
	this.id = id;
    }

    public void setIncomingEdges(ArrayList<Integer> in){
	this.in = in;
    }

    public void setOutgoingEdges(ArrayList<Integer> out){
	this.out = out;
    }

    public void setNodeType(NodeType nt){
	this.type = nt;
    }

    public void addIncomingEdge(int id){
	// avoid duplicates
	for (Integer edgeID : this.in){
	    if (id == edgeID.intValue()){
		System.err.println("Attempt to add duplicate incoming edge (ID "+id+") to node (ID "+this.id+").");
		return;
	    }
	}
	if (!this.in.add(new Integer(id))){
	    System.err.println("Attempt to add incoming edge (ID "+id+") to node (ID "+this.id+") FAILED.");
	}
    }

    public void addOutgoingEdge(int id){
	// avoid duplicates
	for (Integer edgeID : this.out){
	    if (id == edgeID.intValue()){
		System.err.println("Attempt to add duplicate outgoing edge (ID "+id+") to node (ID "+this.id+").");
		return;
	    }
	}
	if (!this.out.add(new Integer(id))){
	    System.err.println("Attempt to add outgoing edge (ID "+id+") to node (ID "+this.id+") FAILED.");
	}
    }

    public void removeIncomingEdge(int id){
	Integer tmp;
	for (int edgeID = 0; edgeID < this.in.size(); edgeID++){
	    tmp = this.in.get(edgeID);
	    if ( tmp != null && tmp.intValue() == id){
		this.in.remove(edgeID);
		//return; //should we continue through list? 
	    }
	}
    }

    public void removeOutgoingEdge(int id){
	Integer tmp;
	for (int edgeID = 0; edgeID < this.out.size(); edgeID++){
	    tmp = this.out.get(edgeID);
	    if ( tmp != null && tmp.intValue() == id){
		this.out.remove(edgeID);
		//return; maybeh? see above
	    }
	}
    }

    public void removeEdge(int id){
	removeIncomingEdge(id);
	removeOutgoingEdge(id);
    } 
    
    public String toString(){
	String tmp = new String("");
	tmp += ("[ :id "+getID()
		+" :type "+getNodeType()
		+" :inEdges [ ");
	for (Integer i : getIncomingEdgeIDs()){
	    tmp += (i+" ");
	}
	tmp += "]";  // closes inEdges
	tmp += " :outEdges [ ";
	for (Integer i : getOutgoingEdgeIDs()){
	    tmp += (i+" ");
	}
	tmp += "]";  // closes outEdges
	tmp += "]";// closes Node

	return tmp;
    }
}


