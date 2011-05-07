//package edu.gatech.synlab;

import java.util.ArrayList;
// import /* edu.gatech.synlab. */ Edge;
// import /* edu.gatech.synlab. */ Node;
// import /* edu.gatech.synlab. */ NodeType;
// import /* edu.gatech.synlab. */ Graph;

public class GraphTest{

    public GraphTest(){

    }

    public static void main(String[] args){
	//Instantiate a Graph
	Graph g = new Graph();
	System.out.println("Graph: "+g);
	
	/*Add Nodes & Edges to Graph*/
	
	//Start with our Molecule Nodes
	ArrayList<Integer>  in0 = new ArrayList<Integer>();
	ArrayList<Integer> out0 = new ArrayList<Integer>(); //out0.add(6);
	g.addNode(new Node(0, in0, out0, NodeType.MOLECULE));
	
	ArrayList<Integer> in1  = new ArrayList<Integer>(); //in1.add(11);
	ArrayList<Integer> out1 = new ArrayList<Integer>(); //out1.add(8);
	g.addNode(new Node(1, in1, out1, NodeType.MOLECULE));
	
	ArrayList<Integer> in2  = new ArrayList<Integer>();   //in2.add(9);
	ArrayList<Integer> out2 = new ArrayList<Integer>(); //out2.add(10);
	g.addNode(new Node(2, in2, out2, NodeType.MOLECULE));
	
	//Now add Reaction Nodes
	ArrayList<Integer> in3  = new ArrayList<Integer>();   //in3.add(6);
	ArrayList<Integer> out3 = new ArrayList<Integer>();  //out3.add(7);
	g.addNode(new Node(3, in3, out3, NodeType.REACTION));
	
	ArrayList<Integer> in4  = new ArrayList<Integer>();
	//	in4.add(7); in4.add(8);
	ArrayList<Integer> out4 = new ArrayList<Integer>();
	//	out4.add(9);
	g.addNode(new Node(4, in4, out4, NodeType.REACTION));

	ArrayList<Integer> in5  = new ArrayList<Integer>();   //in5.add(10);
	ArrayList<Integer> out5 = new ArrayList<Integer>();  //out5.add(11);
	g.addNode(new Node(5, in5, out5, NodeType.REACTION));
	
	
	System.out.println("All nodes added.");
	System.out.println("Graph: "+g);
	
	//Now add Edges
	g.addEdge(new Edge(6,  0, 3));
	g.addEdge(new Edge(7,  3, 4));	
	g.addEdge(new Edge(8,  1, 4));
	g.addEdge(new Edge(9,  4, 2));	
	g.addEdge(new Edge(10, 2, 5));
	g.addEdge(new Edge(11, 5, 1));	
	
	System.out.println("All nodes and edges added.");
	System.out.println("Graph: "+g);


	//Print neighbors
	System.out.println("\n_~_~_~_~_~_~~_~_~_~_~_~");
	System.out.println("Neighbors: ");
	System.out.println("Molecule Neighbors for Molecule Node 0: "+g.getNeighborMolecules(0));
	System.out.println("Molecule Neighbors for Molecule Node 1: "+g.getNeighborMolecules(1));
	System.out.println("Molecule Neighbors for Molecule Node 2: "+g.getNeighborMolecules(2));
	System.out.println("_~_~_~_~_~_~~_~_~_~_~_~\n");

	System.out.println("\n_~_~_~_~_~_~~_~_~_~_~_~");
	System.out.println("Reaction Neighbors for Molecule Node 0: "+g.getNeighborReactions(0));
	System.out.println("Reaction Neighbors for Molecule Node 1: "+g.getNeighborReactions(1));
	System.out.println("Reaction Neighbors for Molecule Node 2: "+g.getNeighborReactions(2));
	System.out.println("_~_~_~_~_~_~~_~_~_~_~_~\n");

	//Reaching nodeB from nodeA
	System.out.println("+++++-REACHABILITY-+++++");
	System.out.println("edgeFrom (mol 0) to (mol 2)? "+g.edgeFrom(0,2));
    	System.out.println("edgeFrom (mol 0) to (rea 4)? "+g.edgeFrom(0,4));
	System.out.println("edgeFrom (mol 1) to (mol 2)? "+g.edgeFrom(1,2));
	
	//Remove Nodes
	int remID;
	
	//Remove Reaction Node
	remID = 3;
	g.removeNode(remID);
	System.out.println("Reaction Node "+remID+" removed.");
	System.out.println("Graph: "+g);
	
	//Remove Molecule Node
	remID = 0;
	g.removeNode(remID);
	System.out.println("Reaction Node "+remID+" removed.");
	System.out.println("Graph: "+g);
    }

}
    