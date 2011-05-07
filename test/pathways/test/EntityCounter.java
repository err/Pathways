// package edu.gatech.synlab;

public class EntityCounter{
    
    private static EntityCounter entityCounter;
    private int count;

    private EntityCounter(){
	this.count = 0;
    }

    public static // synchronized
	EntityCounter getEntityCounter(){
	if (entityCounter == null){
	    entityCounter = new EntityCounter();
	}
	return entityCounter;
    }

    public int getCount(){
	return this.count;
    }

    //TODO: make atomic?
    public int newID(){
	int tmp = this.count;
	this.count++;
	return tmp;
    }

    public Object clone() throws CloneNotSupportedException {
	throw new CloneNotSupportedException();
    }

}