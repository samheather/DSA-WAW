package logicClasses;

public class Waypoint extends Point {

	public Waypoint(double xcoord, double ycoord) {
	    super(xcoord, ycoord);
	   
	}
	
	Waypoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    
	    System.out.println("Waypoint " + pointRef + " set:(" + x + "," + y +").");
	}
	


}
