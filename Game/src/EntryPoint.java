
public class EntryPoint extends Point {

	EntryPoint(double xcoord, double ycoord){
	    super(xcoord, ycoord);
	    pointCount += 1; 
	}
	
	EntryPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("EntryPoint " + pointRef + " set:(" + x + "," + y +").");
	}
	


}
