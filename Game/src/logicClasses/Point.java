package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;



public class Point {

    protected double x;
    protected double y;
    protected String pointRef;
    

	
	// CONSTRUCTORS
    

    //Point Constructor that also takes pointRef string, more commonly used.
    public Point(double xcoord, double ycoord, String name){
    x = xcoord; 
    y = ycoord;
    pointRef = name;
    System.out.println("Point " + pointRef + " set:(" + x + "," + y +").");
    }
    public Point(double xcoord, double ycoord) {

        x = xcoord;
        y = ycoord;
        
     }
    
    
    
    // MUTATORS AND ACCESSORS
    
    public double getX() {
    	return this.x;
    }
    
    public double getY() {
    	return this.y;
    }
    
    public void setX(double newX) {
    	this.x = newX;
    }
    
    public void setY(double newY) {
    	this.y = newY;
    }
    
    public String getPointRef() {
    	return this.pointRef;
    }
    
    public void setPointRef(String pointRef) {
  		this.pointRef = pointRef;
  	}
    


    
    public boolean equals(Point point){
    	if(point instanceof Point){
    		if ((point.getX() ==  this.x) && (point.getY() == this.y)){
    			return true;
    		}
    	}
    	return false;
   }




}
