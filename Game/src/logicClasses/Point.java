package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;



public class Point {

    //A "Point" object has a (X,Y) co-ordinate, representing a line in the
    //game airspace along the altitude axis. Values set individually as (xCoOrd, 
    // yCoOrd) double variables, initialised by default as 0,0.
    
    //If this helps:
    //String pointRef is the letter reference (Point A would be "A").
    //Class Var. static int PointCount tracks number of Points set up.
    protected double x;
    protected double y;
    protected String pointRef;
    Image next_waypoint_image, waypoint_image;
	protected static int pointCount = 0;
	
	// CONSTRUCTORS
    
    //Point Constructor take two doubles, for x then y.
    public Point(double xcoord, double ycoord) {
    x = xcoord;
    y = ycoord;
    pointCount += 1;

    }
    
    //[Not in use] Point Constructor that also takes pointRef string:
    Point(double xcoord, double ycoord, String name){
    x = xcoord; 
    y = ycoord;
    //pointRef = name;
    pointCount += 1;
    System.out.println("Point " + pointRef + " set:(" + x + "," + y +").");
    }
    
    //[Not in use] Constructor pointing out that X,Y has not been set.
    Point(){
    System.out.println("Warning: Constructor method for Point Object called, "
            + "point set as (0,0) with no point ref.");
    pointCount +=1;
    }
    
    
    // INIT, RENDER, UPDATE
    
    public void init(GameContainer gc) throws SlickException {
    	this.waypoint_image =new Image("res/graphics/graphics/waypoint.png"); 
        this.next_waypoint_image =  new Image("res/graphics/graphics/waypoint_next.png");

	}
    
    public void render(Graphics g, Airspace airspace) throws SlickException {
    	
        
   
    	
    	if(airspace.getControls().getFlight() !=null){
    		if (airspace.getControls().getFlight().getFlight_plan().getWaypoints().indexOf(this)==0){
    			this.next_waypoint_image.draw((int)this.x-14, (int)this.y-14,30,30);
    		}
    		else{
    			this.waypoint_image.draw((int)this.x-14, (int)this.y-14,30,30);
    		}
    		
    	}
    	else{
    		this.waypoint_image.draw((int)this.x-13, (int)this.y-14,30,30);
	    	
    	}
    	g.setColor(Color.black);
    	g.drawString(this.pointRef, (int)this.x-3, (int)this.y-9);
	
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
    

    public int getPointCount() {
    	return Point.pointCount;
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
