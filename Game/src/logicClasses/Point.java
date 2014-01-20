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
    Image nextWaypointImage, waypointImage;

	
	// CONSTRUCTORS
    
    //Point Constructor take two doubles, for x then y.
    public Point(double xcoord, double ycoord) {
    x = xcoord;
    y = ycoord;
    

    }
    
    //Point Constructor that also takes pointRef string, more commonly used.
    Point(double xcoord, double ycoord, String name){
    x = xcoord; 
    y = ycoord;
    pointRef = name;
    System.out.println("Point " + pointRef + " set:(" + x + "," + y +").");
    }
    

    
    // INIT, RENDER, UPDATE
    
	/**
	 * init: Initialises the variables and resources required for the Point class render (Sets Waypoint Images)
	 * @param gc
	 * @throws SlickException
	 */
    
    public void init(GameContainer gc) throws SlickException {
    	this.waypointImage =new Image("res/graphics/graphics/waypoint.png"); 
        this.nextWaypointImage =  new Image("res/graphics/graphics/waypoint_next.png");

	}
    
    /**
	 * render: Render the graphics for the Point class (Draws all Waypoints)
	 * @param g
	 * @param gc
	 * @throws SlickException
	 */
    
    public void render(Graphics g, Airspace airspace) throws SlickException {
    	
        
   
    	
    	if(airspace.getControls().getSelectedFlight() !=null){
    		if (airspace.getControls().getSelectedFlight().getFlightPlan().getWaypoints().indexOf(this)==0){
    			this.nextWaypointImage.draw((int)this.x-14, (int)this.y-14,30,30);
    		}
    		else{
    			this.waypointImage.draw((int)this.x-14, (int)this.y-14,30,30);
    		}
    		
    	}
    	else{
    		this.waypointImage.draw((int)this.x-14, (int)this.y-14,30,30);
	    	
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
    


    
    public boolean equals(Point point){
    	if(point instanceof Point){
    		if ((point.getX() ==  this.x) && (point.getY() == this.y)){
    			return true;
    		}
    	}
    	return false;
   }




}
