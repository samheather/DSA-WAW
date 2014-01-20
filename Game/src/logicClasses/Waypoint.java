package logicClasses;

import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;



public class Waypoint extends Point {
	
	Image nextWaypointImage, waypointImage;

	
	public Waypoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    
	    System.out.println("Waypoint " + pointRef + " set:(" + x + "," + y +").");
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
		 * @param airspace
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
	


}
