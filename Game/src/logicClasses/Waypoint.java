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
	
	   // INIT, RENDER
    
		/**
		 * init: Initialises the variables and resources required for the Waypoint class render (Sets Waypoint Images)
		 * @param gc Slick2d game container
		 * @throws SlickException Slick2d exception handler
		 */
	    
	    public void init(GameContainer gc) throws SlickException {
	    	this.waypointImage =new Image("res/graphics/waypoint.png"); 
	        this.nextWaypointImage =  new Image("res/graphics/waypoint_next.png");

		}
	
	   /**
		 * render: Render the graphics for the Waypoint class (Draws all Waypoints)
		 * @param g slick2d graphics object
		 * @param airspace object
		 * @throws SlickException Slick2d exception handler
		 */
	    
	    public void render(Graphics g, Airspace airspace) throws SlickException {
	    	
	        
	   
	    	
	    	if(airspace.getControls().getSelectedFlight() !=null){ // If there is a selected flight use its next waypoint and draw it as next
	    		if (airspace.getControls().getSelectedFlight().getFlightPlan().getCurrentRoute().indexOf(this)==0){
	    			this.nextWaypointImage.draw((int)this.x-14, (int)this.y-14,30,30);
	    		}
	    		else{
	    			this.waypointImage.draw((int)this.x-14, (int)this.y-14,30,30);
	    		}
	    		
	    	}
	    	else{
	    		this.waypointImage.draw((int)this.x-14, (int)this.y-14,30,30); // If there is no flight then draw the waypoint as not next
		    	
	    	}
	    	g.setColor(Color.black);
	    	g.drawString(this.pointRef, (int)this.x-3, (int)this.y-9);
		
	    }
	


}
