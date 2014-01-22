package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;


public class ExitPoint extends Point {
	
	Image exitPointTop, exitPointRight, exitPointLeft;
	
	public ExitPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    
	    System.out.println("ExitPoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
    /**
     * init: Initialises the variables and resources required for the ExitPoint object render (Sets ExitPoint Images)
     * @param gc Slick2d game container object
     * @throws SlickException Slick2d exception handler
     */
	
	public void init(GameContainer gc) throws SlickException {

		    this.exitPointTop = new Image("res/graphics/exitpoint_top.png");
			this.exitPointRight = new Image("res/graphics/exitpoint_right.png");
			this.exitPointLeft = new Image("res/graphics/exitpoint_left.png");	
	   

		}
	
	/**
	 * render: Render method for the ExitPoint object, position determines orientation of image and String of name
	 * @param g Slick2d graphics object 
	 * @param airspace Airpace object
	 * @throws SlickException Slick2d exception handler
	 */
	
	public void render(Graphics g, Airspace airspace) throws SlickException {
		
		if(this.y == 0){
			this.exitPointTop.draw((int)this.x-20, (int)this.y);
		}
		
		else if(this.x == 150){
			this.exitPointLeft.draw((int)this.x, (int)this.y-20);
		}
		
		if(this.x == 1200){
			this.exitPointRight.draw((int)this.x-40, (int)this.y-20);
		}
		
		
    	g.setColor(Color.white);
    	if(this.y == 0){
    		g.drawString(this.pointRef, (int)this.x-15, (int)this.y);
    	}
    	else if(this.x ==150){
    		g.drawString(this.pointRef, (int)this.x, (int)this.y-7);
    	}
    	
    	else if(this.x ==1200){
    		g.drawString(this.pointRef, (int)this.x-35, (int)this.y-7);
    	}
    	
    	
	}



}
