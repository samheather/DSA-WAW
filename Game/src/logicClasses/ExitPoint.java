package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;


public class ExitPoint extends Point {
	
	Image exitPointTop, exitPointRight, exitPointLeft;

	public ExitPoint(double xcoord, double ycoord) {
	    super(xcoord, ycoord);
	    pointCount += 1; 

	}
	
	ExitPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("Waypoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
	   public void init(GameContainer gc) throws SlickException {

		    this.exitPointTop = new Image("/res/graphics/graphics/exitpoint_top.png");
			this.exitPointRight = new Image("/res/graphics/graphics/exitpoint_right.png");
			this.exitPointLeft = new Image("/res/graphics/graphics/exitpoint_left.png");	
	   

		}
	
	@Override
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
