package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;


public class ExitPoint extends Point {
	
	Image exit_point_top, exit_point_right, exit_point_left;

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

		    this.exit_point_top = new Image("/res/graphics/graphics/exitpoint_top.png");
			this.exit_point_right = new Image("/res/graphics/graphics/exitpoint_right.png");
			this.exit_point_left = new Image("/res/graphics/graphics/exitpoint_left.png");	
	   

		}
	
	@Override
	public void render(Graphics g, Airspace airspace) throws SlickException {
		

		

		
		if(this.y == 0){
			this.exit_point_top.draw((int)this.x-20, (int)this.y);
		}
		
		else if(this.x == 150){
			this.exit_point_left.draw((int)this.x, (int)this.y-20);
		}
		
		if(this.x == 1200){
			this.exit_point_right.draw((int)this.x-40, (int)this.y-20);
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
