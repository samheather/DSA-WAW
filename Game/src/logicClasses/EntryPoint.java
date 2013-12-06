package logicClasses;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;


public class EntryPoint extends Point {
	
	Image entry_point_top, entry_point_right, entry_point_left;

	EntryPoint(double xcoord, double ycoord){
	    super(xcoord, ycoord);
	    pointCount += 1; 
	}
	
	EntryPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("EntryPoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
    public void init(GameContainer gc) throws SlickException {
    	entry_point_top = new Image("/res/graphics/graphics/entrypoint_top.png");
		entry_point_right = new Image("/res/graphics/graphics/entrypoint_right.png");
		entry_point_left = new Image("/res/graphics/graphics/entrypoint_left.png");
   

	}
	public void render(Graphics g) throws SlickException {
		
		
		if(this.y == 0){
			entry_point_top.draw((int)this.x-20, (int) this.y);
		}
		
		else if(this.x == 150){
			entry_point_left.draw((int)this.x, (int) this.y-20);
		}
		
		else if(this.x == 1200){
			entry_point_right.draw((int)this.x-40, (int) this.y-20);
		}
    }
	


}
