package logicClasses;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;


public class EntryPoint extends Point {
	
	Image entry_point_top, entry_point_right, entry_point_left;

	EntryPoint(double xcoord, double ycoord) throws SlickException{
	    super(xcoord, ycoord);
	    pointCount += 1; 
	    this.entry_point_top = new Image("/res/graphics/graphics/entrypoint_top.png");
		this.entry_point_right = new Image("/res/graphics/graphics/entrypoint_right.png");
		this.entry_point_left = new Image("/res/graphics/graphics/entrypoint_left.png");
	}
	
	EntryPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("EntryPoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
    public void init(GameContainer gc) throws SlickException {
    	
   

	}
	public void render(Graphics g) throws SlickException {
		
		
		if(this.y == 0){
			this.entry_point_top.draw((int)this.x-20, (int) this.y);
		}
		
		else if(this.x == 150){
			this.entry_point_left.draw((int)this.x, (int) this.y-20);
		}
		
		else if(this.x == 1200){
			this.entry_point_right.draw((int)this.x-40, (int) this.y-20);
		}
    }
	


}
