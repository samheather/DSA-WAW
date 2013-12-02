import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;


public class ExitPoint extends Point {

	ExitPoint(double xcoord, double ycoord){
	    super(xcoord, ycoord);
	    pointCount += 1; 
	}
	
	ExitPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("Waypoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
	@Override
	public void render(Graphics g, Airspace airspace) {
		g.setColor(Color.yellow);
    	if(this.y>0&&this.y<600) {
    		if (this.x == 100){
    			g.drawLine((int)this.x, (int)this.y+30, (int)this.x+10, (int)this.y+30);
    			g.drawLine((int)this.x, (int)this.y-10, (int)this.x+10, (int)this.y-10);
    		}
    		else{
    			g.drawLine((int)this.x, (int)this.y+30, (int)this.x-10, (int)this.y+30);
    			g.drawLine((int)this.x, (int)this.y-10, (int)this.x-10, (int)this.y-10);
    		}
    	}
    	else {
    		
    		g.drawLine((int)this.x+30, (int)this.y-10, (int)this.x+30, (int)this.y+10);
    		g.drawLine((int)this.x-10, (int)this.y-10, (int)this.x-10, (int)this.y+10);
    	}
    	g.setColor(Color.white);
    	if(this.x == 1200){
    		g.drawString(this.pointRef, (int)this.x-50, (int)this.y);
    	}
    	else{
    		g.drawString(this.pointRef, (int)this.x, (int)this.y);
    	}
	}



}
