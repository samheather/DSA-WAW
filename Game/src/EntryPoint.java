import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;


public class EntryPoint extends Point {

	EntryPoint(double xcoord, double ycoord){
	    super(xcoord, ycoord);
	    pointCount += 1; 
	}
	
	EntryPoint(double xcoord, double ycoord, String name){
	    super(xcoord, ycoord, name);
	    pointCount += 1;
	    System.out.println("EntryPoint " + pointRef + " set:(" + x + "," + y +").");
	}
	
	public void render(Graphics g) {
    	g.setColor(Color.green);
    	if(this.y>0&&this.y<600) {
    		g.drawLine((int)this.x-10, (int)this.y+30, (int)this.x+10, (int)this.y+30);
    		g.drawLine((int)this.x-10, (int)this.y-10, (int)this.x+10, (int)this.y-10);
    	}
    	else {
    		g.drawLine((int)this.x+30, (int)this.y-10, (int)this.x+30, (int)this.y+10);
    		g.drawLine((int)this.x-10, (int)this.y-10, (int)this.x-10, (int)this.y+10);
    	}
    }
	


}
