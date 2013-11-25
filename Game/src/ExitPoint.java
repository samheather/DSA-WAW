import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;


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
	public void render(Graphics g) {
		g.setColor(Color.red);
		g.fillArc((int)this.x-15, (int)this.y, 30, 30, 270, 90);
    	//g.fillOval((int)this.x, (int)this.y, 20, 20);
    	g.setColor(Color.white);
    	g.drawString(this.pointRef, (int)this.x, (int)this.y);
	}


}
