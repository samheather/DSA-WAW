import java.util.ArrayList;
import java.util.Random;


public class FlightPlan {
	
	//EntryPoint entryPoint; This is not listed in the classes doc, does the flight create its entry point?
	private ArrayList<Waypoint> waypoints; // the list is for one type of data so the exit point has to be seperate from this list, unless we simply have a point class
	private ExitPoint exitPoint; 
	private int velocity = 0;
	
	public int getVelocity() {
		return this.velocity;
	}
	
	public void build_route() {
		
		
		
	}
	
	public void generate_velocity() {
		Random rand = new Random();
		velocity=rand.nextInt(50)+50;
		
		
	}
	
}