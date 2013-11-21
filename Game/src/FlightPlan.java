import java.util.ArrayList;
import java.util.Random;


public class FlightPlan {
	
	//EntryPoint entryPoint; This is not listed in the classes doc, does the flight create its entry point?
	private ArrayList<Point> waypoints = new ArrayList<Point>(); 
	public ArrayList<Point> getWaypoints() {
		return waypoints;
	}

	private int velocity = 0;
	
	public FlightPlan(Airspace a) {
		generate_velocity();
		build_route(a);
	}
	
	public int getVelocity() {
		return this.velocity;
	}
	
	public void build_route(Airspace a) {
		Random rand = new Random();
		int pointsInPlan=rand.nextInt(4)+4; //the number of waypoints in the flight plan including exit points
		int i;
		for(i=0;i<pointsInPlan-1;i++) {
			int Waypoint=rand.nextInt(9);
			while(waypoints.contains(a.getList_of_way_points().get(Waypoint))) { // this waypoint is already in the list, don't add it
				Waypoint=rand.nextInt(9);
			}
			waypoints.add(a.getList_of_way_points().get(Waypoint)); //if it isn't, add it
		}
		int ExitPoint=rand.nextInt(2);
		waypoints.add(a.getList_of_exit_points().get(ExitPoint));
		
	}
	
	public void generate_velocity() {
		Random rand = new Random();
		velocity=rand.nextInt(50)+50;
		
		
	}
	
}