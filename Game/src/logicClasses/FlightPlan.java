package logicClasses;

import java.util.ArrayList;
import java.util.Random;

public class FlightPlan {
	
	// FIELDS
	
	
	private ArrayList<Point> route = new ArrayList<Point>();
	private double velocity;
	

	// CONSTRUCTOR
	

	public FlightPlan(Airspace airspace, EntryPoint entryPoint) {
		this.velocity = generate_velocity();
		this.route = build_route(airspace, entryPoint);
	}
	

	// METHODS
	
	
	public ArrayList<Point> build_route(Airspace airspace, EntryPoint entryPoint) {
		ArrayList<Point> temp_route = new ArrayList<Point>();
		if (!airspace.getList_of_way_points().isEmpty()&& !airspace.getList_of_exit_points().isEmpty()) {
			Random rand = new Random();
				int ExitPoint = rand.nextInt(3);
				while(entryPoint.getY()==airspace.getList_of_exit_points().get(ExitPoint).getY()) {
					ExitPoint=rand.nextInt(3);
				}
				while(entryPoint.getX()==airspace.getList_of_exit_points().get(ExitPoint).getX()) {
					ExitPoint=rand.nextInt(3);
				}

				int pointsInPlan = rand.nextInt(4) + 2;
				// the number of waypoints in the flight plan including exit points

				for (int i = 0; i < pointsInPlan - 1; i++) {
					int waypoint_index = rand.nextInt(10);

					while (temp_route.contains(airspace.getList_of_way_points().get(waypoint_index))) {
						// this waypoint is already in the list, don't add it
						waypoint_index = rand.nextInt(10);
					}
					temp_route.add(airspace.getList_of_way_points().get(waypoint_index));
					// if it isn't, add it
				}
				temp_route.add(airspace.getList_of_exit_points().get(ExitPoint));
				
				

		}
		
		return temp_route;
	}


	public int generate_velocity() {
		Random rand = new Random();
		return (rand.nextInt(200) + 200);
	}
	

	// ACCESSORS AND MUTATORS

	
	
	public double getVelocity() {
		return this.velocity;
	}

	public ArrayList<Point> getWaypoints() {
		return route;
	}

	public Point getPointByIndex(int i) {
		return this.route.get(i);

	}

	@Override
	public String toString() {
		String returnString = "";
		for (int i = 0; i < this.route.size(); i++) {
			returnString += "Point " + i + ": ";
			returnString += this.route.get(i).getX();
			returnString += ", ";
			returnString += this.route.get(i).getY();
			returnString += " | ";
		}
		return returnString;
	}

}