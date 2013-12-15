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
		ArrayList<Point> temp_list_of_waypoints = new ArrayList<Point>();
		ArrayList<Point> temp_list_of_exitpoints = new ArrayList<Point>();
		Boolean exitpoint_added = false;
		
		if (!airspace.getList_of_way_points().isEmpty()&& !airspace.getList_of_exit_points().isEmpty()) {
				Random rand = new Random();
				
				// Initialising Temporary Lists
				
				for (int i = 0; i < airspace.getList_of_way_points().size(); i++) {
					temp_list_of_waypoints.add(airspace.getList_of_way_points().get(i));
				}
				
				for (int i = 0; i < airspace.getList_of_exit_points().size(); i++) {
					temp_list_of_exitpoints.add(airspace.getList_of_exit_points().get(i));
				}
				
				// Adding Waypoints to Plan
				
				int pointsInPlan = rand.nextInt(4) + 2;
				
				for (int i = 0; i < pointsInPlan - 1; i++) {
					int waypoint_index = rand.nextInt(temp_list_of_waypoints.size());
					temp_route.add(temp_list_of_waypoints.get(waypoint_index));
					temp_list_of_waypoints.remove(waypoint_index);
				}
				
				// Adding ExitPoint to Plan
				
				int ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
				
				while (exitpoint_added == false){
					
					if (entryPoint.getY() == temp_list_of_exitpoints.get(ExitPointIndex).getY()){
						temp_list_of_exitpoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
					}
					
					else if (entryPoint.getX() == temp_list_of_exitpoints.get(ExitPointIndex).getX()){
						temp_list_of_exitpoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
					}
					else{
						temp_route.add(temp_list_of_exitpoints.get(ExitPointIndex));
						exitpoint_added = true;
					}
				}
		}
		
		return temp_route;
	}


	public int generate_velocity() {
		Random rand = new Random();
		return (rand.nextInt(200) + 200);
	}
	

	// ACCESSORS AND MUTATORS

	public void setVelocity(double new_velocity){
		this.velocity = new_velocity;
		
	}
	
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