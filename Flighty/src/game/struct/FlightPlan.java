

package game.struct;

import java.util.ArrayList;
import java.util.Random;

public class FlightPlan {
	
	private ArrayList<Point> currentRoute = new ArrayList<Point>();
	private Plane plane;
	private EntryPoint entryPoint;
	
	// CONSTRUCTOR
	

	public FlightPlan(Game currentGame, Plane plane) {
		this.plane = plane;
		this.entryPoint = generateEntryPoint(currentGame);
		this.currentRoute = buildRoute(currentGame, this.entryPoint);

		
	}
	
	public EntryPoint generateEntryPoint(Game currentGame){
		
		Random rand = new Random();
		int randomNumber = rand.nextInt(3);
			
		// Setting flights x and y to the coordinates of it's entrypoint
		this.plane.setX(currentGame.getListOfEntryPoints().get(randomNumber).getX()); // choose one a get the x and y values
		this.plane.setY(currentGame.getListOfEntryPoints().get(randomNumber).getY());
		
		return currentGame.getListOfEntryPoints().get(randomNumber);
		
	}
	
	
	
	public ArrayList<Point> buildRoute(Game currentGame, EntryPoint entryPoint) {
		ArrayList<Point> tempRoute = new ArrayList<Point>();  // Create the array lists for route and points
		ArrayList<Point> tempListOfWaypoints = new ArrayList<Point>();
		ArrayList<Point> tempListOfExitPoints = new ArrayList<Point>();
		Boolean exitpointAdded = false;
		
		if (!currentGame.getListOfWaypoints().isEmpty()&& !currentGame.getListOfExitPoints().isEmpty()) { // if there is a list of waypoints and a list of exit points
				Random rand = new Random();
				
				// Initialising Temporary Lists
				
				for (int i = 0; i < currentGame.getListOfWaypoints().size(); i++) { //loop through all waypoints and add them to tempwaypoints
					tempListOfWaypoints.add(currentGame.getListOfWaypoints().get(i));
				}
				
				for (int i = 0; i < currentGame.getListOfExitPoints().size(); i++) {// loop through all exit points and add them to temppoints
					tempListOfExitPoints.add(currentGame.getListOfExitPoints().get(i));
				}
				
				// Adding Waypoints to Plan
				
				int pointsInPlan = rand.nextInt(3) + 3; 
				
				for (int i = 0; i < pointsInPlan - 1; i++) {
					int waypointIndex = rand.nextInt(tempListOfWaypoints.size());
					tempRoute.add(tempListOfWaypoints.get(waypointIndex));
					tempListOfWaypoints.remove(waypointIndex);
				}
				
				// Adding ExitPoint to Plan
				
				int ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
				
				while (exitpointAdded == false){
					
					if (this.entryPoint.getY() == tempListOfExitPoints.get(ExitPointIndex).getY()){
						tempListOfExitPoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
					}
					
					else if (this.entryPoint.getX() == tempListOfExitPoints.get(ExitPointIndex).getX()){
						tempListOfExitPoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
					}
					else{
						tempRoute.add(tempListOfExitPoints.get(ExitPointIndex));
						exitpointAdded = true;
					}
				}
		}
		
		return tempRoute;
	}

	public ArrayList<Point> getCurrentRoute() {
		return currentRoute;
	}

	public void setCurrentRoute(ArrayList<Point> currentRoute) {
		this.currentRoute = currentRoute;
	}

	public Plane getPlane() {
		return plane;
	}

	public void setPlane(Plane plane) {
		this.plane = plane;
	}

	public EntryPoint getEntryPoint() {
		return entryPoint;
	}

	public void setEntryPoint(EntryPoint entryPoint) {
		this.entryPoint = entryPoint;
	}
	
	
	

}
