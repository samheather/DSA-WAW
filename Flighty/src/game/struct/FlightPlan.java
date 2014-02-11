

package game.struct;

import java.util.ArrayList;
import java.util.Random;

public class FlightPlan {
	
	private ArrayList<Point> currentRoute = new ArrayList<Point>();
	private Plane plane;
	private Point entryPoint;
	private Game currentGame;
	
	// CONSTRUCTOR
	

	public FlightPlan(Game currentGame, Plane plane) {
		this.plane = plane;
		this.currentGame = currentGame;
		this.entryPoint = generateEntryPoint(currentGame);
		this.currentRoute = buildRoute(currentGame, this.entryPoint);
		

		
	}
	
	public Point generateEntryPoint(Game currentGame){
		
		Random rand = new Random();

		return currentGame.getListOfEntryPoints().get(rand.nextInt(currentGame.getListOfEntryPoints().size()));
		
	}
	
	
	
	public ArrayList<Point> buildRoute(Game currentGame, Point entryPoint) {
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

				int pointsInPlan = rand.nextInt(2) + 3; 

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
					
					if(tempListOfExitPoints.get(ExitPointIndex).equals(currentGame.getAirport())){
						tempRoute.add(currentGame.getAirport().getBeginningOfRunway());
						tempRoute.add(currentGame.getAirport().getEndOfRunway());
						exitpointAdded = true;
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

	public Point getEntryPoint() {
		return entryPoint;
	}

	public void setEntryPoint(EntryPoint entryPoint) {
		this.entryPoint = entryPoint;
	}
	
	
	

}
