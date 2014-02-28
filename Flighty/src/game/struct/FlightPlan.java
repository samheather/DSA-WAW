

package game.struct;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Random;

public class FlightPlan implements java.io.Serializable {

	/** Array list which contains waypoints of flight plan*/
	private ArrayList<Point> currentRoute = new ArrayList<Point>();
	
	/** Plane for which flight plan is generated */
	private Plane plane;
	
	/** Entry point i.e. first waypoint of flight plan */
	private Point entryPoint;
	
	/** Required by Slick2D */
	private Game currentGame;
	
	// CONSTRUCTOR
	
	/** 
	 * Constructor for FlightPlan
	 * 
	 * @param currentGame Required by Slick2D
	 * @param plane Plane for which flight plan is generated
	 */
	public FlightPlan(Game currentGame, Plane plane) {
		this.plane 				= plane;
		this.currentGame 		= currentGame;
		this.entryPoint 		= generateEntryPoint(currentGame);
		this.currentRoute 		= buildRoute(currentGame, this.entryPoint);	
	}
	
	/**
	 * Randomly chooses entry point i.e. the first waypoint, of a plane's flight plan
	 * from list of entry points
	 * 
	 * @param currentGame Required by Slick2D
	 * @return Entry point
	 */
	public Point generateEntryPoint(Game currentGame){
		
		Random rand = new Random();

		return currentGame.getListOfEntryPoints().get(rand.nextInt
					(currentGame.getListOfEntryPoints().size()));	
	}
	
	/**
	 * Builds route for flight plan
	 * @param currentGame Required by Slick2D
	 * @param entryPoint The entry point for a flight i.e. the first waypoint the plane must fly through
	 * @return The route which the plane must fly
	 */
	public ArrayList<Point> buildRoute(Game currentGame, Point entryPoint)
	{
		// Create the array lists for route and points
		ArrayList<Point> tempRoute = new ArrayList<Point>();  
		ArrayList<Point> tempListOfWaypoints = new ArrayList<Point>();
		ArrayList<Point> tempListOfExitPoints = new ArrayList<Point>();
		
		Boolean exitpointAdded = false;
		
		// Checks if there is a list of waypoints and a list of exit points
		if (!currentGame.getListOfWaypoints().isEmpty() && !currentGame.getListOfExitPoints().isEmpty())
		{
			Random rand = new Random();
			
			// Initialising Temporary Lists
			// Loops through all waypoints and add them to tempwaypoints
			for (int i = 0; i < currentGame.getListOfWaypoints().size(); i++)
			{ 
				tempListOfWaypoints.add(currentGame.getListOfWaypoints().get(i));
			}
			
			// Loops through all exit points and add them to temppoints
			for (int i = 0; i < currentGame.getListOfExitPoints().size(); i++)
			{
				tempListOfExitPoints.add(currentGame.getListOfExitPoints().get(i));
			}

			// Adding waypoints to flight plan
			int pointsInPlan = rand.nextInt(2) + 3; 
			
			// This loop ensures that a unique waypoint is selected in each pick
			for (int i = 0; i < pointsInPlan - 1; i++)
			{
				int waypointIndex = rand.nextInt(tempListOfWaypoints.size());
				tempRoute.add(tempListOfWaypoints.get(waypointIndex));
				tempListOfWaypoints.remove(waypointIndex);
			}

			// Adding ExitPoint to Plan
			int ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
			
			//Makes sure that the entrypoints and the exitpoints are not on the same side
			while (exitpointAdded == false)
			{
				// If they are on the same vertical line
				if (this.entryPoint.getY() == tempListOfExitPoints.get(ExitPointIndex).getY())
				{
					tempListOfExitPoints.remove(ExitPointIndex);
					ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
				}
				// If they are on the same horizontal line
				else if (this.entryPoint.getX() == tempListOfExitPoints.get(ExitPointIndex).getX())
				{
					tempListOfExitPoints.remove(ExitPointIndex);
					ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
				}
				
				/* If the entrypoint is the airport then the runway points are added 
				to the plan and the airport removed. */
				if(tempListOfExitPoints.get(ExitPointIndex).equals(currentGame.getAirport()))
				{
					tempRoute.add(currentGame.getAirport().getBeginningOfRunway());
					tempRoute.add(currentGame.getAirport().getEndOfRunway());
					exitpointAdded = true;
				}
				else
				{
					tempRoute.add(tempListOfExitPoints.get(ExitPointIndex));
					exitpointAdded = true;
				}
			}
		}
		
		return tempRoute;
	}

	/**
	 * 
	 * @return Current route for selected plane
	 */
	public ArrayList<Point> getCurrentRoute()
{
		return currentRoute;
	}

	/**
	 * Sets currentRoute
	 * @param currentRoute New route
	 */
	public void setCurrentRoute(ArrayList<Point> currentRoute)
{
		this.currentRoute = currentRoute;
	}

	/**
	 * 
	 * @return Selected plane
	 */
	public Plane getPlane()
{
		return plane;
	}

	/**
	 * 
	 * @param plane Plane to set
	 */
	public void setPlane(Plane plane)
{
		this.plane = plane;
	}

	/**
	 * 
	 * @return Entry point for selected flight
	 */
	public Point getEntryPoint()
{
		return entryPoint;
	}

	/**
	 * 
	 * @param entryPoint Entry point to set
	 */
	public void setEntryPoint(EntryPoint entryPoint)
	{
		this.entryPoint = entryPoint;
	}
}
