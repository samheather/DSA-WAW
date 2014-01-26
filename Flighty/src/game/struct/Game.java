package game.struct;


import game.gfx.GameWindow;

import java.util.ArrayList;
import java.util.ListIterator;


/**
 * Game class controls basic game mechanics
 * <p>
 * <ul>
 * <li>Holds a list of current planes</li>
 * <li>Creates planes</li>
 * <li>Removes planes</li>
 * <li>Tests for collisions</li>
 * </ul>
 * </p>
 */
public class Game {
	
	private ArrayList<ExitPoint> listOfExitPoints = new ArrayList<ExitPoint>();
	private ArrayList<Waypoint> listOfWaypoints = new ArrayList<Waypoint>();
	private ArrayList<EntryPoint> listOfEntryPoints  = new ArrayList<EntryPoint>();
	/** Distance at which planes crash */
	private int separationDistance;
	
	/** Distance at which warning ring around appears */
	private int  penaltyDistance;
	
	/** List of planes currently in the game */
	private ArrayList<Plane> currentPlanes = new ArrayList<Plane>();
	
	/** List of strings that make up first part of a plane's unique ID */
	private ArrayList<String> carriers = new ArrayList<String>();	
	
	/** Reference to the game window  */
	private GameWindow currentGameWindow;	
	
	
	// Constructors
	/**
	 * Constructor for Game
	 * <p>
	 * Initialises carrier names.
	 * </p>
	 * 
	 * @param separationDistance	the distance at which planes should collide
	 * @param penaltyDistance		the distance at which planes should alert
	 * @param currentGameWindow		reference to the current game window
	 */
	public Game(int separationDistance, int penaltyDistance, 
			GameWindow currentGameWindow) {
		this.separationDistance = separationDistance;
		this.penaltyDistance = penaltyDistance;
		this.listOfExitPoints = new ArrayList<ExitPoint>();
		this.listOfWaypoints = new ArrayList<Waypoint>();
		this.listOfEntryPoints = new ArrayList<EntryPoint>();
		this.carriers.add("BA");
		this.carriers.add("EZY");
		this.carriers.add("NZ");
		this.carriers.add("RY");
		this.carriers.add("QU");
		this.addPointsForGame();
		
		this.currentGameWindow = currentGameWindow;
	}
	
	/**
	 * Constructor for Game
	 * <p>
	 * <b>Should be used for testing ONLY.</b>
	 * </p>
	 * 
	 * @param separationDistance	the distance at which planes should collide
	 * @param penaltyDistance		the distance at which planes should alert
	 * @param testing				<code>true</code> if testing,
	 * 								<code>false</code> otherwise
	 */
	public Game(int separationDistance, int penaltyDistance, 
			boolean testing) {
		this.separationDistance = separationDistance;
		this.penaltyDistance = penaltyDistance;
		
		this.carriers.add("BA");
		this.carriers.add("EZY");
		this.carriers.add("NZ");
		this.carriers.add("RY");
		this.carriers.add("QU");
		
		this.currentGameWindow = null;
		
		//this.addPointsForGame();
	}

	

	// MAIN METHODS
	
	public void addPointsForGame(){
		this.listOfEntryPoints.add(new EntryPoint(0,400));
		this.listOfEntryPoints.add(new EntryPoint(1200,200));
		this.listOfEntryPoints.add(new EntryPoint(600,0));
		
		this.listOfWaypoints.add(new Waypoint(350,150));
		this.listOfWaypoints.add(new Waypoint(400,470));
		this.listOfWaypoints.add(new Waypoint(700,60));
		this.listOfWaypoints.add(new Waypoint(800,320));
		this.listOfWaypoints.add(new Waypoint(600, 418));
		this.listOfWaypoints.add(new Waypoint(500, 220));
		this.listOfWaypoints.add(new Waypoint(950, 188));
		this.listOfWaypoints.add(new Waypoint(1050, 272));
		this.listOfWaypoints.add(new Waypoint(900, 420));
		this.listOfWaypoints.add(new Waypoint(240, 250));
		
		this.listOfExitPoints.add(new ExitPoint(800,0));
		this.listOfExitPoints.add(new ExitPoint(0,200));
		this.listOfExitPoints.add(new ExitPoint(1200,300));
		
		
	}
	
	
	
	public String generateFlightID(){
		
		String id = "DEFAULT_ID", idRandNum;
		// Generate a random id, consisting of a carrier ID and a
		// random 2-digit number
		do {
			idRandNum = String.valueOf(Math.round(Math.random() * 100)
					% 100);
			idRandNum = idRandNum.length() == 1 ?
					"0".concat(idRandNum) : idRandNum;
			id = carriers.get((int) (Math.round(Math.random()
					* carriers.size()) % carriers.size())).concat(
							String.valueOf(idRandNum));
		} while (this.getPlaneFromID(id) != null);
		
		return id;

	}
	
	/**
	 * Creates a plane
	 * <p>
	 * Calls {@link #createPlane(boolean)} with testing = false,
	 * creating a plane and adding it to the game.
	 * 
	 * @see					#createPlane(boolean)
	 * @return				the plane
	*/
	public String createPlane() {
		return this.createPlane(false).getID();
	}
	
	/**
	 * Creates a plane
	 * <p>
	 * Performs the following actions:
	 * <ul>
	 * <li>Initialises velocity and bearing</li>
	 * <li>Generates unique ID: a carrier string concatenated with a
	 * 2 digit randomised number.</li>
	 * <li>Checks against other plane's ID to insure it is unique</li>
	 * <li>Generates a random crew number</li>
	 * <li>Generates a random starting position on the edge of the map</li>
	 * <li>Generates flight plan consisting of {@link game.struct.Waypoint} nodes</li>
	 * <li>Adds plane to list currentPlanes</li>
	 * </ul>
	 * </p>
	 * 
	 * @param testing		use <code>true</code> if testing,
	 * 						or <code>false</code> otherwise
	 * @return				plane's ID
	*/
	public Plane createPlane(boolean testing) {
		Plane newPlane;
		String id;
		int size, velocity = 7000;
		double altitude = 1;
		float x, y;
		double bearing = 0;
		int width, height;
		boolean[] penaltyTest;
		
		if(this.currentGameWindow != null) {
			width = this.currentGameWindow.getWindowWidth();
			height = this.currentGameWindow.getWindowHeight();
		} else {
			width = 1024;
			height = 512;
		}

		// Generate a random id, consisting of a carrier ID and a
		// random 2-digit number
		id = this.generateFlightID();

		
		// Generate a random size, between 1 and 3
		size = (int) (1 + (Math.round(Math.random()*100) % 2));
		
		// Use new size to adjust velocity
		velocity += (1000 * size);
		
		newPlane = new Plane(id, size, velocity, altitude, bearing, this);
		newPlane.setX(newPlane.getFlightPlan().getEntryPoint().getX());
		newPlane.setY(newPlane.getFlightPlan().getEntryPoint().getY());
		newPlane.setStartX(newPlane.getX());
		newPlane.setStartY(newPlane.getY());
		
		
		
		// Skip adding flight plan if testing toggle on
		if(!testing) {
			

			// Add new plane to the game
			this.currentPlanes.add(newPlane);
		}
		System.out.println("Plane HAPPENED");
		
		return newPlane;
		
	}

	/**
	 * Removes a plane from the game
	 * <p>
	 * Iterates through currentPlanes list and removes the plane
	 * </p>
	 * 
	 * @param toDelete		the plane to remove
	*/
	public void removePlane(Plane toDelete) {
		for (ListIterator<Plane> iter = this.currentPlanes
				.listIterator(this.currentPlanes.size());
				iter.hasPrevious();){
		    if (toDelete.equals(iter.previous())) { iter.remove(); return; }
		}
	}
	
	/**
	 * Returns the plane with the given ID
	 * <p>
	 * Iterates through currentPlanes to find the plane from it's ID
	 * </p>
	 * <p>
	 * Returns null if plane not found
	 * </p>
	 * 
	 * @param id			the ID of the plane to find
	 * @return				plane specified by id
	*/
	public Plane getPlaneFromID(String id) {
		for(Plane plane : this.currentPlanes) {
			if(plane.getID().equals(id)) {
				return plane;
			}
		}
		// If id not in array, return null
		return null;
	}
	
	/**
	 * Tests whether a plane is either colliding or alerting
	 * <p>
	 * Tests whether planes are within either the penalty or separation distance
	 * </p>
	 * <p>
	 * Performs the following actions:
	 * <ul>
	 * <li>Iterates through currentPlanes, ignoring the plane under test and planes
	 * that are on different altitudes
	 * <li>Calculates the distance between them and plane
	 * <li>If the distance is less than the separation distance, mark the plane as
	 * colliding
	 * <li>If the distance is less than the penalty distance, mark the plane as
	 * alerting</li>
	 * </ul>
	 * </p>
	 * 
	 * @param planeI		the plane to test
	 * @return				<code>true</code> if the plane is colliding,
	 * 						<code>false</code> otherwise
	*/
	public boolean collision(Plane planeI) {
		return this.collisionHelper(planeI)[0];
	}

	/**
	 * Helper method for {@link #collision(Plane)}
	 * <p>
	 * Tests whether planes are within either the penalty or separation distance,
	 * and marks the plane as such
	 * </p>
	 * <p>
	 * If check is set to <code>true</code>, this will not update images
	 * (but will still return the check result).
	 * </p>
	 * 
	 * @param planeI		the plane to test
	 * @return				an array of size 2, with the 0th element set if the plane
	 * 						is colliding, and the 1th element set if the plane is alerting
	*/
	public boolean[] collisionHelper(Plane planeI) {

		double distIJSqr;
		boolean risk = false;
		
		// First -> are planes colliding? {1:true, 0:false}
		// Second -> result used for testing purposes {1:true, 0:false}
		boolean[] result = new boolean[] {false, false};

		for(Plane planeJ : this.currentPlanes) {
			if((planeI.equals(planeJ))
					|| (planeJ.getAltitude()
							> (planeI.getAltitude() + 1.1))
					|| (planeJ.getAltitude()
							< (planeI.getAltitude() - 1.1))) {
				continue;
			}

			distIJSqr = Math.pow(planeJ.getX() - planeI.getX(), 2)
					+ Math.pow(planeJ.getY() - planeI.getY(), 2);

			if(distIJSqr < Math.pow(this.separationDistance, 2)) {
				// Two planes have collided
				result[0] = true;
				return result;
			} else if(distIJSqr < Math.pow(this.penaltyDistance, 2)) {
				planeJ.setAlertStatus(true);
				risk = true;
			}
		}
		
		planeI.setAlertStatus(risk);
		result[1] = risk;
	
		return result;
	}
	
	// ACCESSORS
	
	
		/**
		 * @return				separation distance (the collision range)
		 */
		public int getSeparationDistance() {
			return this.separationDistance;
		}
		
		/**
		 * @return				penalty distance (the alert range)
		 */
		public int getPenaltyDistance() {
			return this.penaltyDistance;
		}
		
		/**
		 * @return				list of planes attached to the game
		 */
		public ArrayList<Plane> getCurrentPlanes() {
			return this.currentPlanes;
		}
		
		/**
		 * @return				list of carriers attached to the game
		 */
		public ArrayList<String> getCarriers() {
			return this.carriers;
		}
		
		/**
		 * @return				a reference to the current game window
		 */
		public GameWindow getCurrentGameWindow() {
			return this.currentGameWindow;
		}
		
		
		// MUTATORS
		
		
		/**
		 * @param separationDistance	distance at which planes should collide
		 */
		public void setSeparationDistance(int separationDistance) {
			this.separationDistance = separationDistance;
		}
		
		/**
		 * @param penaltyDistance		distance at which planes should alert
		 */
		public void setPenaltyDistance(int penaltyDistance) {
			this.penaltyDistance = penaltyDistance;
		}
		
		/**
		 * @param currentPlanes	the array of planes to set
		 */
		public void setCurrentPlanes(ArrayList<Plane> currentPlanes) {
			this.currentPlanes = currentPlanes;
		}
		
		/**
		 * @param carriers		the array of carrier ID's to set
		 */
		public void setCarriers(ArrayList<String> carriers) {
			this.carriers = carriers;
		}
		
		/**
		 * @param currentGameWindow		a new parent game window
		 */
		public void setCurrentGameWindow(GameWindow currentGameWindow) {
			this.currentGameWindow = currentGameWindow;
		}

		public ArrayList<ExitPoint> getListOfExitPoints() {
			return listOfExitPoints;
		}

		public void setListOfExitPoints(ArrayList<ExitPoint> listOfExitPoints) {
			this.listOfExitPoints = listOfExitPoints;
		}

		public ArrayList<Waypoint> getListOfWaypoints() {
			return listOfWaypoints;
		}

		public void setListOfWaypoints(ArrayList<Waypoint> listOfWaypoints) {
			this.listOfWaypoints = listOfWaypoints;
		}

		public ArrayList<EntryPoint> getListOfEntryPoints() {
			return listOfEntryPoints;
		}

		public void setListOfEntryPoints(ArrayList<EntryPoint> listOfEntryPoints) {
			this.listOfEntryPoints = listOfEntryPoints;
		}
}
