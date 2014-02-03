package game.struct;


import game.gfx.GameWindow;

import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Random;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Image;
import org.newdawn.slick.Input;
import org.newdawn.slick.state.StateBasedGame;


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
	
	private ArrayList<Point> listOfExitPoints = new ArrayList<Point>();
	private ArrayList<Waypoint> listOfWaypoints = new ArrayList<Waypoint>();
	private ArrayList<Point> listOfEntryPoints  = new ArrayList<Point>();
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
	
	/** The width the game is displayed at */
	public int windowWidth;
	
	/** The height the game is displayed at */
	public int windowHeight;
	
	/** The player's score*/
	private double score;

	/** Whether the player should be penalised **/
	boolean penalty;
	
	/** The time the game has been running for */
	private double time;
	
	/** The time the game ended at */
	private double endTime;
	
	/** The amount of time before another plane will enter the game */
	private double countToNextPlane;
	
	/** The collision state - <code>true</code> if two planes have collided */
	private boolean collision;
	
	/** The ending state - <code>true</code> if the game is ending */
	private boolean ending;
	
	/** The speed modifier */
	private double speedDifficulty;
	
	/** The (plane) spawn rate modifier */
	private int spawnRate;
	
	/** The number of planes to spawn at a time */
	private int spawnCount;
	
	/** A list of planes under manual control */
	private ArrayList<Plane> manualPlanes;
	
	/** A list of planes which are colliding */
	private ArrayList<Plane> collidedPlanes;

	
	/** The plane currently being controlled by the player */
	private Plane currentPlane;
	
	/** The current map */
	private Image map;
	
	private Airport airport;
	
	
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
	public Game(int separationDistance, int penaltyDistance) {
		this.windowWidth = 1200;
		this.windowHeight = 600;
		this.separationDistance = separationDistance;
		this.penaltyDistance = penaltyDistance;
		this.listOfExitPoints = new ArrayList<Point>();
		this.listOfWaypoints = new ArrayList<Waypoint>();
		this.listOfEntryPoints = new ArrayList<Point>();
		this.score = 0;
		this.penalty = true;
		this.countToNextPlane = 0;
		this.collision = false;
		this.ending = false;
		this.airport = new Airport(-100,-100); // Placing Airport off screen
		this.manualPlanes = new ArrayList<Plane>();
		this.collidedPlanes = new ArrayList<Plane>();
		this.currentPlane = null;
		this.carriers.add("BA");
		this.carriers.add("EZY");
		this.carriers.add("NZ");
		this.carriers.add("RY");
		this.carriers.add("QU");
		this.addPointsForGame();
		this.penalty = true;
		
		
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
		//this.listOfEntryPoints.add(new EntryPoint(0,400));
		//this.listOfEntryPoints.add(new EntryPoint(1200,200));
		this.listOfEntryPoints.add(new EntryPoint(600,0));
		this.listOfEntryPoints.add(this.airport);
		
		this.listOfWaypoints.add(new Waypoint(250,150));
		this.listOfWaypoints.add(new Waypoint(300,470));
		this.listOfWaypoints.add(new Waypoint(600,160));
		this.listOfWaypoints.add(new Waypoint(700,320));
		this.listOfWaypoints.add(new Waypoint(500, 200));
		this.listOfWaypoints.add(new Waypoint(400, 220));
		this.listOfWaypoints.add(new Waypoint(850, 188));
		this.listOfWaypoints.add(new Waypoint(950, 272));
		this.listOfWaypoints.add(new Waypoint(1000, 300));
		this.listOfWaypoints.add(new Waypoint(140, 250));
		
		//this.listOfExitPoints.add(new ExitPoint(800,0));
		//this.listOfExitPoints.add(new ExitPoint(0,200));
		//this.listOfExitPoints.add(new ExitPoint(1200,300));
		this.listOfExitPoints.add(this.airport);
		
		
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
	 * Moves a plane
	 * <p>
	 * If the plane is under manual control, it will follow the
	 * bearing specified by the player.
	 * </p>
	 * <p>
	 * If the plane is following its flight path, it will tend towards
	 * its next target.
	 * </p>
	 * 
	 * @param plane				the plane to move
	 */
	public void movePlane(Plane plane) {
		if(plane.getAltitude() < 2000 && plane.getTargetAltitude()>0) {
			// Move the plane
			plane.setX((float) (plane.getX()
					- (Math.cos(Math.toRadians(plane.getBearing()))
							* (this.speedDifficulty
									* plane.getVelocity() / 7000d))));
			//System.out.println(String.valueOf((double)((double)plane.getVelocity()/7000d)));
			//System.out.println(this.speedDifficulty*plane.getVelocity()/7000d);
			plane.setY((float) (plane.getY()
					- (Math.sin(Math.toRadians(plane.getBearing()))
							* (this.speedDifficulty
									* plane.getVelocity() / 7000d))));
		}
		else {
			double angle = plane.getTargetBearing();

			// Get the angle to the next waypoint
			if(plane.getTarget() != null) {
				if(!this.manualPlanes.contains(plane)) {

					plane.calculateHeadingToNextWaypoint();
					plane.updateCurrentHeading();
					//System.out.println(angle);

				}
				else {
					plane.updateCurrentHeading();
					//; <----- does not work
				}

				// Move the plane
				plane.setX((float) (plane.getX()
						- (Math.cos(Math.toRadians(plane.getBearing()))
								* (this.speedDifficulty
										* plane.getVelocity() / 7000d))));
				//System.out.println(String.valueOf((double)((double)plane.getVelocity()/7000d)));
				//System.out.println(this.speedDifficulty*plane.getVelocity()/7000d);
				plane.setY((float) (plane.getY()
						- (Math.sin(Math.toRadians(plane.getBearing()))
								* (this.speedDifficulty
										* plane.getVelocity() / 7000d))));
			}
		}
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
		double angle;
		String id;
		int size, velocity = 7000;
		int altitude = 3000;
		float x, y;
		double bearing = 0;
		int width, height;
		boolean[] penaltyTest;
		
		// Generate a random id, consisting of a carrier ID and a
		// random 2-digit number
		id = this.generateFlightID();

		
		// Generate a random size, between 1 and 3
		size = (int) (1 + (Math.round(Math.random()*100) % 2));
		
		// Use new size to adjust velocity
		velocity += (1000 * size);
		
		newPlane = new Plane(id, size, velocity, altitude, bearing, this);
		if(newPlane.getFlightPlan().getEntryPoint()==this.airport) {
			
			newPlane.getFlightPlan().setEntryPoint((new EntryPoint(1180,580)));
			newPlane.getFlightPlan().getCurrentRoute().add(0,this.airport.getBeginningOfRunway());
			newPlane.getFlightPlan().getCurrentRoute().add(0,this.airport.getEndOfRunway());
			
			newPlane.setTarget(newPlane.getFlightPlan().getCurrentRoute().get(0));
			newPlane.setVelocity(0);
			newPlane.setAltitude(0);
			newPlane.setTargetAltitude(0);
			this.listOfEntryPoints.remove(this.airport);
			
		}

			newPlane.setX(newPlane.getFlightPlan().getEntryPoint().getX());
			newPlane.setY(newPlane.getFlightPlan().getEntryPoint().getY());

		angle = Math.toDegrees(Math.atan2(newPlane.getY() - newPlane.getTarget().getY(),
				newPlane.getX() - newPlane.getTarget().getX()));
		//System.out.println(angle);
		if(angle<0) {
			angle +=360;
		}
		newPlane.setBearing(angle);
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
							> (planeI.getAltitude() + 0.4))
					|| (planeJ.getAltitude()
							< (planeI.getAltitude() - 0.4))) {
				continue;
			}

			distIJSqr = Math.pow(planeJ.getX() - planeI.getX(), 2)
					+ Math.pow(planeJ.getY() - planeI.getY(), 2);

			if(distIJSqr < Math.pow(this.separationDistance, 2)) {
				// Two planes have collided
				result[0] = true;
				return result;
			} else if(distIJSqr < Math.pow(this.penaltyDistance, 2)) {
				// Two planes are in penalty distance
				planeJ.setAlertStatus(true);
				risk = true; 
				if (penalty){
					if (this.score >= 2){
						this.score -= 2;
					}
					penalty = false;
				}
				
				}
			}
		
		
		planeI.setAlertStatus(risk);
		result[1] = risk;
		for (Plane p : this.currentPlanes){ 
			if (p.getAlertStatus()){
				penalty = false;
				break;
			}
			penalty = true;
		}
		return result;
	}
	
	/**
	 * Removes a plane from manual control
	 * 
	 * @param plane				the plane to remove from manual control
	 */
	public void removeFromManual(Plane plane) {
		while(this.manualPlanes.contains(plane)) {
			this.manualPlanes.remove(plane);
			plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
		}
	}
	
	public void deleteFromManual(Plane plane) {
		while(this.manualPlanes.contains(plane)) {
			this.manualPlanes.remove(plane);
		}
	}
	
	
	
	
	
	/**
	 * Updates the state
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 * @param delta				the time change between calls
	 */
	
	public void update(GameContainer gameContainer,
			StateBasedGame game) {
		ArrayList<Plane> planesToRemove = new ArrayList<Plane>();
		Waypoint tempNextVisibleTarget;
		

		
		// Spawn more planes when no planes present
		if(this.currentPlanes.size() == 0) {
			this.countToNextPlane = 0;
		}
		
		if(!this.collision && !gameContainer.isPaused()
				&& gameContainer.hasFocus()) {
			// Create planes			
			if (this.countToNextPlane == 0) {
				for(int i = 0; i < this.spawnCount; i++) {
					this.createPlane();
				}

				if(this.spawnRate == 0) {
					this.countToNextPlane = -1;
				} else {
					this.countToNextPlane = (15 * ((new Random()).nextInt(
							this.spawnRate / 2) + this.spawnRate));
				}
			}

			// Handle directional controls
			
			//NOTE: THESE NEED TO SET TURNINGLEFT AND TURNINGRIGHT TO FALSE FOR THE PLANE YOU ARE CONTROLLING
			if(this.currentPlane != null) {
				// Action on 'a' and 'left' keys
				if(gameContainer.getInput().isKeyDown(203)
						|| gameContainer.getInput().isKeyDown(30)) {
					if(!this.manualPlanes.contains(this.currentPlane)) {
						this.manualPlanes.add(this.currentPlane);
					}
					
					this.currentPlane.decrementBearing();
				}

				// Action on 'd' and 'right' keys
				if(gameContainer.getInput().isKeyDown(205)
						|| gameContainer.getInput().isKeyDown(32)) {
					if(!this.manualPlanes.contains(this.currentPlane)) {
						this.manualPlanes.add(this.currentPlane);
					}
					
					this.currentPlane.incrementBearing();
				}

				// Action on 'w' and 'up' keys
				if(gameContainer.getInput().isKeyPressed(200)
						|| gameContainer.getInput().isKeyPressed(17)) {
					this.currentPlane.incrementTargetAltitude();
				}
				
				// Action on 's' and 'down' keys
				if(gameContainer.getInput().isKeyPressed(208)
						|| gameContainer.getInput().isKeyPressed(31)) {
					this.currentPlane.decrementTargetAltitude();
				}
				
				//Action on 'l' Key
				
				if(gameContainer.getInput().isKeyPressed(38)) {
					if (this.currentPlane.isNeedsToLand()){
						this.currentPlane.landPlane();
					}
				}
			}
			
			
			
			// Action on TAB key
			if(gameContainer.getInput().isKeyPressed(15)) {
				if(currentPlane != null) {
					int index = 0;
					int planeCount = this
							.getCurrentPlanes().size();
				
					for(int i = 0; i < planeCount; i++) {
						if(this.getCurrentPlanes()
								.get(i) == this.currentPlane) {
							index = ((i + 1) % planeCount);
						}
					}
					
					this.currentPlane = this
							.getCurrentPlanes().get(index);
				} else {
					this.currentPlane = this
							.getCurrentPlanes().get(0);
				}
			}
			
			// Update planes
			for(Plane plane : this.getCurrentPlanes()) {
				

				// Check plane still in game area
				
				if(this.manualPlanes.contains(plane)
						&& ((plane.getX() > this.windowWidth)
						|| (plane.getX() < 0)
						|| (plane.getY() > this.windowHeight)
						|| (plane.getY() < 0))) {
					if (this.score >= 5){
						this.score -= 5;
					}
					planesToRemove.add(plane);
				}
				//if(plane.getVelocity()==0) {
				//	this.manualPlanes.add(plane);
				//}
				if(plane.equals(this.currentPlane)) { //<- this is what I'm talking about on google, you can only take of if you press an arrow first or right click
					if(plane.getVelocity()==0) {
						if(gameContainer.getInput().isKeyPressed(Input.KEY_T)) {
							
							plane.setVelocity(7000);
							this.manualPlanes.remove(plane);
							plane.setTakingOff(true);
							this.currentPlane = null;
							this.listOfEntryPoints.add(this.airport);
						}
					}
				}
				
				if(plane.getLandingDescentRate()!=0 ) {
					if(plane.getAltitude()<0) {
						plane.setAltitude(0);
						plane.setLandingDescentRate(0);
						plane.setTargetAltitude(0);
					}
					else{
						plane.setAltitude(plane.getAltitude()-(int)Math.round(plane.getLandingDescentRate()));
					}
					//System.out.println(plane.getAltitudeFromHeight(plane.getAltitude())-plane.getLandingDescentRate());
				}
				// Check if colliding with another plane
				/*if(this.collision(plane)) {
					this.currentPlane = null;
					this.collidedPlanes.add(plane);
					this.collision = true;
				}*/
				
				// If plane has no more waypoints, remove it
				if(plane.getFlightPlan().getCurrentRoute().size() == 0) {
					planesToRemove.add(plane);
					
					
				} else {
					// Check if plane at waypoint
					
					if(plane.checkIfFlightAtWaypoint(plane.getFlightPlan().getCurrentRoute().get(0), this)) {
						
						System.out.println("here");
						
						if(plane.getFlightPlan().getCurrentRoute().get(0).equals(this.airport.getBeginningOfRunway()) && plane.isTakingOff()){
							
							plane.setTakingOff(false);
							plane.setBearing(360);
							plane.setTargetAltitude(3000);
						}
						
						plane.getFlightPlan().getCurrentRoute().remove(0);
						if(plane.getFlightPlan().getCurrentRoute().size()!= 0){
						
							if (plane.getFlightPlan().getCurrentRoute().get(0).equals(this.airport.getBeginningOfRunway())&& plane.getFlightPlan().getCurrentRoute().size()==2){
								this.manualPlanes.add(plane);
								plane.setNeedsToLand(true);
							}
							

							
							plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
						}
						
						
						
						
						// Scoring 10 for exitpoints and airport, and 5 for normal waypoints
						if (plane.getFlightPlan().getCurrentRoute().size() == 0){
							this.score += 10;
						} else {
							this.score += 5; 
						}
						
					}
				}

				// Change altitude
					if(plane.getAltitude() > plane.getTargetAltitude()) {
						plane.decrementAltitude();
					} 
					else if(plane.getAltitude()<plane.getTargetAltitude()) {
						plane.incrementAltitude();
					}

				
				this.movePlane(plane);
				
			}
			
			// Remove planes
			for(Plane plane : planesToRemove) {
				this.deleteFromManual(plane);
				this.getCurrentPlanes().remove(plane);
			}

			this.countToNextPlane--;
		}
	}
	// ACCESSORS
	
	
		/**
		 * @return				separation distance (the collision range)
		 */
		public int getSeparationDistance() {
			return this.separationDistance;
		}
		
		public double getScore() {
			return score;
		}

		public void setScore(double score) {
			this.score = score;
		}

		public double getEndTime() {
			return endTime;
		}

		public void setEndTime(double endTime) {
			this.endTime = endTime;
		}

		public double getCountToNextPlane() {
			return countToNextPlane;
		}

		public void setCountToNextPlane(double countToNextPlane) {
			this.countToNextPlane = countToNextPlane;
		}

		public boolean isCollision() {
			return collision;
		}

		public void setCollision(boolean collision) {
			this.collision = collision;
		}

		public boolean isEnding() {
			return ending;
		}

		public void setEnding(boolean ending) {
			this.ending = ending;
		}

		public double getSpeedDifficulty() {
			return speedDifficulty;
		}

		public void setSpeedDifficulty(double speedDifficulty) {
			this.speedDifficulty = speedDifficulty;
		}

		public int getSpawnRate() {
			return spawnRate;
		}

		public void setSpawnRate(int spawnRate) {
			this.spawnRate = spawnRate;
		}

		public int getSpawnCount() {
			return spawnCount;
		}

		public void setSpawnCount(int spawnCount) {
			this.spawnCount = spawnCount;
		}

		public ArrayList<Plane> getManualPlanes() {
			return manualPlanes;
		}

		public void setManualPlanes(ArrayList<Plane> manualPlanes) {
			this.manualPlanes = manualPlanes;
		}

		public ArrayList<Plane> getCollidedPlanes() {
			return collidedPlanes;
		}

		public void setCollidedPlanes(ArrayList<Plane> collidedPlanes) {
			this.collidedPlanes = collidedPlanes;
		}

		public Plane getCurrentPlane() {
			return currentPlane;
		}

		public void setCurrentPlane(Plane currentPlane) {
			this.currentPlane = currentPlane;
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

		public ArrayList<Point> getListOfExitPoints() {
			return listOfExitPoints;
		}

		public void setListOfExitPoints(ArrayList<Point> listOfExitPoints) {
			this.listOfExitPoints = listOfExitPoints;
		}

		public ArrayList<Waypoint> getListOfWaypoints() {
			return listOfWaypoints;
		}

		public void setListOfWaypoints(ArrayList<Waypoint> listOfWaypoints) {
			this.listOfWaypoints = listOfWaypoints;
		}

		public ArrayList<Point> getListOfEntryPoints() {
			return listOfEntryPoints;
		}

		public void setListOfEntryPoints(ArrayList<Point> listOfEntryPoints) {
			this.listOfEntryPoints = listOfEntryPoints;
		}

		public Airport getAirport() {
			return airport;
		}
		
		
}
