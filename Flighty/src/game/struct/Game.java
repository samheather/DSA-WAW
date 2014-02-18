package game.struct;

import game.gfx.GameWindow;

import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Random;

import org.newdawn.slick.GameContainer;
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
	
	/** How long can a play stay landed before penalty applies*/
	private static final int TAKE_OFF_PENALTY_TIME = 1500;

	/** Array list containing airspace exit points */
	private ArrayList<Point> listOfExitPoints = new ArrayList<Point>();

	/** Array list containing airspace waypoints */
	private ArrayList<Waypoint> listOfWaypoints = new ArrayList<Waypoint>();

	/** Array list containing airspace entry points */
	private ArrayList<Point> listOfEntryPoints = new ArrayList<Point>();

	/** Distance at which planes crash */
	private int separationDistance;

	/** Distance at which warning ring around appears */
	private int penaltyDistance;

	/** List of planes currently in the game */
	private ArrayList<Plane> currentPlanes = new ArrayList<Plane>();

	/** Reference to the game window */
	private GameWindow currentGameWindow;

	/** The width the game is displayed at */
	public int windowWidth;

	/** The height the game is displayed at */
	public int windowHeight;

	/** Whether the player should be penalised **/
	private boolean penalty;

	/** Penalty for delaying to take off the planes */
	private boolean takeOffPenalty;

	/** Synchronisation variable to measure the taking off delay*/
	private int takeOffSynch;
	
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

	/** Holds number of planes currently in the airspace */
	int planeCount;

	/** Holds airport in airspace */
	private Airport airport;
	
	/** Variable which holds score */
	private Score score;

	// Constructors

	/**
	 * Constructor for Game
	 * <p>
	 * Initialises carrier names.
	 * </p>
	 * 
	 * @param separationDistance
	 *            the distance at which planes should collide
	 * @param penaltyDistance
	 *            the distance at which planes should alert
	 * @param currentGameWindow
	 *            reference to the current game window
	 */
	public Game(int separationDistance, int penaltyDistance)
	{
		// Screen size
		this.windowWidth 				= 1200;
		this.windowHeight 				= 600;
		
		// This sets the game difficulty
		this.separationDistance 		= separationDistance;
		this.penaltyDistance 			= penaltyDistance;
		
		// Dynamic lists of points 
		this.listOfExitPoints 			= new ArrayList<Point>();
		this.listOfWaypoints 			= new ArrayList<Waypoint>();
		this.listOfEntryPoints 			= new ArrayList<Point>();
		
		// Whether score penalties apply
		this.penalty 					= true;
		
		// Penalty for not taking off the planes in time
		this.takeOffPenalty = false;
		this.takeOffSynch = TAKE_OFF_PENALTY_TIME;
		
		// New plane added shortly after startup
		this.countToNextPlane			= 0;
		
		// Game over conditions
		this.collision 					= false;
		this.ending 					= false;
		
		// Adding the airport into the gmae
		this.airport 					= new Airport(); 
		
		// Dynamic lists for planes
		this.manualPlanes = new ArrayList<Plane>();
		this.collidedPlanes = new ArrayList<Plane>();
		
		// Airspace is empty at startup
		this.currentPlane = null;
		this.planeCount = 0;

		// Adding Points To Game
		this.listOfEntryPoints.add(new EntryPoint(   0, 400));
		this.listOfEntryPoints.add(new EntryPoint(1200, 200));
		this.listOfEntryPoints.add(new EntryPoint( 600,   0));
		this.listOfEntryPoints.add(this.airport);

		this.listOfWaypoints.add(new Waypoint( 250, 150));
		this.listOfWaypoints.add(new Waypoint( 100, 100));
		this.listOfWaypoints.add(new Waypoint( 950, 140));
		this.listOfWaypoints.add(new Waypoint( 100, 350));
		this.listOfWaypoints.add(new Waypoint( 550, 200));
		this.listOfWaypoints.add(new Waypoint( 400, 310));
		this.listOfWaypoints.add(new Waypoint( 700, 310));
		this.listOfWaypoints.add(new Waypoint( 100, 550));
		this.listOfWaypoints.add(new Waypoint(1000, 330));
		this.listOfWaypoints.add(new Waypoint( 760, 150));

		this.listOfExitPoints.add(this.airport);
		this.listOfExitPoints.add(new ExitPoint( 800,   0));
		this.listOfExitPoints.add(new ExitPoint(   0, 200));
		this.listOfExitPoints.add(new ExitPoint(1200, 300));
		
		// Initialise score
		this.score = new Score();
	}

	// METHODS

	/**
	 * Creates a plane
	 * <p>
	 * Performs the following actions:
	 * <ul>
	 * <li>Initialises velocity and bearing</li>
	 * <li>Generates unique ID: a carrier string concatenated with a 2 digit
	 * randomised number.</li>
	 * <li>Checks against other plane's ID to insure it is unique</li>
	 * <li>Generates a random crew number</li>
	 * <li>Generates a random starting position on the edge of the map</li>
	 * <li>Generates flight plan consisting of {@link game.struct.Waypoint}
	 * nodes</li>
	 * <li>Adds plane to list currentPlanes</li>
	 * </ul>
	 * </p>
	 * 
	 * @param testing
	 *            use <code>true</code> if testing, or <code>false</code>
	 *            otherwise
	 * @return plane's ID
	 */
	public void createPlane()
	{
		Plane newPlane;
		this.planeCount++;

		newPlane = new Plane(this.planeCount, this.generateVelocity(),
								this.generateAltitude(), 0, this);

		if (newPlane.getFlightPlan().getEntryPoint() == this.airport)
		{
			this.configurePlaneForTakeOff(newPlane);
		}

		newPlane.calculateBearingToNextWaypoint();
		newPlane.setBearing(newPlane.getTargetBearing());

		// Add new plane to the game
		this.currentPlanes.add(newPlane);
	}
	/**
	 * Configure plane to take off properly
	 * 
	 * It adds the runway waypoints to its flight plan,
	 * inserts the flight in the airport spot
	 * and signals that a plane needs to take off
	 * @param newPlane - a new plane that needs to take off
	 */
	public void configurePlaneForTakeOff(Plane newPlane)
	{
		newPlane.getFlightPlan().setEntryPoint(new EntryPoint(1180, 580));
		
		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, this.airport.getBeginningOfRunway());
		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, this.airport.getEndOfRunway());

		newPlane.setX(1180);
		newPlane.setY( 580);
	
		newPlane.setTarget(newPlane.getFlightPlan().getCurrentRoute().get(0));
		newPlane.setVelocity(0);
		newPlane.setAltitude(0);
		newPlane.setTargetAltitude(0);
		newPlane.setNeedsToTakeOff(true);
		
		/* Airport is removed from list of entrypoints so another flight can't
		 spawn on airport until current plane has left.*/
		this.listOfEntryPoints.remove(this.airport);
	}

	/**
	 * Randomly assigns plane one of three fixed velocities
	 * 
	 * @return Velocity value
	 */
	public double generateVelocity()
	{
		double velocity;
		int random = new Random().nextInt(3);
		
		switch (random)
		{
		case 0:
			velocity = 1;
			break;
		case 1:
			velocity = 1.1;
			break;
		case 2:
			velocity = 1.2;
			break;
		default:
			velocity = 1;
			break;
		}

		return velocity;
	}

	/**
	 * Randomly generates altitude in range [2000, 7000], with increments of
	 * 1000
	 * 
	 * @return Random altitude
	 */
	public int generateAltitude()
	{
		int altitude;
		
		altitude = 2000 + (int) ((Math.random()) * 6) * 1000;
		
		return altitude;
	}

	/**
	 * Removes a plane from the game
	 * <p>
	 * Iterates through currentPlanes list and removes the plane
	 * </p> 
	 * @param toDelete
	 *            the plane to remove
	 */
	public void removePlane(Plane toDelete)
	{
		for (ListIterator<Plane> iter = this.currentPlanes
				.listIterator(this.currentPlanes.size()); iter.hasPrevious();)
		{
			if (toDelete.equals(iter.previous()))
			{
				iter.remove();
				return;
			}
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
	 * @param ID
	 *            the ID of the plane to find
	 * @return plane specified by id
	 */
	public Plane getPlaneFromID(int ID)
	{
		for (Plane plane : this.currentPlanes)
		{
			if (plane.getID() == ID)
			{
				return plane;
			}
		}
		
		// If ID not in array, return null
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
	 * <li>Iterates through currentPlanes, ignoring the plane under test and
	 * planes that are on different altitudes
	 * <li>Calculates the distance between them and plane
	 * <li>If the distance is less than the separation distance, mark the plane
	 * as colliding
	 * <li>If the distance is less than the penalty distance, mark the plane as
	 * alerting</li>
	 * </ul>
	 * </p>
	 * 
	 * @param planeI
	 *            the plane to test
	 * @return <code>true</code> if the plane is colliding, <code>false</code>
	 *         otherwise
	 */
	public boolean collision(Plane planeI)
	{
		return this.collisionHelper(planeI)[0];
	}

	/**
	 * Helper method for {@link #collision(Plane)}
	 * <p>
	 * Tests whether planes are within either the penalty or separation
	 * distance, and marks the plane as such
	 * </p>
	 * <p>
	 * If check is set to <code>true</code>, this will not update images (but
	 * will still return the check result).
	 * </p>
	 * 
	 * @param plane1
	 *            the plane to test
	 * @return an array of size 2, with the 0th element set if the plane is
	 *         colliding, and the 1th element set if the plane is alerting
	 */
	public boolean[] collisionHelper(Plane plane1)
	{
		// Squared distance between 2 planes
		double distIJSqr;
		boolean risk = false;

		// First -> are planes colliding? {1:true, 0:false}
		// Second -> result used for testing purposes {1:true, 0:false}
		boolean[] result = new boolean[] { false, false };

		//Loops through all the planes
		for (Plane plane2 : this.currentPlanes)
		{
			if ((plane1.equals(plane2))
					|| (plane2.getAltitude() > (plane1.getAltitude() + 400))
					|| (plane2.getAltitude() < (plane1.getAltitude() - 400)))
			{
				continue;
			}

			// Calculates the distance between 2 planes
			distIJSqr = Math.pow(plane2.getX() - plane1.getX(), 2)
						+ Math.pow(plane2.getY() - plane1.getY(), 2);

			// Calculates if two planes have collided
			if (distIJSqr < Math.pow(this.separationDistance, 2))
			{
				result[0] = true;
				return result;
			}
			//Calculates if two planes are in penalty distance
			else if (distIJSqr < Math.pow(this.penaltyDistance, 2))
			{
				plane2.setAlertStatus(true);
				risk = true;
				
				// Applying score penalties for violating the penalty distance
				if (penalty)
				{
					this.getScore().planeCollisionWarningMultAndScorePenalties();
					
					penalty = false;
					plane2.setViolationOccurred();
				}
			}
		}

		plane1.setAlertStatus(risk);
		result[1] = risk;
		
		for (Plane p : this.currentPlanes)
		{
			if (p.getAlertStatus())
			{
				//Removes the penalty temporary so the user doesn't get penalised for the same violation
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
	 * @param plane
	 *            the plane to remove from manual control
	 */
	public void removeFromManual(Plane plane)
	{
		while (this.manualPlanes.contains(plane))
		{
			this.manualPlanes.remove(plane);
			if (plane.getFlightPlan().getCurrentRoute().size() != 0)
			{
				plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
			}

		}
	}

	/**
	 * Deletes plane from manual
	 * 
	 * @param plane
	 *            The plane to be deleted from manual
	 */
	public void deleteFromManual(Plane plane)
	{
		while (this.manualPlanes.contains(plane))
		{
			this.manualPlanes.remove(plane);
		}
	}

	public void handleKeyPresses(GameContainer gameContainer)
	{
		//Steering controls apply only to active planes
		if (!this.currentPlane.isNeedsToTakeOff())
		{
			// Action on 'a' and 'left' keys
			if (gameContainer.getInput().isKeyDown(203)
					|| gameContainer.getInput().isKeyDown(30))
			{

				if (!this.manualPlanes.contains(this.currentPlane))
				{
					this.manualPlanes.add(this.currentPlane);
				}

				this.currentPlane.decrementBearing();
			}

			// Action on 'd' and 'right' keys
			if (gameContainer.getInput().isKeyDown(205)
					|| gameContainer.getInput().isKeyDown(32))
			{

				if (!this.manualPlanes.contains(this.currentPlane))
				{
					this.manualPlanes.add(this.currentPlane);
				}

				this.currentPlane.incrementBearing();
			}

			// Action on 'w' and 'up' keys
			if (gameContainer.getInput().isKeyPressed(200)
					|| gameContainer.getInput().isKeyPressed(17))
			{
				this.currentPlane.incrementTargetAltitude();
			}

			// Action on 's' and 'down' keys
			if (gameContainer.getInput().isKeyPressed(208)
					|| gameContainer.getInput().isKeyPressed(31))
			{
				this.currentPlane.decrementTargetAltitude();
			}

			// Action on 'l' Key
			if (gameContainer.getInput().isKeyPressed(38))
			{
				if (this.currentPlane.isNeedsToLand())
				{
					this.currentPlane.land();
				}
			}

		}

		// Action on 'T' Key
		else if (this.currentPlane.isNeedsToTakeOff())
		{
			if (gameContainer.getInput().isKeyDown(Input.KEY_T))
			{
				this.currentPlane.takeOff();
			}

		}
	}

	/**
	 * Updates the state
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 * @param delta
	 *            the time change between calls
	 */
	public void update(GameContainer gameContainer, StateBasedGame game)
	{
		ArrayList<Plane> planesToRemove = new ArrayList<Plane>();

		// Spawn more planes when no planes present
		if (this.currentPlanes.size() == 0)
		{
			this.countToNextPlane = 0;
		}

		if (!this.collision && !gameContainer.isPaused()
					&& gameContainer.hasFocus())
		{
			// Create planes
			if (this.countToNextPlane == 0)
			{
				for (int i = 0; i < this.spawnCount; i++)
				{
					this.createPlane();
				}
				
				// If the spawn rate is set to 0, no planes are added
				if (this.spawnRate == 0)
				{
					this.countToNextPlane = -1;
				}
				// If the spawn rate is more than 0, planes are added according to it
				else
				{
					Random rand = new Random();
					this.countToNextPlane = (rand.nextInt(360) + this.spawnRate * 60);
				}
			}

			// Handle directional controls
			if (this.currentPlane != null)
			{
				this.handleKeyPresses(gameContainer);
			}
		}

		// Update planes
		for (Plane plane : this.getCurrentPlanes())
		{
			// Check if the plane is still in the game area
			if (this.manualPlanes.contains(plane)
					&& ((plane.getX() > this.windowWidth) || (plane.getX() < 0)
							|| (plane.getY() > this.windowHeight) || (plane.getY() < 0)))
			{
				// Updates score if plane in game area
				this.getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();

				// Deselects plane that left the airspace
				if (this.currentPlane != null)
				{
					if (plane.equals(this.currentPlane))
					{
						this.currentPlane = null;
					}
				}
				
				// Removes planes that left the airspace
				planesToRemove.add(plane);
			}

			// Updating the Planes altitude and adjusting it accordingly.
			plane.updatePlaneAltitude();

			// Check if colliding with another plane
			if (this.collision(plane))
			{
				this.currentPlane = null;
				this.collidedPlanes.add(plane);
				this.collision = true;
			}

			/* If plane has no more waypoints, remove it as it means it went through its exitpoint
			 * and update the multiplier if the plane doesn't go through any violations
			 */
			if (plane.getFlightPlan().getCurrentRoute().size() == 0)
			{
				this.getScore().planePilotedPerfectlyMultiplierBonus(plane);

				if (this.currentPlane != null 
						&& plane.equals(this.currentPlane))
				{
					this.currentPlane = null;
				}
				
				planesToRemove.add(plane);

			}
			else
			{
				
				// Check if plane at waypoint
				if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan()
						.getCurrentRoute().get(0), this))
				{
					
					// REFACTOR
					this.getScore().addScore(plane, this);
					
					
					// Accommodates planes that are taking off 
					if (plane.getFlightPlan().getCurrentRoute().get(0)
							.equals(this.airport.getBeginningOfRunway())
								&& plane.isTakingOff())
					{
						plane.setTakingOff(false);
						plane.setBearing(360);
						plane.setTargetAltitude(3000);
					
						// Allows other planes to be created at airport
						this.listOfEntryPoints.add(this.airport);
					}

					// Removes reached waypoint
					plane.getFlightPlan().getCurrentRoute().remove(0);

					// If plane has waypoints left in its flight plan
					if (plane.getFlightPlan().getCurrentRoute().size() != 0)
					{
						// Checks if plane needs to land
						if (plane.getFlightPlan().getCurrentRoute().get(0)
								.equals(this.airport.getBeginningOfRunway())
									&& plane.getFlightPlan().getCurrentRoute()
										.size() == 2)
						{
							// Planes that need to be landed are controlled manually
							if (!this.manualPlanes.contains(plane))
							{
								this.manualPlanes.add(plane);
							}
							
							// Signals that plane needs to land
							plane.setNeedsToLand(true);
						}
						
						plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
					}
					else 
					{
						if (plane.isLanding())
						{
							this.getAirport().setPlaneLanding(false);
						}
					}

				}
			}

			// Applies penalty for planes landed for too long
			if (plane.isNeedsToTakeOff())
			{
				if (this.takeOffSynch < 0)
				{
					this.takeOffPenalty = true;
					this.takeOffSynch = TAKE_OFF_PENALTY_TIME;
				}
				
				this.takeOffSynch --;
			}
			
			
			// Updates the plane position
			plane.movePlane();
		}

		// Remove planes
		for (Plane plane : planesToRemove)
		{
			this.deleteFromManual(plane);
			this.getCurrentPlanes().remove(plane);
		}

		this.countToNextPlane--;
	}

	// GETTERS
	
	public Score getScore(){
		return this.score;
	}

	public void setTakeOffPenalty(boolean takeOffPenalty) {
		this.takeOffPenalty = takeOffPenalty;
	}

	public boolean isTakeOffPenalty() {
		return takeOffPenalty;
	}

	/**
	 * @return separation distance (the collision range)
	 */
	public int getSeparationDistance()
	{
		return this.separationDistance;
	}

	/**
	 * @return Time when game ended
	 */
	public double getEndTime()
	{
		return endTime;
	}

	/**
	 * @param endTime
	 *            Value which endTime is to be changed to
	 */
	public void setEndTime(double endTime)
	{
		this.endTime = endTime;
	}

	/**
	 * @return Loop count till next plane can be added
	 */
	public double getCountToNextPlane()
	{
		return countToNextPlane;
	}

	/**
	 * @param countToNextPlane
	 *            Value which countToNextPlane is to be changed to
	 */
	public void setCountToNextPlane(double countToNextPlane)
	{
		this.countToNextPlane = countToNextPlane;
	}

	/**
	 * @return Boolean representing whether a collision has occurred
	 */
	public boolean isCollision()
	{
		return collision;
	}

	/**
	 * @param collision
	 *            Value which collision is to be changed to
	 */
	public void setCollision(boolean collision)
	{
		this.collision = collision;
	}

	/**
	 * @return ending
	 */
	public boolean isEnding()
	{
		return ending;
	}

	/**
	 * @param ending
	 *            Value which ending is to be changed to
	 */
	public void setEnding(boolean ending)
	{
		this.ending = ending;
	}

	/**
	 * @return speedDifficulty
	 */
	public double getSpeedDifficulty()
	{
		return speedDifficulty;
	}

	/**
	 * @param speedDifficulty
	 *            Value which speedDifficulty is to be changed to
	 */
	public void setSpeedDifficulty(double speedDifficulty)
	{
		this.speedDifficulty = speedDifficulty;
	}

	/**
	 * @return spawnRate
	 */
	public int getSpawnRate()
	{
		return spawnRate;
	}

	/**
	 * @param spawnRate
	 *            Value which spawnRate is to be changed to
	 */
	public void setSpawnRate(int spawnRate)
	{
		this.spawnRate = spawnRate;
	}

	/**
	 * @return spawnCount
	 */
	public int getSpawnCount()
	{
		return spawnCount;
	}

	/**
	 * @param spawnCount
	 *            Value which spawnCount is to be changed to
	 */
	public void setSpawnCount(int spawnCount)
	{
		this.spawnCount = spawnCount;
	}

	/**
	 * @return manualPlanes
	 */
	public ArrayList<Plane> getManualPlanes()
	{
		return manualPlanes;
	}

	/**
	 * @param manualPlanes
	 *            Array list which manual planes is to be changed to
	 */
	public void setManualPlanes(ArrayList<Plane> manualPlanes)
	{
		this.manualPlanes = manualPlanes;
	}

	/**
	 * @return collidedPlanes
	 */
	public ArrayList<Plane> getCollidedPlanes()
	{
		return collidedPlanes;
	}

	/**
	 * @param collidedPlanes
	 *            Array list which collidedPlanes is to be changed to
	 */
	public void setCollidedPlanes(ArrayList<Plane> collidedPlanes)
	{
		this.collidedPlanes = collidedPlanes;
	}

	/**
	 * @return currentPlane
	 */
	public Plane getCurrentPlane()
	{
		return currentPlane;
	}

	/**
	 * @param currentPlane
	 *            Plane which currentPlane is to be changed to
	 */
	public void setCurrentPlane(Plane currentPlane)
	{
		this.currentPlane = currentPlane;
	}

	/**
	 * @return penalty distance (the alert range)
	 */
	public int getPenaltyDistance()
	{
		return this.penaltyDistance;
	}

	/**
	 * @return list of planes attached to the game
	 */
	public ArrayList<Plane> getCurrentPlanes()
	{
		return this.currentPlanes;
	}

	/**
	 * @return a reference to the current game window
	 */
	public GameWindow getCurrentGameWindow()
	{
		return this.currentGameWindow;
	}

	// SETTERS

	/**
	 * @param separationDistance
	 *            distance at which planes should collide
	 */
	public void setSeparationDistance(int separationDistance)
	{
		this.separationDistance = separationDistance;
	}

	/**
	 * @param penaltyDistance
	 *            distance at which planes should alert
	 */
	public void setPenaltyDistance(int penaltyDistance)
	{
		this.penaltyDistance = penaltyDistance;
	}

	/**
	 * @param currentPlanes
	 *            the array of planes to set
	 */
	public void setCurrentPlanes(ArrayList<Plane> currentPlanes)
	{
		this.currentPlanes = currentPlanes;
	}

	/**
	 * @param currentGameWindow
	 *            a new parent game window
	 */
	public void setCurrentGameWindow(GameWindow currentGameWindow)
	{
		this.currentGameWindow = currentGameWindow;
	}

	/**
	 * @return Array list of exit points
	 */
	public ArrayList<Point> getListOfExitPoints()
	{
		return listOfExitPoints;
	}

	/**
	 * @param listOfExitPoints
	 *            Array list which listOfExitPoints is to be changed to
	 */
	public void setListOfExitPoints(ArrayList<Point> listOfExitPoints)
	{
		this.listOfExitPoints = listOfExitPoints;
	}

	/**
	 * @return Array list of waypoints in the airspace
	 */
	public ArrayList<Waypoint> getListOfWaypoints()
	{
		return listOfWaypoints;
	}

	/**
	 * @param listOfWaypoints
	 *            Array list which listOfWaypoints is to be changed to
	 */
	public void setListOfWaypoints(ArrayList<Waypoint> listOfWaypoints)
	{
		this.listOfWaypoints = listOfWaypoints;
	}

	/**
	 * @return Array list of entry points in the airspace
	 */
	public ArrayList<Point> getListOfEntryPoints()
	{
		return listOfEntryPoints;
	}

	/**
	 * @param listOfEntryPoints
	 *            Array list which listOfEntryPoints is to be changed to
	 */
	public void setListOfEntryPoints(ArrayList<Point> listOfEntryPoints)
	{
		this.listOfEntryPoints = listOfEntryPoints;
	}

	/**
	 * @return Airport within in the airspace
	 */
	public Airport getAirport()
	{
		return airport;
	}
}
