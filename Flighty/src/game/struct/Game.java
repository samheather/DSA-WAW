package game.struct;

import game.gfx.GameWindow;
import game.gfx.WindowManager;

import java.io.IOException;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;
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
public abstract class Game {

	/** How long can a play stay landed before penalty applies */
	private static final int TAKE_OFF_PENALTY_TIME = 1500;

	/** The window width */
	private static final int WINDOW_WIDTH = 1200;

	/** The window height */
	private static final int WINDOW_HEIGHT = 600;

	/** Distance from left edge for sidebar so planes don't fly in it */
	protected static int distFromLeftEdge = 0;

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

	/** Synchronisation variable to measure the taking off delay */
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

	/** A list of planes which are colliding */
	private ArrayList<Plane> collidedPlanes;

	/** The plane currently being controlled by the player */
	protected Plane currentPlane;

	/** Holds number of planes currently in the airspace */
	private int planeCount;

	/** Holds airport in airspace */
	private Airport airport;

	/** Variable which holds score */
	private Score score;
	SecureRandom secureRandom;
	private Random rand;

	protected abstract Airport createAirport();

	protected abstract ArrayList<Point> createExitPoints();

	protected abstract ArrayList<Waypoint> createWayPoints();

	protected abstract ArrayList<Point> createEntryPoints();

	protected abstract void configurePlane(Plane p);

	protected abstract void planeUpdate(Plane p, GameContainer gameContainer);

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
	 * @throws NoSuchAlgorithmException
	 */
	protected Game(int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft) throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		WindowManager.endingText = "";
		secureRandom = SecureRandom.getInstance("SHA1PRNG");
		ByteBuffer b = ByteBuffer.allocate(8).put(secureRandom.generateSeed(8));
		b.rewind();
		rand = new Random(b.getLong());
		// Screen size
		windowWidth = WINDOW_WIDTH;
		windowHeight = WINDOW_HEIGHT;

		// Used for the side bar
		distFromLeftEdge = distFromLeft;

		// This sets the game difficulty
		separationDistance = newSeparationDistance;
		penaltyDistance = newPenaltyDistance;

		// Dynamic lists of points
		listOfExitPoints = new ArrayList<Point>();
		listOfWaypoints = new ArrayList<Waypoint>();
		listOfEntryPoints = new ArrayList<Point>();

		// Whether score penalties apply
		penalty = true;

		// Penalty for not taking off the planes in time
		takeOffPenalty = false;
		takeOffSynch = TAKE_OFF_PENALTY_TIME;

		// New plane added shortly after startup
		countToNextPlane = 0;

		// Game over conditions
		collision = false;
		ending = false;

		// Adding the airport into the game
		airport = createAirport();

		collidedPlanes = new ArrayList<Plane>();

		// Airspace is empty at startup
		currentPlane = null;
		planeCount = 0;

		// Adding Points To Game
		listOfEntryPoints.add(new EntryPoint(distFromLeftEdge, 400));
		listOfEntryPoints.add(airport);

		// Single player extra things

		listOfEntryPoints.addAll(createEntryPoints());
		listOfWaypoints.addAll(createWayPoints());
		listOfExitPoints.addAll(createExitPoints());

		listOfWaypoints.add(new Waypoint(400, 150));
		listOfWaypoints.add(new Waypoint(250, 100));
		listOfWaypoints.add(new Waypoint(250, 350));
		listOfWaypoints.add(new Waypoint(550, 310));
		listOfWaypoints.add(new Waypoint(550, 550));

		listOfExitPoints.add(airport);
		listOfExitPoints.add(new ExitPoint(distFromLeftEdge, 200));

		// Initialise score
		score = new Score();
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
	public void createPlane() {
		Plane newPlane;
		setPlaneCount(getPlaneCount() + 1);

		newPlane = constructPlane(planeCount, generateVelocity(),
				generateAltitude(), 0, rand.nextLong());
		configurePlane(newPlane);
		if (newPlane.getFlightPlan().getEntryPoint() == airport) {
			configurePlaneForTakeOff(newPlane);
		}

		newPlane.calculateBearingToNextWaypoint();
		newPlane.setBearing(newPlane.getTargetBearing());
	}

	protected abstract Plane constructPlane(int id, double velocity,
			int altitude, double bearing, long uniqueNetworkObjectId);

	/**
	 * Configure plane to take off properly
	 * 
	 * It adds the runway waypoints to its flight plan, inserts the flight in
	 * the airport spot and signals that a plane needs to take off
	 * 
	 * @param newPlane
	 *            - a new plane that needs to take off
	 */
	public void configurePlaneForTakeOff(Plane newPlane) {
		newPlane.getFlightPlan().setEntryPoint(
				new EntryPoint(Airport.getEndOfRunwayX(),
						Airport.getRunwayY() + 30));

		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, airport.getBeginningOfRunway());
		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, airport.getEndOfRunway());

		newPlane.setX(newPlane.getFlightPlan().getEntryPoint().x);
		newPlane.setY(newPlane.getFlightPlan().getEntryPoint().y);

		newPlane.setTarget(newPlane.getFlightPlan().getCurrentRoute().get(0));
		newPlane.setVelocity(0);
		newPlane.setAltitude(0);
		newPlane.setTargetAltitude(0);
		newPlane.setNeedsToTakeOff(true);

		/*
		 * Airport is removed from list of entrypoints so another flight can't
		 * spawn on airport until current plane has left.
		 */
		listOfEntryPoints.remove(airport);
	}

	/**
	 * Randomly assigns plane one of three fixed velocities
	 * 
	 * @return Velocity value
	 */
	public double generateVelocity() {
		double velocity;
		int random = new Random().nextInt(3);

		switch (random) {
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
	public int generateAltitude() {
		int altitude;

		altitude = 2000 + (int) ((Math.random()) * 6) * 1000;

		return altitude;
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
	public Plane getPlaneFromID(int ID) {
		for (Plane plane : getCurrentPlanes()) {
			if (plane.getID() == ID) {
				return plane;
			}
		}

		// If ID not in array, return null
		return null;
	}

	// TODO(David) Is this misplaced? Should be in testing?
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
	public boolean collision(Plane planeI) {
		return collisionHelper(planeI)[0];
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
	public boolean[] collisionHelper(Plane plane1) {
		// Squared distance between 2 planes
		double distIJSqr;
		boolean risk = false;

		// First -> are planes colliding? {1:true, 0:false}
		// Second -> result used for testing purposes {1:true, 0:false}
		boolean[] result = new boolean[] { false, false };

		// Loops through all the planes
		for (Plane plane2 : getCurrentPlanes()) {
			if (!plane1.ownedByCurrentPlayer) {
				break;
			}
			if ((plane1.equals(plane2))
					|| (plane2.getAltitude() > (plane1.getAltitude() + 400))
					|| (plane2.getAltitude() < (plane1.getAltitude() - 400))
					|| (plane2.ownedByCurrentPlayer == false)) {
				continue;
			}

			// Calculates the distance between 2 planes
			distIJSqr = Math.pow(plane2.getX() - plane1.getX(), 2)
					+ Math.pow(plane2.getY() - plane1.getY(), 2);

			// Calculates if two planes have collided
			if (distIJSqr < Math.pow(separationDistance, 2)) {
				result[0] = true;
				return result;
			}
			// Calculates if two planes are in penalty distance
			else if (distIJSqr < Math.pow(penaltyDistance, 2)) {
				plane2.setAlertStatus(true);
				risk = true;

				// Applying score penalties for violating the penalty distance
				if (penalty) {
					if (plane2.ownedByCurrentPlayer)
						getScore().planeCollisionWarningMultAndScorePenalties();

					penalty = false;
					plane2.setViolationOccurred();
				}
			}
		}

		plane1.setAlertStatus(risk);
		result[1] = risk;

		for (Plane p : getCurrentPlanes()) {
			if (p.getAlertStatus()) {
				// Removes the penalty temporary so the user doesn't get
				// penalised for the same violation
				penalty = false;
				break;
			}

			penalty = true;
		}

		return result;
	}

	/**
	 * Checks if any plane on screen needs to take off
	 * 
	 * @return Plane any plane that needs to take off
	 */

	private Plane planeNeedsToTakeOff() {
		for (Plane p : getCurrentPlanes()) {
			if (p.getNeedsToTakeOff() && p.ownedByCurrentPlayer) {
				return p;
			}
		}
		return null;
	}

	public void handleKeyPresses(GameContainer gameContainer) {
		if (gameContainer.getInput().isKeyDown(Input.KEY_T)
				&& planeNeedsToTakeOff() != null) {
			Plane takeoffPlane = planeNeedsToTakeOff();
			takeoffPlane.takeOff();
			takeoffPlane.markForSyncing();
		}

		if (currentPlane == null)
			return;
		// Steering controls apply only to active planes
		if (!currentPlane.getNeedsToTakeOff()) {
			// Action on 'a' and 'left' keys
			if (gameContainer.getInput().isKeyDown(203)
					|| gameContainer.getInput().isKeyDown(30)) {

				currentPlane.setManual();

				currentPlane.decrementBearing();
				currentPlane.markForSyncing();
			}

			// Action on 'd' and 'right' keys
			if (gameContainer.getInput().isKeyDown(205)
					|| gameContainer.getInput().isKeyDown(32)) {

				currentPlane.setManual();

				currentPlane.incrementBearing();
				currentPlane.markForSyncing();
			}

			// Action on 'w' and 'up' keys
			if (gameContainer.getInput().isKeyPressed(200)
					|| gameContainer.getInput().isKeyPressed(17)) {
				currentPlane.incrementTargetAltitude();
				currentPlane.markForSyncing();
			}

			// Action on 's' and 'down' keys
			if (gameContainer.getInput().isKeyPressed(208)
					|| gameContainer.getInput().isKeyPressed(31)) {
				currentPlane.decrementTargetAltitude();
				currentPlane.markForSyncing();
			}

			// Action on 'L' Key
			if (gameContainer.getInput().isKeyPressed(38)) {
				if (currentPlane.getNeedsToLand()) {
					currentPlane.land();
				}
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
	 * @throws IOException
	 */
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {

		// Spawn more planes when no planes present
		if (getCurrentPlanes().size() == 0) {
			countToNextPlane = 0;
		}

		if (!collision && !gameContainer.isPaused() && gameContainer.hasFocus()) {
			// Create planes
			if (countToNextPlane <= 1) {
				for (int i = 0; i < spawnCount; i++) {
					createPlane();
				}

				// If the spawn rate is set to 0, no planes are added
				if (spawnRate == 0) {
					countToNextPlane = -1;
				}
				// If the spawn rate is more than 0, planes are added according
				// to it
				else {
					Random rand = new Random();
					countToNextPlane = (rand.nextInt(360) + spawnRate * 60);
				}
			}

			// Handle directional controls
			handleKeyPresses(gameContainer);
		}

		// Update planes
		for (Plane plane : getCurrentPlanes()) {
			// Check if the plane is still in the game area

			planeUpdate(plane, gameContainer);

			// Updating the Planes altitude and adjusting it accordingly.
			plane.updatePlaneAltitude();

			// Check if colliding with another plane
			if (collision(plane)) {
				currentPlane = null;
				collidedPlanes.add(plane);
				collision = true;
			}

			/*
			 * If plane has no more waypoints, remove it as it means it went
			 * through its exitpoint and update the multiplier if the plane
			 * doesn't go through any violations
			 */
			if (plane.getFlightPlan().getCurrentRoute().size() == 0) {
				if (plane.ownedByCurrentPlayer)
					getScore().planePilotedPerfectlyMultiplierBonus(plane);

				if (currentPlane != null && plane.equals(currentPlane)) {
					currentPlane = null;
				}
				plane.markForDeletion();

			} else {

				// Check if plane at waypoint
				if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan()
						.getCurrentRoute().get(0), this)) {
					if (plane.ownedByCurrentPlayer)
						getScore().addScore(plane, this);

					// Accommodates planes that are taking off
					if (plane.getFlightPlan().getCurrentRoute().get(0)
							.equals(airport.getBeginningOfRunway())
							&& plane.isTakingOff()) {
						plane.setTakingOff(false);
						plane.setBearingForTakeoff();
						plane.setTargetAltitude(2000);

						// Allows other planes to be created at airport
						listOfEntryPoints.add(airport);
					}

					// Removes reached waypoint
					plane.getFlightPlan().getCurrentRoute().remove(0);

					// If plane has waypoints left in its flight plan
					if (plane.getFlightPlan().getCurrentRoute().size() != 0) {
						// Checks if plane needs to land
						if (plane.getFlightPlan().getCurrentRoute().get(0)
								.equals(airport.getBeginningOfRunway())
								&& plane.getFlightPlan().getCurrentRoute()
										.size() == 2) {
							// Planes that need to be landed are controlled
							// manually
							plane.setManual();

							// Signals that plane needs to land
							plane.setNeedsToLand(true);
						}

						plane.setTarget(plane.getFlightPlan().getCurrentRoute()
								.get(0));
					} else {
						if (plane.isLanding()) {
							getAirport().setPlaneLanding(false);
						}
					}

				}
			}

			// Applies penalty for planes landed for too long
			if (plane.getNeedsToTakeOff()) {
				if (takeOffSynch < 0) {
					takeOffPenalty = true;
					takeOffSynch = TAKE_OFF_PENALTY_TIME;
				}

				takeOffSynch--;
			}

			// Updates the plane position
			plane.movePlane();
			/*
			 * if (plane.needsSyncing()) {
			 * 
			 * ByteBuffer toTransmit = plane.serialize(); int i =
			 * toTransmit.limit(); toTransmit.rewind(); ByteBuffer b =
			 * ByteBuffer.allocate(4); b.putInt(i); b.rewind();
			 * os.write(b.array()); os.write(toTransmit.array(), 0, i);
			 * os.write("\r\n".getBytes());
			 * 
			 * System.out.println("I should not be blocking");
			 * oos.writeObject(plane);
			 * System.out.println("I should not be blocking here either");
			 * plane.resetSyncState();
			 * 
			 * }
			 */
		}

		countToNextPlane--;
	}

	// All general Accessors

	/**
	 * 
	 * @return Score object
	 */
	public Score getScore() {
		return score;
	}

	/**
	 * Has this plane been sitting on the runway long enough to now have a
	 * take-off penalty?
	 * 
	 * @return
	 */
	public boolean isTakeOffPenalty() {
		return takeOffPenalty;
	}

	/**
	 * @return separation distance (the collision range)
	 */
	public int getSeparationDistance() {
		return separationDistance;
	}

	/**
	 * @return Time when game ended
	 */
	public double getEndTime() {
		return endTime;
	}

	/**
	 * @return Loop count till next plane can be added
	 */
	public double getCountToNextPlane() {
		return countToNextPlane;
	}

	/**
	 * @return Boolean representing whether a collision has occurred
	 */
	public boolean isCollision() {
		return collision;
	}

	/**
	 * @return ending
	 */
	public boolean isEnding() {
		return ending;
	}

	/**
	 * @return speedDifficulty
	 */
	public double getSpeedDifficulty() {
		return speedDifficulty;
	}

	/**
	 * @return spawnRate
	 */
	public int getSpawnRate() {
		return spawnRate;
	}

	/**
	 * @return spawnCount
	 */
	public int getSpawnCount() {
		return spawnCount;
	}

	/**
	 * @return collidedPlanes
	 */
	public ArrayList<Plane> getCollidedPlanes() {
		return collidedPlanes;
	}

	/**
	 * @return currentPlane
	 */
	public Plane getCurrentPlane() {
		return currentPlane;
	}

	/**
	 * @return penalty distance (the alert range)
	 */
	public int getPenaltyDistance() {
		return penaltyDistance;
	}

	/**
	 * @return Array list of exit points
	 */
	public ArrayList<Point> getListOfExitPoints() {
		return listOfExitPoints;
	}

	/**
	 * @return a reference to the current game window
	 */
	public GameWindow getCurrentGameWindow() {
		return currentGameWindow;
	}

	/**
	 * @return Array list of waypoints in the airspace
	 */
	public ArrayList<Waypoint> getListOfWaypoints() {
		return listOfWaypoints;
	}

	/**
	 * @return Array list of entry points in the airspace
	 */
	public ArrayList<Point> getListOfEntryPoints() {
		return listOfEntryPoints;
	}

	/**
	 * @return Airport within in the airspace
	 */
	public Airport getAirport() {
		return airport;
	}

	/**
	 * Get number of planes in the current airspace
	 * 
	 * @return number of planes in current airspaec
	 */
	public int getPlaneCount() {
		return planeCount;
	}

	// All general Mutators

	/**
	 * 
	 * @param takeOffPenalty
	 *            sets whether the user should be penalised for not taking off
	 */
	public void setTakeOffPenalty(boolean newTakeOffPenalty) {
		takeOffPenalty = newTakeOffPenalty;
	}

	/**
	 * @param endTime
	 *            Value which endTime is to be changed to
	 */
	public void setEndTime(double newEndTime) {
		endTime = newEndTime;
	}

	/**
	 * @param countToNextPlane
	 *            Value which countToNextPlane is to be changed to
	 */
	public void setCountToNextPlane(double newCountToNextPlane) {
		countToNextPlane = newCountToNextPlane;
	}

	/**
	 * @param collision
	 *            Value which collision is to be changed to
	 */
	public void setCollision(boolean newCollision) {
		collision = newCollision;
	}

	/**
	 * @param ending
	 *            Value which ending is to be changed to
	 */
	public void setEnding(boolean newEnding) {
		ending = newEnding;
	}

	/**
	 * @param speedDifficulty
	 *            Value which speedDifficulty is to be changed to
	 */
	public void setSpeedDifficulty(double newSpeedDifficulty) {
		speedDifficulty = newSpeedDifficulty;
	}

	/**
	 * @param spawnRate
	 *            Value which spawnRate is to be changed to
	 */
	public void setSpawnRate(int newSpawnRate) {
		spawnRate = newSpawnRate;
	}

	/**
	 * @param spawnCount
	 *            Value which spawnCount is to be changed to
	 */
	public void setSpawnCount(int newSpawnCount) {
		spawnCount = newSpawnCount;
	}

	public void clearManualPlanes() {
		for (Plane i : getCurrentPlanes()) {
			i.setAuto();
		}
	}

	/**
	 * @param collidedPlanes
	 *            Array list which collidedPlanes is to be changed to
	 */
	public void setCollidedPlanes(ArrayList<Plane> newCollidedPlanes) {
		collidedPlanes = newCollidedPlanes;
	}

	/**
	 * @param currentPlane
	 *            Plane which currentPlane is to be changed to
	 */
	public void setCurrentPlane(Plane newCurrentPlane) {
		currentPlane = newCurrentPlane;
	}

	/**
	 * @return list of planes attached to the game
	 */
	public abstract List<? extends Plane> getCurrentPlanes();

	/**
	 * @param separationDistance
	 *            distance at which planes should collide
	 */
	public void setSeparationDistance(int newSeparationDistance) {
		separationDistance = newSeparationDistance;
	}

	/**
	 * @param penaltyDistance
	 *            distance at which planes should alert
	 */
	public void setPenaltyDistance(int newPenaltyDistance) {
		penaltyDistance = newPenaltyDistance;
	}

	/**
	 * @param currentGameWindow
	 *            a new parent game window
	 */
	public void setCurrentGameWindow(GameWindow newCurrentGameWindow) {
		currentGameWindow = newCurrentGameWindow;
	}

	/**
	 * @param listOfExitPoints
	 *            Array list which listOfExitPoints is to be changed to
	 */
	public void setListOfExitPoints(ArrayList<Point> newListOfExitPoints) {
		listOfExitPoints = newListOfExitPoints;
	}

	/**
	 * @param listOfWaypoints
	 *            Array list which listOfWaypoints is to be changed to
	 */
	public void setListOfWaypoints(ArrayList<Waypoint> newListOfWaypoints) {
		listOfWaypoints = newListOfWaypoints;
	}

	/**
	 * @param listOfEntryPoints
	 *            Array list which listOfEntryPoints is to be changed to
	 */
	public void setListOfEntryPoints(ArrayList<Point> newListOfEntryPoints) {
		listOfEntryPoints = newListOfEntryPoints;
	}

	public void setPlaneCount(int newPlaneCount) {
		planeCount = newPlaneCount;
	}

	public abstract void endingRoutine();
}
