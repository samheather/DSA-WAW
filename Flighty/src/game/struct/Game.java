package game.struct;

import game.gfx.GameWindow;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Input;
import org.newdawn.slick.state.StateBasedGame;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.KryoSerializable;
import com.esotericsoftware.kryo.io.Output;

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
public class Game implements java.io.Serializable, KryoSerializable {

	private void writeObject(ObjectOutputStream out) throws IOException {

	}

	private void readObject(ObjectInputStream in) throws IOException {

	}

	/** How long can a play stay landed before penalty applies */
	private static final int TAKE_OFF_PENALTY_TIME = 1500;

	/** The window width */
	private static final int WINDOW_WIDTH = 1200;

	/** The window height */
	private static final int WINDOW_HEIGHT = 600;
	
	private static boolean multiplayer = false;
	
	/** Distance from left edge for sidebar so planes don't fly in it */
	private static int distFromLeftEdge = 0;  

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

	/** A list of planes under manual control */
	private ArrayList<Plane> manualPlanes;

	/** A list of planes which are colliding */
	private ArrayList<Plane> collidedPlanes;

	/** The plane currently being controlled by the player */
	private Plane currentPlane;

	/** Holds number of planes currently in the airspace */
	private int planeCount;

	/** Holds airport in airspace */
	private Airport airport;

	/** Variable which holds score */
	private Score score;
	SecureRandom secureRandom;
	private Random rand;

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

	private Socket s;
	private InputStream is;
	private OutputStream os;
	private ConcurrentLinkedQueue<Plane> queue = new ConcurrentLinkedQueue<Plane>();
	ObjectOutputStream oos;
	ObjectInputStream ois;

	public Game(int newSeparationDistance, int newPenaltyDistance, int distFromLeft)
			throws NoSuchAlgorithmException, UnknownHostException, IOException {
		secureRandom = SecureRandom.getInstance("SHA1PRNG");
		ByteBuffer b = ByteBuffer.allocate(8).put(secureRandom.generateSeed(8));
		b.rewind();
		rand = new Random(b.getLong());
		// Screen size
		windowWidth = WINDOW_WIDTH;
		windowHeight = WINDOW_HEIGHT;
		
		
		distFromLeftEdge = distFromLeft;
		if (distFromLeftEdge != 0) {
			multiplayer = true;
		}
		/*

		// Initialise TCP Connection
		s = new Socket("teaching0.york.ac.uk", 1025);

		is = s.getInputStream();
		Thread t = new Thread(new SyncReceiver(queue, is));
		t.start();

		System.out.println("SLOG lol");

		os = s.getOutputStream();
		oos = new ObjectOutputStream(os);

		System.out.println("SLOG rofl");
		*/

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
		if (multiplayer) {
			airport = new Airport(415, 515, 150, 200, 100);
		} else {
			airport = new Airport(720, 460, 1180, 320, 230);
		}
		

		// Dynamic lists for planes
		manualPlanes = new ArrayList<Plane>();
		collidedPlanes = new ArrayList<Plane>();

		// Airspace is empty at startup
		currentPlane = null;
		planeCount = 0;

		// Adding Points To Game
		listOfEntryPoints.add(new EntryPoint(distFromLeftEdge, 400));
		listOfEntryPoints.add(new EntryPoint(1200, 200));
		listOfEntryPoints.add(new EntryPoint(750, 0));
		listOfEntryPoints.add(airport);

		listOfWaypoints.add(new Waypoint(400, 150));
		listOfWaypoints.add(new Waypoint(250, 100));
		listOfWaypoints.add(new Waypoint(1100, 140));
		listOfWaypoints.add(new Waypoint(250, 350));
		listOfWaypoints.add(new Waypoint(700, 200));
		listOfWaypoints.add(new Waypoint(550, 310));
		listOfWaypoints.add(new Waypoint(850, 310));
		listOfWaypoints.add(new Waypoint(550, 550));
		listOfWaypoints.add(new Waypoint(1150, 330));
		listOfWaypoints.add(new Waypoint(910, 150));

		listOfExitPoints.add(airport);
		listOfExitPoints.add(new ExitPoint(950, 0));
		listOfExitPoints.add(new ExitPoint(distFromLeftEdge, 200));
		listOfExitPoints.add(new ExitPoint(1200, 300));

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

		newPlane = new Plane(planeCount, generateVelocity(),
				generateAltitude(), 0, this, rand.nextLong());

		if (newPlane.getFlightPlan().getEntryPoint() == airport) {
			configurePlaneForTakeOff(newPlane);
		}

		newPlane.calculateBearingToNextWaypoint();
		newPlane.setBearing(newPlane.getTargetBearing());

		// Add new plane to the game
		currentPlanes.add(newPlane);
	}

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
		newPlane.getFlightPlan().setEntryPoint(new EntryPoint(1180, 580));

		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, airport.getBeginningOfRunway());
		newPlane.getFlightPlan().getCurrentRoute()
				.add(0, airport.getEndOfRunway());

		newPlane.setX(1180);
		newPlane.setY(580);

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
	 * Removes a plane from the game
	 * <p>
	 * Iterates through currentPlanes list and removes the plane
	 * </p>
	 * 
	 * @param toDelete
	 *            the plane to remove
	 */
	public void removePlane(Plane toDelete) {
		for (ListIterator<Plane> iter = currentPlanes
				.listIterator(currentPlanes.size()); iter.hasPrevious();) {
			if (toDelete.equals(iter.previous())) {
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
	public Plane getPlaneFromID(int ID) {
		for (Plane plane : currentPlanes) {
			if (plane.getID() == ID) {
				return plane;
			}
		}

		// If ID not in array, return null
		return null;
	}

	// TODO v Is this misplaced ?
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
		for (Plane plane2 : currentPlanes) {
			if ((plane1.equals(plane2))
					|| (plane2.getAltitude() > (plane1.getAltitude() + 400))
					|| (plane2.getAltitude() < (plane1.getAltitude() - 400))) {
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
					getScore().planeCollisionWarningMultAndScorePenalties();

					penalty = false;
					plane2.setViolationOccurred();
				}
			}
		}

		plane1.setAlertStatus(risk);
		result[1] = risk;

		for (Plane p : currentPlanes) {
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
	 * Removes a plane from manual control
	 * 
	 * @param plane
	 *            the plane to remove from manual control
	 */
	public void removeFromManual(Plane plane) {
		// Loop while there is at last a plane in the manual control
		while (manualPlanes.contains(plane)) {
			// and remove it
			manualPlanes.remove(plane);

			// Make the unselected plane go to the next waypoint
			if (plane.getFlightPlan().getCurrentRoute().size() != 0) {
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
	public void deleteFromManual(Plane plane) {
		while (manualPlanes.contains(plane)) {
			manualPlanes.remove(plane);
		}
	}

	public void handleKeyPresses(GameContainer gameContainer) {
		if(currentPlane == null)
			return;
		// Steering controls apply only to active planes
		if (!currentPlane.getNeedsToTakeOff()) {
			// Action on 'a' and 'left' keys
			if (gameContainer.getInput().isKeyDown(203)
					|| gameContainer.getInput().isKeyDown(30)) {

				if (!manualPlanes.contains(currentPlane)) {
					manualPlanes.add(currentPlane);
				}

				currentPlane.decrementBearing();
				//currentPlane.markForSyncing();
			}

			// Action on 'd' and 'right' keys
			if (gameContainer.getInput().isKeyDown(205)
					|| gameContainer.getInput().isKeyDown(32)) {

				if (!manualPlanes.contains(currentPlane)) {
					manualPlanes.add(currentPlane);
				}

				currentPlane.incrementBearing();
				//currentPlane.markForSyncing();
			}

			// Action on 'w' and 'up' keys
			if (gameContainer.getInput().isKeyPressed(200)
					|| gameContainer.getInput().isKeyPressed(17)) {
				currentPlane.incrementTargetAltitude();
				//currentPlane.markForSyncing();
			}

			// Action on 's' and 'down' keys
			if (gameContainer.getInput().isKeyPressed(208)
					|| gameContainer.getInput().isKeyPressed(31)) {
				currentPlane.decrementTargetAltitude();
				//currentPlane.markForSyncing();
			}

			// Action on 'l' Key
			if (gameContainer.getInput().isKeyPressed(38)) {
				if (currentPlane.getNeedsToLand()) {
					currentPlane.land(multiplayer);
					//currentPlane.markForSyncing();
				}
			}

		}

		// Action on 'T' Key
		else if (currentPlane.getNeedsToTakeOff()) {
			if (gameContainer.getInput().isKeyPressed(Input.KEY_T)) {
				currentPlane.takeOff();
				//currentPlane.markForSyncing();
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
		ArrayList<Plane> planesToRemove = new ArrayList<Plane>();
		// Spawn more planes when no planes present
		if (currentPlanes.size() == 0) {
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
			if (currentPlane != null) {
				handleKeyPresses(gameContainer);
			}
		}
		Plane p = null;
		while ((p = queue.poll()) != null) {
			System.out.println("Got a plane");
			p.currentGame = this;
			p.resetSyncState();
			ListIterator<Plane> i = getCurrentPlanes().listIterator();
			while (i.hasNext()) {
				Plane p2 = i.next();
				if (p == null)
					System.out.println("p is null");
				if (p2 == null)
					System.out.println("p2 is null");
				if (p2.getUniqueNetworkObjectID() == p
						.getUniqueNetworkObjectID()) {
					i.set(p);
					p = null;
					break;
				}
			}
			if (p != null){
				getCurrentPlanes().add(p);
			}
		}
		// Update planes
		for (Plane plane : getCurrentPlanes()) {
			// Check if the plane is still in the game area
				
			if ((plane.getX() > windowWidth) || (plane.getX() < distFromLeftEdge)
							|| (plane.getY() > windowHeight) || (plane.getY() < 0)) {
				// Updates score if plane in game area
				getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();

				// Deselects plane that left the airspace
				if (currentPlane != null) {
					if (plane.equals(currentPlane)) {
						currentPlane = null;
					}
				}

				// Removes planes that left the airspace
				planesToRemove.add(plane);
			}
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
				getScore().planePilotedPerfectlyMultiplierBonus(plane);

				if (currentPlane != null && plane.equals(currentPlane)) {
					currentPlane = null;
				}

				planesToRemove.add(plane);

			} else {

				// Check if plane at waypoint
				if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan()
						.getCurrentRoute().get(0), this)) {

					getScore().addScore(plane, this);
					

					// Accommodates planes that are taking off
					if (plane.getFlightPlan().getCurrentRoute().get(0)
							.equals(airport.getBeginningOfRunway())
							&& plane.isTakingOff()) {
						plane.setTakingOff(false);
						plane.setBearing(360);
						plane.setTargetAltitude(3000);

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
							if (!manualPlanes.contains(plane)) {
								manualPlanes.add(plane);
							}

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
			if (plane.needsSyncing()) {
				
				 ByteBuffer toTransmit = plane.serialize(); int i =
				 toTransmit.limit(); toTransmit.rewind(); ByteBuffer b =
				 ByteBuffer.allocate(4); b.putInt(i); b.rewind();
				 os.write(b.array()); os.write(toTransmit.array(), 0, i);
				 os.write("\r\n".getBytes());
				 
				System.out.println("I should not be blocking");
				oos.writeObject(plane);
				System.out.println("I should not be blocking here either");
				plane.resetSyncState();

			}
		*/
		}

		// Remove planes
		for (Plane plane : planesToRemove) {
			deleteFromManual(plane);
			getCurrentPlanes().remove(plane);
		}

		countToNextPlane--;
	}

	// GETTERS

	/**
	 * 
	 * @return Score object
	 */
	public Score getScore() {
		return score;
	}

	/**
	 * 
	 * @param takeOffPenalty
	 *            sets whether the user should be penalised for not taking off
	 */
	public void setTakeOffPenalty(boolean newTakeOffPenalty) {
		takeOffPenalty = newTakeOffPenalty;
	}

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
	 * @param endTime
	 *            Value which endTime is to be changed to
	 */
	public void setEndTime(double newEndTime) {
		endTime = newEndTime;
	}

	/**
	 * @return Loop count till next plane can be added
	 */
	public double getCountToNextPlane() {
		return countToNextPlane;
	}

	/**
	 * @param countToNextPlane
	 *            Value which countToNextPlane is to be changed to
	 */
	public void setCountToNextPlane(double newCountToNextPlane) {
		countToNextPlane = newCountToNextPlane;
	}

	/**
	 * @return Boolean representing whether a collision has occurred
	 */
	public boolean isCollision() {
		return collision;
	}

	/**
	 * @param collision
	 *            Value which collision is to be changed to
	 */
	public void setCollision(boolean newCollision) {
		collision = newCollision;
	}

	/**
	 * @return ending
	 */
	public boolean isEnding() {
		return ending;
	}

	/**
	 * @param ending
	 *            Value which ending is to be changed to
	 */
	public void setEnding(boolean newEnding) {
		ending = newEnding;
	}

	/**
	 * @return speedDifficulty
	 */
	public double getSpeedDifficulty() {
		return speedDifficulty;
	}

	/**
	 * @param speedDifficulty
	 *            Value which speedDifficulty is to be changed to
	 */
	public void setSpeedDifficulty(double newSpeedDifficulty) {
		speedDifficulty = newSpeedDifficulty;
	}

	/**
	 * @return spawnRate
	 */
	public int getSpawnRate() {
		return spawnRate;
	}

	/**
	 * @param spawnRate
	 *            Value which spawnRate is to be changed to
	 */
	public void setSpawnRate(int newSpawnRate) {
		spawnRate = newSpawnRate;
	}

	/**
	 * @return spawnCount
	 */
	public int getSpawnCount() {
		return spawnCount;
	}

	/**
	 * @param spawnCount
	 *            Value which spawnCount is to be changed to
	 */
	public void setSpawnCount(int newSpawnCount) {
		spawnCount = newSpawnCount;
	}

	/**
	 * @return manualPlanes
	 */
	public ArrayList<Plane> getManualPlanes() {
		return manualPlanes;
	}

	/**
	 * @param manualPlanes
	 *            Array list which manual planes is to be changed to
	 */
	public void setManualPlanes(ArrayList<Plane> newManualPlanes) {
		manualPlanes = newManualPlanes;
	}

	/**
	 * @return collidedPlanes
	 */
	public ArrayList<Plane> getCollidedPlanes() {
		return collidedPlanes;
	}

	/**
	 * @param collidedPlanes
	 *            Array list which collidedPlanes is to be changed to
	 */
	public void setCollidedPlanes(ArrayList<Plane> newCollidedPlanes) {
		collidedPlanes = newCollidedPlanes;
	}

	/**
	 * @return currentPlane
	 */
	public Plane getCurrentPlane() {
		return currentPlane;
	}

	/**
	 * @param currentPlane
	 *            Plane which currentPlane is to be changed to
	 */
	public void setCurrentPlane(Plane newCurrentPlane) {
		currentPlane = newCurrentPlane;
	}

	/**
	 * @return penalty distance (the alert range)
	 */
	public int getPenaltyDistance() {
		return penaltyDistance;
	}

	/**
	 * @return list of planes attached to the game
	 */
	public ArrayList<Plane> getCurrentPlanes() {
		return currentPlanes;
	}

	/**
	 * @return a reference to the current game window
	 */
	public GameWindow getCurrentGameWindow() {
		return currentGameWindow;
	}

	// SETTERS

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
	 * @param currentPlanes
	 *            the array of planes to set
	 */
	public void setCurrentPlanes(ArrayList<Plane> setCurrentPlanes) {
		currentPlanes = setCurrentPlanes;
	}

	/**
	 * @param currentGameWindow
	 *            a new parent game window
	 */
	public void setCurrentGameWindow(GameWindow newCurrentGameWindow) {
		currentGameWindow = newCurrentGameWindow;
	}

	/**
	 * @return Array list of exit points
	 */
	public ArrayList<Point> getListOfExitPoints() {
		return listOfExitPoints;
	}

	/**
	 * @param listOfExitPoints
	 *            Array list which listOfExitPoints is to be changed to
	 */
	public void setListOfExitPoints(ArrayList<Point> newListOfExitPoints) {
		listOfExitPoints = newListOfExitPoints;
	}

	/**
	 * @return Array list of waypoints in the airspace
	 */
	public ArrayList<Waypoint> getListOfWaypoints() {
		return listOfWaypoints;
	}

	/**
	 * @param listOfWaypoints
	 *            Array list which listOfWaypoints is to be changed to
	 */
	public void setListOfWaypoints(ArrayList<Waypoint> newListOfWaypoints) {
		listOfWaypoints = newListOfWaypoints;
	}

	/**
	 * @return Array list of entry points in the airspace
	 */
	public ArrayList<Point> getListOfEntryPoints() {
		return listOfEntryPoints;
	}

	/**
	 * @param listOfEntryPoints
	 *            Array list which listOfEntryPoints is to be changed to
	 */
	public void setListOfEntryPoints(ArrayList<Point> newListOfEntryPoints) {
		listOfEntryPoints = newListOfEntryPoints;
	}

	/**
	 * @return Airport within in the airspace
	 */
	public Airport getAirport() {
		return airport;
	}

	public int getPlaneCount() {
		return planeCount;
	}

	public void setPlaneCount(int newPlaneCount) {
		planeCount = newPlaneCount;
	}

	@Override
	public void read(Kryo arg0, com.esotericsoftware.kryo.io.Input arg1) {

	}

	@Override
	public void write(Kryo arg0, Output arg1) {

	}
}

