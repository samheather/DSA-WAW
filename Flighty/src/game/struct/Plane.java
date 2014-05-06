package game.struct;

/**
 * Plane class
 */
public abstract class Plane {

	/**
	 * The high angle of the green cone that indicates valid space to give the
	 * order to land in a multiplayer game.
	 */
	protected int takeoffAngleHighMulti = 345;

	/**
	 * The low angle of the green cone that indicates valid space to give the
	 * order to land in a multiplayer game.
	 */
	protected int takeoffAngleLowMulti = 15;

	/**
	 * The high angle of the green cone that indicates valid space to give the
	 * order to land in a singleplayer game.
	 */
	protected int takeoffAngleHighSingle = 225;

	/**
	 * The low angle of the green cone that indicates valid space to give the
	 * order to land in a singleplayer game.
	 */
	protected int takeoffAngleLowSingle = 135;

	/** Is this plane under manual control? */
	private boolean manual = false;

	/** Boolean indicating if the plane needs to be deleted. */
	private boolean deleted = false;

	/** Unique identifier */
	private int id;

	/**
	 * A unique ID for the plane that is the same across all clients in a
	 * networked game.
	 */
	private final long uniqueNetworkObjectID;

	/** Size to display plane */
	private int size;

	/** Speed the plane is traveling at */
	private double velocity;

	/** Current altitude */
	private int altitude;

	/** Current bearing in radians */
	private double bearing;

	/** Target bearing in radians */
	private double targetBearing;

	/** Turning right boolean */
	private boolean turningRight;

	/** Turning left boolean */
	private boolean turningLeft;

	/** Current X co-ordinate */
	private double x;

	/** Current Y co-ordinate */
	private double y;

	/** Is the plane alerting */
	private boolean alertStatus;

	/** Current destination */
	private Point target = new Point();

	/** Current target altitude */
	private double targetAltitude;

	/** Necessary for creation of plane's flight plan */
	private FlightPlan flightPlan;

	/** Boolean to express whether plane is currently landing */
	private boolean landing;

	/** Boolean to express whether a plane needs to land */
	private boolean needsToLand;

	/** Boolean to express whether a plane is currently taking off */
	private boolean takingOff;

	/** Boolean to express whether a plane needs to take off */
	private boolean needsToTakeOff;

	/** Boolean to express whether a plane has ever broken the separation rules */
	private boolean violationOccurred;

	/** Necessary for calculation of plane's landing descent rate */
	private double landingDescentRate;

	/** Required by Slick2D */
	public transient Game currentGame;

	public boolean ownedByCurrentPlayer = false;

	public Plane() {
		uniqueNetworkObjectID = 0;
	}

	/**
	 * Constructor for Plane
	 * <p>
	 * Initialises the plane's position and general information about the
	 * flight.
	 * </p>
	 * 
	 * @param id
	 *            a unique identifier
	 * @param size
	 *            the size to display the plane at
	 * @param velocity
	 *            the speed at which the plane is traveling
	 * @param altitude
	 *            the altitude at which the plane is traveling
	 * @param bearing
	 *            the bearing the plane is following (in <b>radians</b>)
	 * @param x
	 *            the x position to create the plane at
	 * @param y
	 *            the y position to create the plane at
	 */
	protected Plane(int id, double velocity, int altitude, double bearing,
			Game currentGame, long uniqueNetworkObjectId) {
		this.currentGame = currentGame;
		this.id = id;
		this.uniqueNetworkObjectID = uniqueNetworkObjectId;
		this.size = 2;
		this.velocity = velocity;
		this.altitude = altitude;
		this.bearing = bearing;
		this.targetBearing = bearing;
		this.flightPlan = new FlightPlan(currentGame, this);
		this.x = this.flightPlan.getEntryPoint().getX();
		this.y = this.flightPlan.getEntryPoint().getY();
		this.targetAltitude = altitude;
		this.turningLeft = false;
		this.turningRight = false;
		this.landing = false;
		this.needsToLand = false;
		this.needsToTakeOff = false;
		this.takingOff = false;
		this.landingDescentRate = 0;
		this.violationOccurred = false;

		if (this.flightPlan.getCurrentRoute().size() != 0) { // Forces every new
																// plane to head
																// towards its
																// first
																// waypoint
			this.target = this.flightPlan.getCurrentRoute().get(0);
		}
	}

	// METHODS

	/**
	 * Determine whether a plane is currently at waypoint
	 * 
	 * @param waypoint
	 *            The waypoint which is currently being checked
	 * @param game
	 *            Required by Slick2D library
	 * @return Boolean which is set to true if a flight is currently at the
	 *         waypoint; false otherwise
	 */
	public boolean checkIfFlightAtWaypoint(Point waypoint, Game game) {
		// Ensuring that the plane cannot go through its landing waypoint when
		// it isnt landing
		if (waypoint.equals(game.getAirport().getBeginningOfRunway())
				&& isLanding() != true && isTakingOff() != true) {
			return false;
		}

		// If the plane is in the range, the flight is at waypoint
		if (((Math.abs(Math.round(getX()) - Math.round(waypoint.getX()))) <= 15)
				&& (Math.abs(Math.round(getY()) - Math.round(waypoint.getY()))) <= 15) {
			return true;
		}

		return false;
	}

	/**
	 * Increments the Plane's bearing by 5 degrees
	 */
	public void incrementBearing() {
		// Increment turning by going right
		setTurningLeft(false);
		setTurningRight(true);

		setBearing(getBearing() + 1);
		setTargetBearing(getBearing());

		// Resets the bearing if it is bigger than 360 degrees
		if (bearing >= 360) {
			setBearing(0);
			setTargetBearing(0);
		}
	}

	/**
	 * Decrements the Plane's bearing by 5 degrees
	 */
	public void decrementBearing() {
		// Decrement bearing by going left
		setTurningLeft(true);
		setTurningRight(false);

		setBearing(getBearing() - 1);
		setTargetBearing(getBearing());

		// Resets the bearing if it is bigger than 360 degrees
		if (bearing < 0) {
			setBearing(359);
			setTargetBearing(359);
		}
	}

	/**
	 * Increments the Plane's altitude to the next flight level
	 * <p>
	 * Note: highest flight level allowed for planes is 4
	 * </p>
	 */
	public void incrementAltitude() {
		setAltitude(getAltitude() + 5);
	}

	/**
	 * Decrements the Plane's target altitude to the next flight level
	 * <p>
	 * Note: lowest flight level allowed for planes is 1
	 * </p>
	 */
	public void decrementAltitude() {
		setAltitude(getAltitude() - 5);
	}

	/**
	 * Increments the Plane's target altitude to the next flight level
	 * <p>
	 * Note: highest flight level allowed for planes is 6000
	 * </p>
	 */
	public void incrementTargetAltitude() {
		if (getTargetAltitude() <= 6000) {
			setTargetAltitude(getTargetAltitude() + 1000);
		}
		markForSyncing();
	}

	/**
	 * Decrements the altitude by 1000 units, so long as the current altitude is
	 * greater than or equal to 3000
	 */
	public void decrementTargetAltitude() {
		if (getTargetAltitude() >= 3000) {
			setTargetAltitude(getTargetAltitude() - 1000);
		}
	}

	/**
	 * Calculates bearing from plane's current position to its next waypoint
	 * This is done so planes follow their flight plan automatically
	 */
	public void calculateBearingToNextWaypoint() {
		if (!isManual()) {
			double angle;
			angle = Math.toDegrees(Math.atan2(getY() - target.getY(), getX()
					- target.getX()));

			if (angle < 0) {
				angle += 360;
			}
			setTurningLeft(false);
			setTurningRight(false);
			setTargetBearing(angle);
		}
	}

	/** Updates current bearing */
	public void updateCurrentBearing() {
		// Rate at which the plane changes its bearing
		double rate = 0.9;

		if (!isManual()) {

			if (Math.round(getTargetBearing()) <= Math.round(getBearing()) - 3
					|| Math.round(getTargetBearing()) >= Math
							.round(getBearing()) + 3) {
				/*
				 * If plane has been given a heading so no turning direction
				 * specified, below works out which one is quicker between
				 * turning left and turning right
				 */
				if (isTurningRight() == false && isTurningLeft() == false) {
					if (Math.abs(getTargetBearing() - getBearing()) == 180) {
						setTurningRight(true);
					} else if (getBearing() + 180 <= 359) {
						if (getTargetBearing() < getBearing() + 180
								&& getTargetBearing() > getBearing()) {
							setTurningRight(true);
						} else {
							setTurningLeft(true);
						}
					} else {
						if (getTargetBearing() > getBearing() - 180
								&& getTargetBearing() < getBearing()) {
							setTurningLeft(true);
						} else {
							setTurningRight(true);
						}
					}

				}

				// Change bearing if plane is already turning right or user has
				// told it to turn right
				if (isTurningRight() == true) {
					setBearing((getBearing() + rate) % 360);
				}

				// Change bearing if plane is already turning left or user has
				// told it to turn left
				if (isTurningLeft() == true) {
					setBearing((getBearing() - rate) % 360);
				}
			} else {
				// Do not change bearing if no commands have been given
				setTurningLeft(false);
				setTurningRight(false);
			}
		}
		if (bearing < 0) {
			setBearing(359);
			setTargetBearing(359);
		}
		if (bearing >= 360) {
			setBearing(0);
			setTargetBearing(0);
		}
	}

	/**
	 * Calculates the rate at which a plane has to descend, given its current
	 * altitude, such that by the time it reaches the runway, its altitude is 0
	 * 
	 * @return Rate at which plane needs to descend
	 */
	public double findLandingDescentRate() {
		double rate;

		// Find distance to runway waypoint
		double distanceFromRunway = Math.sqrt(Math.pow(getX()
				- currentGame.getAirport().getBeginningOfRunway().getX(), 2)
				+ Math.pow(getY()
						- currentGame.getAirport().getBeginningOfRunway()
								.getY(), 2));

		double descentPerPixel = getAltitude() / distanceFromRunway;

		rate = descentPerPixel * getVelocity()
				* currentGame.getSpeedDifficulty();

		return rate;
	}

	/**
	 * First checks that another plane is not landing, then checks whether plane
	 * is in valid position to initiate landing before finally checking that the
	 * plane's current bearing is such that the plane is facing the runway. If
	 * all these conditions are met, the plane begins to land.
	 */
	public void land() {
		if (allowedToLand()) {
			currentGame.getAirport().setPlaneLanding(true);

			setNeedsToLand(false);
			setLanding(true);
			setTarget(flightPlan.getCurrentRoute().get(0));

			calculateBearingToNextWaypoint();
			setLandingDescentRate(findLandingDescentRate());
			setAuto();
			currentGame.setCurrentPlane(null);
		}
		markForSyncing();
	}

	/**
	 * Method to take off. Plane is assigned a velocity upon taking off, and is
	 * deselected so the user can't change the course while the plane is in the
	 * process of taking off
	 */
	public void takeOff() {
		setVelocity(currentGame.generateVelocity());
		setAuto();

		setNeedsToTakeOff(false);
		setTakingOff(true);

		// Penalising stops because the plane has been commanded to take off
		currentGame.setTakeOffPenalty(false);

		markForSyncing();
	}

	/**
	 * Method to update altitude First it checks whether the plane is landing,
	 * in which case the altitude decreases according to the landing descent
	 * rate Otherwise, the altitude changes towards the desired altitude at a
	 * constant speed, incrementally.
	 */
	public void updatePlaneAltitude() {
		if (getLandingDescentRate() != 0) {
			if (getAltitude() < 0) {
				setAltitude(0);
				setLandingDescentRate(0);
				setTargetAltitude(0);
			} else {
				setAltitude(getAltitude()
						- (int) Math.round(getLandingDescentRate()));
			}

		} else {
			if (getAltitude() > getTargetAltitude()) {
				decrementAltitude();
			} else if (getAltitude() < getTargetAltitude()) {
				incrementAltitude();
			}
		}
	}

	/**
	 * Moves a plane
	 * <p>
	 * If the plane is under manual control, it will follow the bearing
	 * specified by the player.
	 * </p>
	 * <p>
	 * If the plane is following its flight path, it will tend towards its next
	 * target.
	 * </p>
	 * 
	 * @param plane
	 *            the plane to move
	 */
	public void movePlane() {
		// If Plane is taking off, don't change the bearing
		if (getAltitude() < 2000 && getTargetAltitude() > 0 && !isLanding()) {
			updateXYCoordinates();
		} else {
			if (getTarget() != null) {
				if (!isManual()) {
					// Get the angle to the next waypoint
					calculateBearingToNextWaypoint();
				}
				updateCurrentBearing();

				// Move the plane
				updateXYCoordinates();
			}
		}
	}

	/** Updates x and y coordinates */
	public void updateXYCoordinates() {
		setX((float) (getX() - (Math.cos(Math.toRadians(getBearing())) * (currentGame
				.getSpeedDifficulty() * getVelocity()))));

		setY((float) (getY() - (Math.sin(Math.toRadians(getBearing())) * (currentGame
				.getSpeedDifficulty() * getVelocity()))));
	}

	@Override
	public final boolean equals(Object obj) {
		if (this.uniqueNetworkObjectID == 0)
			return false;
		if (obj == null)
			return false;
		if (!(obj instanceof Plane))
			return false;
		Plane rhs = (Plane) obj;
		if (rhs.uniqueNetworkObjectID == 0)
			return false;
		return this.uniqueNetworkObjectID == rhs.uniqueNetworkObjectID;
	}

	@Override
	public final int hashCode() {
		return String.valueOf(uniqueNetworkObjectID).hashCode();
	}

	// Accessors and Mutators

	/**
	 * @return the size the plane displays at
	 */
	public int getSize() {
		return this.size;
	}

	/**
	 * @return the plane's current speed
	 */
	public double getVelocity() {
		return this.velocity;
	}

	/**
	 * @return the plane's current altitude
	 */
	public int getAltitude() {
		return this.altitude;
	}

	/**
	 * @return the plane's current bearing
	 */
	public double getBearing() {
		return this.bearing;
	}

	/**
	 * @return the plane's current X position
	 */
	public double getX() {
		return this.x;
	}

	/**
	 * @return the plane's current Y position
	 */
	public double getY() {
		return this.y;
	}

	/**
	 * @return <code>true</code> if the plane is alerting, <code>false</code>
	 *         otherwise
	 */
	public boolean getAlertStatus() {
		return this.alertStatus;
	}

	/**
	 * @return the plane's current destination
	 */
	public Point getTarget() {
		return this.target;
	}

	/**
	 * @return the plane's target altitude
	 */
	public double getTargetAltitude() {
		return this.targetAltitude;
	}

	public void clearFlightPlan() {
		this.flightPlan = null;
	}

	/**
	 * @return the Plane's unique ID
	 */
	public int getID() {
		return this.id;
	}

	/**
	 * @return the violationOccurred boolean
	 */
	public boolean getViolationOccurred() {
		return this.violationOccurred;
	}

	/**
	 * Set violationOccurred boolean to true
	 */
	public void setViolationOccurred() {
		this.violationOccurred = true;
	}

	/**
	 * @param id
	 *            the new unique ID
	 */
	public void setID(int id) {
		this.id = id;
	}

	/**
	 * @param size
	 *            the new display size
	 */
	public void setSize(int size) {
		this.size = size;
	}

	/**
	 * @param velocity
	 *            the new speed
	 */
	public void setVelocity(double velocity) {
		this.velocity = velocity;
	}

	/**
	 * @param altitude
	 *            the new altitude
	 */
	public void setAltitude(int altitude) {
		this.altitude = altitude;
	}

	/**
	 * @param bearing
	 *            the new bearing
	 */
	public void setBearing(double bearing) {
		this.bearing = bearing;
	}

	/**
	 * @param x
	 *            the new X co-ordinate
	 */
	public void setX(double x) {
		this.x = x;
	}

	/**
	 * @param y
	 *            the new Y co-ordinate
	 */
	public void setY(double y) {
		this.y = y;
	}

	/**
	 * @param status
	 *            <code>true</code> if the plane should be alerting,
	 *            <code>false</code> otherwise
	 */
	public void setAlertStatus(boolean status) {
		this.alertStatus = status;
	}

	/**
	 * @param target
	 *            the new target Waypoint
	 */
	public void setTarget(Point target) {
		this.target = target;
	}

	/**
	 * @param targetAltitude
	 *            the new target altitude
	 */
	public void setTargetAltitude(double targetAltitude) {
		this.targetAltitude = targetAltitude;
	}

	/**
	 * @return Plane's flight plan
	 */
	public FlightPlan getFlightPlan() {
		return flightPlan;
	}

	/**
	 * @return Boolean expressing whether plane needs to land
	 */
	public boolean getNeedsToLand() {
		return needsToLand;
	}

	/** Set boolean expressing whether plane needs to land */
	public void setNeedsToLand(boolean needsToLand) {
		this.needsToLand = needsToLand;
	}

	/** Sets plane's flight plan */
	public void setFlightPlan(FlightPlan flightPlan) {
		this.flightPlan = flightPlan;
	}

	/**
	 * @return Get plane's target bearing
	 */
	public double getTargetBearing() {
		return targetBearing;
	}

	/** Set plane's target bearing */
	public void setTargetBearing(double targetBearing) {
		this.targetBearing = targetBearing;
	}

	/**
	 * @return Boolean expressing whether plane is turning right
	 */
	public boolean isTurningRight() {
		return turningRight;
	}

	/**
	 * 
	 * @param turningRight
	 */
	public void setTurningRight(boolean turningRight) {
		this.turningRight = turningRight;
	}

	/**
	 * Returns a boolean indicating if a plane is turning left.
	 * 
	 * @return
	 */
	public boolean isTurningLeft() {
		return turningLeft;
	}

	/**
	 * Sets a plane as turning left.
	 * 
	 * @param turningLeft
	 */
	public void setTurningLeft(boolean turningLeft) {
		this.turningLeft = turningLeft;
	}

	/**
	 * Returns if this plane is now allowed to land.
	 * 
	 * @return
	 */
	public abstract boolean allowedToLand();

	/**
	 * Returns boolean indicating if a plane is landing.
	 * 
	 * @return
	 */
	public boolean isLanding() {
		return this.landing;
	}

	/**
	 * Sets a plane as landing
	 * 
	 * @param bool
	 */
	public void setLanding(boolean bool) {
		this.landing = bool;
	}

	/**
	 * Sets a plane as taking off.
	 * 
	 * @return
	 */
	public boolean isTakingOff() {
		return takingOff;
	}

	/**
	 * Sets a planes taking off state to the input parameter takingOff
	 * 
	 * @param takingOff
	 */
	public void setTakingOff(boolean takingOff) {
		this.takingOff = takingOff;
	}

	/**
	 * Returns a boolean indicating if a plane needs to take off.
	 * 
	 * @return
	 */
	public boolean getNeedsToTakeOff() {
		return needsToTakeOff;
	}

	/**
	 * Sets a planes needsToTakeOff parameter to the input parameter
	 * needsToTakeOff
	 * 
	 * @param needsToTakeOff
	 */
	public void setNeedsToTakeOff(boolean needsToTakeOff) {
		this.needsToTakeOff = needsToTakeOff;
	}

	/**
	 * Returns a double representing the landing descent rate.
	 * 
	 * @return
	 */
	public double getLandingDescentRate() {
		return landingDescentRate;
	}

	/**
	 * Sets the landing descent rate of this plane to the input parameter double
	 * landingDescentRate.
	 * 
	 * @param landingDescentRate
	 */
	public void setLandingDescentRate(double landingDescentRate) {
		this.landingDescentRate = landingDescentRate;
	}

	/**
	 * Returns getTakeoffValueHighMulti - angle for drawing green cone for
	 * airport.
	 * 
	 * @return
	 */
	public int getTakeoffValueHighMulti() {
		return this.takeoffAngleHighMulti;
	}

	/**
	 * Returns takeoffAngleLowMulti - angle for drawing green cone for airport.
	 * 
	 * @return
	 */
	public int getTakeoffValueLowMulti() {
		return this.takeoffAngleLowMulti;
	}

	/**
	 * Returns takeoffAngleHighSingle - angle for drawing green cone for
	 * airport.
	 * 
	 * @return
	 */
	public int getTakeoffValueHighSingle() {
		return this.takeoffAngleHighSingle;
	}

	/**
	 * Returns takeoffAngleLowSingle - angle for drawing green cone for airport.
	 * 
	 * @return
	 */
	public int getTakeoffValueLowSingle() {
		return this.takeoffAngleLowSingle;
	}

	/**
	 * Returns boolean indicating if this plane is owned by the current player.
	 * 
	 * @return
	 */
	public boolean getOwnedByCurrentPlayer() {
		return this.ownedByCurrentPlayer;
	}

	/**
	 * Sets this plane's boolean indicator as to whether it is owned by the
	 * current player to the input parameter Owns.
	 * 
	 * @param Owns
	 */
	public void setOwnedByCurrentPlayer(boolean Owns) {
		this.ownedByCurrentPlayer = Owns;
	}

	/**
	 * Sets this plane's manual parameter to true - sets plane to manual.
	 */
	public void setManual() {
		manual = true;
		markForSyncing();
	}

	/**
	 * Sets this plane's manual parameter to false - sets plane to autopilot.
	 */
	public void setAuto() {
		manual = false;
		if (getFlightPlan().getCurrentRoute().size() > 0) {
			setTarget(getFlightPlan().getCurrentRoute().get(0));
		}
		markForSyncing();
	}

	/**
	 * Returns boolean indicating if this plane is in manual control (autopilot
	 * is disabled).
	 * 
	 * @return
	 */
	public boolean isManual() {
		return manual;
	}

	/**
	 * Boolean to inciate if this plane needs to be synced
	 */
	private boolean needsSyncing = true;

	public void markForSyncing() {
		this.needsSyncing = true;
	}

	/**
	 * Reset's the need to sync state - plane has been synced and no longer
	 * needs to be.
	 */
	public void resetSyncState() {
		this.needsSyncing = false;
	}

	/**
	 * Returns boolean indicating if the plane needs to be synced.
	 * 
	 * @return
	 */
	public boolean needsSyncing() {
		return this.needsSyncing;
	}

	/**
	 * Returns boolean indicating if the plane has been deleted.
	 * 
	 * @return
	 */
	public boolean deleted() {
		return deleted;
	}

	/**
	 * Marks this plane for deletion.
	 */
	public void markForDeletion() {
		deleted = true;
		markForSyncing();
	}

	/**
	 * Set's bearing for takeoff.
	 */
	public abstract void setBearingForTakeoff();
}
