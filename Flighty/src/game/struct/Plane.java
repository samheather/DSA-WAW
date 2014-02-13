package game.struct;


import java.util.ArrayList;


/**
 * Plane class
 */
public class Plane {

	/** Unique identifier */
	private int id;

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
	private Point target;

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
	
	/**Boolean to express whether a plane needs to take off */
	private boolean needsToTakeOff;
	
	/** Boolean to express whether a plane has ever broken the separation rules */
	private boolean violationOccurred;
	
	/** Necessary for calculation of plane's landing descent rate */
	private double landingDescentRate;
	
	/** Required by Slick2D */
	private Game currentGame;

	// Constructor
	
	/**
	 * Constructor for Plane
	 * <p>
	 * Initialises the plane's position and general information about the flight.
	 * </p>
	 * 
	 * @param id		a unique identifier
	 * @param crew		the number of crew on board
	 * @param size		the size to display the plane at
	 * @param velocity	the speed at which the plane is traveling
	 * @param altitude	the altitude at which the plane is traveling
	 * @param bearing	the bearing the plane is following
	 * 					(in <b>radians</b>)
	 * @param x			the x position to create the plane at
	 * @param y			the y position to create the plane at
	 */
	public Plane(int id, double velocity, int altitude, 
			double bearing, Game currentGame) {
		this.currentGame = currentGame;
		this.id = id;
		this.size = 2;
		this.velocity = velocity;
		this.altitude = altitude;
		this.bearing = bearing;
		this.targetBearing= bearing;
		this.x = 0;
		this.y = 0;
		this.flightPlan = new FlightPlan(currentGame, this);
		this.target = this.flightPlan.getCurrentRoute().get(0);
		this.targetAltitude = altitude;
		this.turningLeft=false;
		this.turningRight=false;
		this.landing = false;
		this.needsToLand = false;
		this.needsToTakeOff = false;
		this.takingOff = false;
		this.landingDescentRate=0;
		this.violationOccurred = false;
	}

	// METHODS

	/**
	 * Determine whether a plane is currently at waypoint
	 * @param waypoint The waypoint which is currently being checked
	 * @param game Required by Slick2D library
	 * @return Boolean which is set to true if  a flight is currently at the waypoint; false otherwise
	 */
	public boolean checkIfFlightAtWaypoint(Point waypoint, Game game) {
		
		// Ensuring that the plane cannot go through its landing waypoint when it isnt landing
		if (waypoint.equals(game.getAirport().getBeginningOfRunway()) && this.landing != true && this.takingOff != true){
			return false;
		}

		if (((Math.abs(Math.round(this.x) - Math.round(waypoint.getX()))) <= 15)
				&& (Math.abs(Math.round(this.y) - Math.round(waypoint.getY()))) <= 15) {
			
			return true;
		}

		return false;
	}

	/**
	 * Increments the Plane's bearing by 5 degrees
	 */
	public void incrementBearing() {
		
		this.turningLeft = false;
		this.turningRight = true;
		
		this.bearing +=1;
		this.targetBearing = this.bearing;
		
		
		if (bearing >= 360){
			this.bearing = 0;
			this.targetBearing = 0;
		}
	}

	/**
	 * Decrements the Plane's bearing by 5 degrees
	 */
	public void decrementBearing() {
		
		this.turningRight = false;
		this.turningLeft = true;
		
		this.bearing -=1;
		this.targetBearing = this.bearing; 
		
		if (bearing < 0){
			this.bearing = 359;
			this.targetBearing = 359;
		}
	}
	
	/**
	 * Increments the Plane's altitude to the next flight level
	 * <p>
	 * Note: highest flight level allowed for planes is 4
	 * </p>
	 */
	public void incrementAltitude() {
		this.altitude += 5;
	}

	/**
	 * Decrements the Plane's target altitude to the next flight level
	 * <p>
	 * Note: lowest flight level allowed for planes is 1
	 * </p>
	 */
	public void decrementAltitude() {
		this.altitude -= 5;
	}

	/**
	 * Increments the Plane's target altitude to the next flight level
	 * <p>
	 * Note: highest flight level allowed for planes is 4
	 * </p>
	 */
	public void incrementTargetAltitude() {
		if(this.targetAltitude <= 6000) {
			this.targetAltitude+=1000;
		}
	}
	
	/** Decrements the altitude by 1000 units, so long as the current altitude is greater than or equal to 3000 */
	public void decrementTargetAltitude() {
		if(this.targetAltitude >= 3000) {
			this.targetAltitude-=1000;
		}
	}
	
	/** Calculates bearing from plane's current position, to its next waypoint */
	public void calculateBearingToNextWaypoint(){
		double angle;
		angle = Math.toDegrees(Math.atan2(this.y - this.target.getY(),
				this.x - this.target.getX()));
		
		if(angle<0) {
			angle +=360;
		}
		this.turningRight = false;
		this.turningLeft = false;
		this.targetBearing = angle;
	}

	/** Updates current bearing */
	public void updateCurrentBearing() {

		double rate = 0.9;
		if (Math.round(this.targetBearing) <= Math.round(this.bearing)-3 || Math.round(this.targetBearing) >= Math.round(this.bearing)+3) {
			

			/*
			 * If plane has been given a heading so no turning direction specified,
			 * below works out whether it should turn left or right to that heading
			 */

			if(this.turningRight == false && this.turningLeft == false){

				if (Math.abs(this.targetBearing - this.bearing) == 180) {
					this.turningRight = true;
				} 

				else if (this.bearing + 180 <= 359){

					if (this.targetBearing < this.bearing + 180 && this.targetBearing > this.bearing){
						this.turningRight = true;
					}
					else {
						this.turningLeft = true;
					}
				}

				else {

					if (this.targetBearing > this.bearing - 180 && this.targetBearing < this.bearing){
						this.turningLeft = true;
					}
					else {
						this.turningRight = true;
					}
				}

			}
			// If plane is already turning right or user has told it to turn right

			if (this.turningRight == true) {
				this.bearing+=rate;
				if (Math.round(this.bearing) >= 360 && this.targetBearing != 360) {
					this.bearing = 0;
				}
			}

			// If plane is already turning left or user has told it to turn left

			if (this.turningLeft == true) {
				this.bearing-=rate;
				if (Math.round(this.bearing) <= 0 && this.targetBearing != 0) {
					this.bearing = 360;
				}
			}
		}
		else {
			this.turningLeft=false;
			this.turningRight=false;
		}
	}

    /** Calculates the rate at which a plane has to descend, given its current altitude, such
     * that by the time it reaches the runway, its altitude is 0
     * @return Rate at which plane needs to descend
     */
	public double findLandingDescentRate() {
		
		double rate;
		//find distance to runway waypoint
		double distanceFromRunway =  Math.sqrt(Math.pow(this.x-this.currentGame.getAirport().getBeginningOfRunway().getX(), 2)+Math.pow(this.y-this.currentGame.getAirport().getBeginningOfRunway().getY(), 2));
		double descentPerPixel = this.altitude/distanceFromRunway;
		rate = descentPerPixel*(this.velocity)*this.currentGame.getSpeedDifficulty();

		return rate;
	}
	
	/** First checks that another plane is not landing, then checks whether plane is in valid position
	 * to initiate landing before finally checking that the plane's current bearing is such that the plane
	 * is facing the runway. If all these conditions are met, the plane begins to land.
	 */
	public void landPlane(){
		
		if (!this.currentGame.getAirport().isPlaneLanding()){
			if (this.currentGame.getAirport().getLandingApproachArea().contains((float)this.x, (float)this.y)){
				if (this.bearing >= 150 && this.bearing <= 210 && this.altitude <= 2000){
					this.currentGame.getAirport().setPlaneLanding(true);
					this.needsToLand = false;
					this.landing = true;
					this.target = this.flightPlan.getCurrentRoute().get(0);
					this.calculateBearingToNextWaypoint();
					this.landingDescentRate=this.findLandingDescentRate();
					this.currentGame.getManualPlanes().remove(this);
					this.currentGame.setCurrentPlane(null);					
				}
			}
		}
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
	public void movePlane() { 

		// If Plane is taking off, don't change the bearing
		if(this.altitude < 2000 && this.targetAltitude>0 && !this.landing) {

			this.updateXYCoordinates();

		}
		else {
			double angle = this.targetBearing;


			if(this.target != null) {
				if(!this.currentGame.getManualPlanes().contains(this)) {

					// Get the angle to the next waypoint
					this.calculateBearingToNextWaypoint();
					this.updateCurrentBearing();


				}
				else {
					this.updateCurrentBearing();

				}

				// Move the plane

				this.updateXYCoordinates();
			}
		}
	}
	
	/** Updates x and y coordinates */
	public void updateXYCoordinates(){
		
		this.setX((float) (this.x
				- (Math.cos(Math.toRadians(this.bearing))
						* (this.currentGame.getSpeedDifficulty()
								* this.velocity))));
		
		this.setY((float) (this.y
				- (Math.sin(Math.toRadians(this.bearing))
						* (this.currentGame.getSpeedDifficulty()
								* this.velocity))));
		
	}

	// Accessors
	
	/**
	 * @return the violationOccurred boolean
	 */
	public boolean getViolationOccurred(){
		return this.violationOccurred;
	}
	
	/**
	 * @return			the Plane's unique ID
	 */
	public int getID() {
		return this.id;
	}

	/**
	 * @return			the size the plane displays at
	 */
	public int getSize() {
		return this.size;
	}

	/**
	 * @return			the plane's current speed
	 */
	public double getVelocity() {
		return this.velocity;
	}

	/**
	 * @return			the plane's current altitude
	 */
	public int getAltitude() {
		return this.altitude;
	}

	/**
	 * @return			the plane's current bearing
	 */
	public double getBearing() {
		return this.bearing;
	}

	/**
	 * @return			the plane's current X position
	 */
	public double getX() {
		return this.x;
	}

	/**
	 * @return			the plane's current Y position
	 */
	public double getY() {
		return this.y;
	}

	/**
	 * @return			<code>true</code> if the plane is alerting,
	 * 					<code>false</code> otherwise
	 */
	public boolean getAlertStatus() {
		return this.alertStatus;
	}

	/**
	 * @return			the plane's current destination
	 */
	public Point getTarget() {
		return this.target;
	}

	/**
	 * @return			the plane's target altitude
	 */
	public double getTargetAltitude() {
		return this.targetAltitude;
	}

	// Mutators
	
	/**
	 * Set violationOccurred boolean to true
	 */
	public void setViolationOccurred(){
		this.violationOccurred = true;
	}

	/**
	 * @param id		the new unique ID
	 */
	public void setID(int id) {
		this.id = id;
	}

	/**
	 * @param size		the new display size
	 */
	public void setSize(int size) {
		this.size = size;
	}

	/**
	 * @param velocity	the new speed
	 */
	public void setVelocity(double velocity) {
		this.velocity = velocity;
	}

	/**
	 * @param altitude	the new altitude
	 */
	public void setAltitude(int altitude) {
		this.altitude = altitude;
	}

	/**
	 * @param bearing	the new bearing
	 */
	public void setBearing(double bearing) {
		this.bearing = bearing;
	}

	/**
	 * @param x			the new X co-ordinate
	 */
	public void setX(double x) {
		this.x = x;
	}

	/**
	 * @param y			the new Y co-ordinate
	 */
	public void setY(double y) {
		this.y = y;
	}

	/**
	 * @param status	<code>true</code> if the plane should be alerting,
	 * 					<code>false</code> otherwise
	 */
	public void setAlertStatus(boolean status) {
		this.alertStatus = status;
	}

	/**
	 * @param target	the new target Waypoint
	 */
	public void setTarget(Point target) {
		this.target = target;
	}

	/**
	 * @param targetAltitude	the new target altitude
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
	public boolean isNeedsToLand() {
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


	public boolean isTurningLeft() {
		return turningLeft;
	}


	public void setTurningLeft(boolean turningLeft) {
		this.turningLeft = turningLeft;
	}

	
	public boolean isLanding(){
		return this.landing;
	}
	
	
	public void setLanding(boolean bool){
		this.landing = bool;
	}
	
	
	public boolean isTakingOff() {
		return takingOff;
	}


	public void setTakingOff(boolean takingOff) {
		this.takingOff = takingOff;
	}


	public boolean isNeedsToTakeOff() {
		return needsToTakeOff;
	}


	public void setNeedsToTakeOff(boolean needsToTakeOff) {
		this.needsToTakeOff = needsToTakeOff;
	}


	public double getLandingDescentRate() {
		return landingDescentRate;
	}


	public void setLandingDescentRate(double landingDescentRate) {
		this.landingDescentRate = landingDescentRate;
	}

}
