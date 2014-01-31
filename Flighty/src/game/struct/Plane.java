package game.struct;


import java.util.ArrayList;


/**
 * Plane class
 */
public class Plane {

	/** Unique identifier */
	private String id;


	/** Size to display plane */
	private int size;

	/** Speed the plane is traveling at */
	private int velocity;

	/** Current altitude */
	private double altitude;

	/** Current bearing in radians */
	private double bearing;

	private double targetBearing;

	private boolean turningRight, turningLeft;



	/** Current X co-ordinate */
	private double x;

	/** Current Y co-ordinate */
	private double y;

	/** Original X co-ordinate */
	private double startX;

	/** Original Y co-ordinate */
	private double startY;

	/** Is the plane alerting */
	private boolean alertStatus;

	/** Current destination */
	private Point target;

	/** Current target altitude */
	private double targetAltitude;

	private FlightPlan flightPlan;
	
	private boolean landing, needsToLand;
	
	private Game currentGame;


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
	// Constructor
	public Plane(String id, int size, int velocity, double altitude, 
			double bearing, Game currentGame) {
		this.currentGame = currentGame;
		this.id = id;
		this.size = size;
		this.velocity = velocity;
		this.altitude = altitude;
		this.bearing = bearing;
		this.targetBearing=0;
		this.x = 0;
		this.y = 0;
		this.startX = 0;
		this.startY = 0;
		this.flightPlan = new FlightPlan(currentGame, this);
		this.target = this.flightPlan.getCurrentRoute().get(0);
		this.targetAltitude = altitude;
		this.turningLeft=false;
		this.turningRight=false;
		this.landing = false;
		this.needsToLand = false;
		

	}





	// MAIN METHODS

	public boolean checkIfFlightAtWaypoint(Point waypoint, Game game) {
		
		// Ensuring that the plane cannot go through its landing waypoint when it isnt landing
		if (waypoint.equals(game.getAirport().getBeginningOfRunway()) && this.landing != true){
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
		this.altitude += 0.005;
	}

	/**
	 * Decrements the Plane's target altitude to the next flight level
	 * <p>
	 * Note: lowest flight level allowed for planes is 1
	 * </p>
	 */
	public void decrementAltitude() {
		this.altitude -= 0.005;
	}

	/**
	 * Increments the Plane's target altitude to the next flight level
	 * <p>
	 * Note: highest flight level allowed for planes is 4
	 * </p>
	 */
	public void incrementTargetAltitude() {
		if(this.targetAltitude <= 3) {
			this.targetAltitude++;
		}
	}
	
	public void calculateHeadingToNextWaypoint(){
		double angle;
		angle = Math.toDegrees(Math.atan2(this.y - this.target.getY(),
				this.x - this.target.getX()));
		//System.out.println(angle);
		if(angle<0) {
			angle +=360;
		}
		this.turningRight = false;
		this.turningLeft = false;
		this.targetBearing = angle;

	}

	public void updateCurrentHeading() {

		double rate = 0.9;
		if (Math.round(this.targetBearing) <= Math.round(this.bearing)-3 || Math.round(this.targetBearing) >= Math.round(this.bearing)+3) {
			//System.out.println(Math.round(this.targetBearing));
			//System.out.println(Math.round(this.bearing));


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

	/**
	 * Decrements the Plane's altitude to the next flight level
	 * <p>
	 * Note: lowest flight level allowed for planes is 1
	 * </p>
	 */
	public void decrementTargetAltitude() {
		if(this.targetAltitude > 1) {
			this.targetAltitude--;
		}
	}
	
	public void landPlane(){
		
		
		
		if (this.currentGame.getAirport().getApproachPolygon().contains((float)this.x, (float)this.y)){
			if (this.bearing >= 150 && this.bearing <= 210 && this.altitude <= 1){
				this.needsToLand = false;
				this.landing = true;
				this.target = this.flightPlan.getCurrentRoute().get(0);
				this.calculateHeadingToNextWaypoint();
				this.currentGame.getManualPlanes().remove(this);
				this.currentGame.setCurrentPlane(null);

				
				
			}
		}
	}









	// Accessors
	
	
	
	/**
	 * @return			the Plane's unique ID
	 */
	public String getID() {
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
	public int getVelocity() {
		return this.velocity;
	}

	/**
	 * @return			the plane's current altitude
	 */
	public double getAltitude() {
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
	 * @param id		the new unique ID
	 */
	public void setID(String id) {
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
	public void setVelocity(int velocity) {
		this.velocity = velocity;
	}

	/**
	 * @param altitude	the new altitude
	 */
	public void setAltitude(double altitude) {
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

	public FlightPlan getFlightPlan() {
		return flightPlan;
	}





	public boolean isNeedsToLand() {
		return needsToLand;
	}





	public void setNeedsToLand(boolean needsToLand) {
		this.needsToLand = needsToLand;
	}





	public void setFlightPlan(FlightPlan flightPlan) {
		this.flightPlan = flightPlan;
	}





	public double getStartX() {
		return startX;
	}





	public void setStartX(double startX) {
		this.startX = startX;
	}





	public double getStartY() {
		return startY;
	}





	public void setStartY(double startY) {
		this.startY = startY;
	}

	public double getTargetBearing() {
		return targetBearing;
	}





	public void setTargetBearing(double targetBearing) {
		this.targetBearing = targetBearing;
	}




	public boolean isTurningRight() {
		return turningRight;
	}





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
	





	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof Plane)) {
			return false;
		}
		Plane other = (Plane) obj;
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		return true;
	}
}
