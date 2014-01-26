package game.struct;


import java.util.ArrayList;


/**
 * Plane class
 */
public class Plane {

	/** Unique identifier */
	private String id;
	
	/** Number of crew on board */
	private int crew;
	
	/** Size to display plane */
	private int size;
	
	/** Speed the plane is traveling at */
	private int velocity;
	
	/** Current altitude */
	private double altitude;
	
	/** Current bearing in radians */
	private double bearing;
	
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
	private Waypoint target;
	
	/** Current target altitude */
	private double targetAltitude;

	
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
	public Plane(String id, int crew, int size, int velocity, double altitude, 
					double bearing, float x, float y) {
		this.id = id;
		this.crew = crew;
		this.size = size;
		this.velocity = velocity;
		this.altitude = altitude;
		this.bearing = bearing;
		this.x = x;
		this.y = y;
		this.startX = x;
		this.startY = y;
		this.target = null;
		this.targetAltitude = altitude;
	}


	


	// MAIN METHODS
	
	/**
	 * Increments the Plane's bearing by 5 degrees
	 */
	public void incrementBearing() {
		this.bearing += Math.toRadians(5);
	}
	
	/**
	 * Decrements the Plane's bearing by 5 degrees
	 */
	public void decrementBearing() {
		this.bearing -= Math.toRadians(5);
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
	
	/**
	 * Creates a flight plan and attaches it to the current Plane
	 * <p>
	 * A Catmul-Rom spline (cubic hermite spline derivative) implementation is used
	 * </p>
	 * <p>
	 * A series of visible Waypoints are created, with random positions within
	 * a bounded region.
	 * </p>
	 * <p>
	 * An ExitPoint is created (at the edge of the screen)
	 * </p>
	 * <p>
	 * A series of invisible Waypoints are created, to allow smoother turning
	 * </p>
	 * 
	 * @param numWaypoints	the number of Waypoints to add to the flight path
	 * @param currentGame	the number of Waypoints to add to the flight path
	 */
	public void generateFlightPlan(int numWaypoints, Game currentGame) {
		this.target = this.interpolateCurve(
				this.generateFlightPlanTemplate(numWaypoints, currentGame));
	}

	/**
	 * <b>DO NOT USE</b> Creates the constituent points of a flight plan
	 * <p>
	 * Should be used for testing purposes only.
	 * </p>
	 */
	public ArrayList<double[]> generateFlightPlanTemplate(int numWaypoints,
			Game currentGame) {
		double x, y;
		int width, height;
		ArrayList<double[]> tmpPoints;
		
		tmpPoints = new ArrayList<double[]>();

		if(currentGame.getCurrentGameWindow() != null) {
			width = currentGame.getCurrentGameWindow().getWindowWidth();
			height = currentGame.getCurrentGameWindow().getWindowHeight();
		} else {
			width = 1024;
			height = 512;
		}

		if (Math.random() > 0.5) {
			// Set x coord to a random point
			// Set y coord to the top or bottom of the screen

			x = (Math.random() * width);
			y = ((Math.random() > 0.5) ? 0 : height);
		} else {
			// Set x coord to the left or right of the screen
			// Set y coord to a random point

			x = ((Math.random() > 0.5) ? 0 : width);
			y = (Math.random() * height);
		}

		// Point plane to the new exit point
		tmpPoints.add(new double[] {x, y});
		tmpPoints.add(new double[] {x, y});

		// Add in numWaypoints of intermediate Waypoints
		for(int i = 0; i < numWaypoints; i++) {
			tmpPoints.add(new double[] {(Math.min(Math.max(Math.random(), 0.1 ), 0.9)
					* width), (Math.min(Math.max(Math.random(), 0.1 ), 0.9) * height)});
		}
		
		return tmpPoints;
	}
	
	/**
	 * <b>DO NOT USE</b> Produces a flight plan from a list of points
	 * <p>
	 * Should be used for testing purposes only.
	 * </p>
	 */
	public Waypoint interpolateCurve(ArrayList<double[]> points) {
		// Pt = (2t^3-3t^2+1)P0 + (t^3-2t^2+t)M0 + (-2t^3+3t^2)P1 + (t^3-t^2)M1

		double x, y;
		double Pnegx, Pnegy, P0x, P0y, P1x, P1y, P2x, P2y;
		double M1x, M1y, M2x, M2y;
		double t2, t3;
		double H0, H1, H2, H3;
		
		Waypoint target = this.target;
		
		// Build linked list recursively, starting from a random ExitPoint
		// and working back towards the plane's original location
		
		// Add ExitPoint
		target = new ExitPoint(points.get(0)[0], points.get(0)[1], true);
		
		// Duplicate 'last' (actually first) point
		points.add(new double[] {this.startX, this.startY});
		points.add(new double[] {this.startX, this.startY});
		
		for(int i = 1; i < (points.size() - 2); i++) {
			Pnegx = points.get(i - 1)[0];
			Pnegy = points.get(i - 1)[1];
			
			P0x = points.get(i)[0];
			P0y = points.get(i)[1];
			
			P1x = points.get(i + 1)[0];
			P1y = points.get(i + 1)[1];
			
			P2x = points.get(i + 2)[0];
			P2y = points.get(i + 2)[1];
			
			for(double t = 0; t < 1; t += 0.01) {
				// Stop sampling in range 0.3 to 0.7
				// Decimate sampling after 0.1 and before 0.9
				if((t > 0.299) && (t < 0.301)) {
					t = 0.7;
				} else if ((t < 0.10)
						&& ((t > 0.051) || (t < 0.049))) {
					continue;
				}
				
				t2 = Math.pow(t, 2);
				t3 = Math.pow(t, 3);

				H0 = ((2 * t3) - (3 * t2) + 1);
				H1 = (t3 - (2 * t2) + t);
				H2 = (-2 * t3) + (3 * t2);
				H3 = (t3 - t2);

				M1x = 0.5 * (P1x - Pnegx);
				M1y = 0.5 * (P1y - Pnegy);
				M2x = 0.35 * (P2x - P0x);
				M2y = 0.35 * (P2y - P0y);

				x = (H0 * P0x) + (H1 * M1x) + (H2 * P1x) + (H3 * M2x);
				y = (H0 * P0y) + (H1 * M1y) + (H2 * P1y) + (H3 * M2y);

				target = target.createWaypoint(x, y);
			}
			
			target = target.createWaypoint(P1x, P1y, true);
		}
		
		return target;
	}
	
	/**
	 * Gets the next <b>visible</b> target
	 */
	public Waypoint getNextVisibleTarget() {
		Waypoint tmpWaypoint = this.target;
		
		while(tmpWaypoint != null) {
			if(tmpWaypoint.isVisible()) {
				return tmpWaypoint;
			}
			
			tmpWaypoint = tmpWaypoint.getNext();
		}
		
		return null;
	}
	
	/**
	 * Gets the next <b>visible</b> target
	 * <p>
	 * This method allows selection of the point to start searching from.
	 */
	public Waypoint getNextVisibleTarget(Waypoint start) {
		Waypoint tmpWaypoint = start.getNext();
		
		while(tmpWaypoint != null) {
			if(tmpWaypoint.isVisible()) {
				return tmpWaypoint;
			}
			
			tmpWaypoint = tmpWaypoint.getNext();
		}
		
		return null;
	}


	// Overrides
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = (int) (prime * result + altitude);
		long temp;
		temp = Double.doubleToLongBits(bearing);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + crew;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		temp = Double.doubleToLongBits(startX);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(startY);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((target == null) ? 0 : target.hashCode());
		result = prime * result + velocity;
		temp = Double.doubleToLongBits(x);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(y);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}
	
	// Accessors
		/**
		 * @return			the Plane's unique ID
		 */
		public String getID() {
			return this.id;
		}

		/**
		 * @return			the number of crew on board
		 */
		public int getCrew() {
			return this.crew;
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
		public Waypoint getTarget() {
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
		 * @param crew		the new crew count
		 */
		public void setCrew(int crew) {
			this.crew = crew;
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
		public void setTarget(Waypoint target) {
			this.target = target;
		}
		
		/**
		 * @param targetAltitude	the new target altitude
		 */
		public void setTargetAltitude(double targetAltitude) {
			this.targetAltitude = targetAltitude;
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
