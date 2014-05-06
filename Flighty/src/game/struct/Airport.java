package game.struct;

import org.newdawn.slick.geom.Polygon;

public class Airport extends Point {

	/** is plane currently landing **/
	private boolean planeLanding;

	/** is there a plane waiting to take off? **/
	private Plane planeWaitingtoTakeoff;

	/**
	 * waypoint at the end of the runway where planes leave/enter the airport at
	 **/
	private Waypoint beginningOfRunway, endOfRunway;

	/** area that a landing can be initiated from **/
	private Polygon landingApproachArea;

	/** point where the runway begins in X **/
	private static int beginningOfRunwayX;

	/** Y position of the end of the runway **/
	private static int runwayY;

	/** point where the runway ends in X **/
	private static int endOfRunwayX;

	/** size of triangle in X used for the runway **/
	private static int triangleSizeX;

	/** size of triangle in Y used for the runway **/
	private static int triangleSizeY;

	/**
	 * <p>
	 * Constructor
	 * </p>
	 * Initialises Airport class during runtime
	 * 
	 */
	public Airport(int beginningOfRunwayXVal, int runwayYVal,
			int endOfRunwayXVal, int triangleSizeXVal, int triangleSizeYVal) {
		// Placing the airport off screen as the airport graphics are part of
		// the graphics already
		Airport.beginningOfRunwayX = beginningOfRunwayXVal;
		Airport.runwayY = runwayYVal;
		Airport.endOfRunwayX = endOfRunwayXVal;
		Airport.triangleSizeX = triangleSizeXVal;
		Airport.triangleSizeY = triangleSizeYVal;

		this.x = -100;
		this.y = -100;

		// Airport status
		this.planeLanding = false;
		this.planeWaitingtoTakeoff = null;

		// Creating the runway waypoints
		this.beginningOfRunway = new Waypoint(beginningOfRunwayX, runwayY);
		this.endOfRunway = new Waypoint(endOfRunwayX, runwayY);

		// Creating the landing area. This is the triangle that appears when a
		// flight needs to land. It
		// is used to check whether the flights have the right approach.

		landingApproachArea = new Polygon();
		landingApproachArea.addPoint(beginningOfRunwayX, runwayY);
		landingApproachArea.addPoint(beginningOfRunwayX + triangleSizeX,
				runwayY + (triangleSizeY / 2));
		landingApproachArea.addPoint(beginningOfRunwayX + triangleSizeX,
				runwayY - (triangleSizeY / 2));
	}

	// All general accessors

	/**
	 * 
	 * @return The waypoint representing the beginning of the runway
	 */
	public Waypoint getBeginningOfRunway() {
		return beginningOfRunway;
	}

	/**
	 * 
	 * @return A plane if there is one currently waiting to takeoff
	 */
	public Plane getPlaneWaitingtoTakeoff() {
		return planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @return The waypoint representing the end of the runway
	 */
	public Waypoint getEndOfRunway() {
		return endOfRunway;
	}

	/**
	 * 
	 * @return Boolean representing whether a plane is landing
	 *         <p>
	 *         True - A plane is landing
	 *         </p>
	 *         <p>
	 *         False - A plane is not landing
	 *         </p>
	 */
	public boolean isPlaneLanding() {
		return planeLanding;
	}

	/**
	 * 
	 * @return A polygon representing the landing approach area i.e. the area
	 *         which a plane must be in before landing
	 */
	public Polygon getLandingApproachArea() {
		return landingApproachArea;
	}

	/**
	 * Get the x posiiton of the end of the runway
	 * 
	 * @return
	 */
	public static int getEndOfRunwayX() {
		return endOfRunwayX;
	}

	/**
	 * Get the x position of the start of the runway
	 * 
	 * @return
	 */
	public static int getBeginningOfRunwayX() {
		return beginningOfRunwayX;
	}

	/**
	 * get the y position of the runway
	 * 
	 * @return
	 */
	public static int getRunwayY() {
		return runwayY;
	}

	/**
	 * Get the size of the runway triangle
	 * 
	 * @return
	 */
	public static int[] getTriangleSize() {
		int[] temp = { triangleSizeX, triangleSizeY };
		return temp;
	}

	// All general Mutators

	/**
	 * 
	 * @param beginningOfRunway
	 *            Waypoint that is to represent the beginning of the runway
	 */
	public void setBeginningOfRunway(Waypoint beginningOfRunway) {
		this.beginningOfRunway = beginningOfRunway;
	}

	/**
	 * 
	 * @param endOfRunway
	 *            Waypoint that is to represent the end of the runway
	 */
	public void setEndOfRunway(Waypoint endOfRunway) {
		this.endOfRunway = endOfRunway;
	}

	/**
	 * 
	 * @param planeLandingOrTakingOff
	 *            Boolean value that sets planeLanding variable
	 */
	public void setPlaneLanding(boolean planeLandingOrTakingOff) {
		this.planeLanding = planeLandingOrTakingOff;
	}

	/**
	 * 
	 * @param planeWaitingtoTakeoff
	 *            If there is a plane waiting to takeoff, the
	 *            planeWaitingToTakeoff variable is set to this
	 */
	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) {
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @param landingApproachArea
	 *            sets the landing area
	 */
	public void setLandingApproachArea(Polygon landingApproachArea) {
		this.landingApproachArea = landingApproachArea;
	}

}