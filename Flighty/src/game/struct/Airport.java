package game.struct;

import org.newdawn.slick.geom.*;

public class Airport extends Point {

	private boolean planeLanding;
	private Plane planeWaitingtoTakeoff;
	private Waypoint beginningOfRunway, endOfRunway;
	private Polygon landingApproachArea;
	private static int beginningOfRunwayX;
	private static int runwayY;
	private static int endOfRunwayX;
	private static int triangleSizeX;
	private static int triangleSizeY;
	

	/**
	 * <p>
	 * Constructor
	 * </p>
	 * Initialises Airport class during runtime
	 * 
	 */
	public Airport(int beginningOfRunwayXVal, int runwayYVal, int endOfRunwayXVal, int triangleSizeXVal, int triangleSizeYVal) {
		// Placing the airport off screen as the airport graphics are part of
		// the graphics already
		this.beginningOfRunwayX = beginningOfRunwayXVal;
		this.runwayY = runwayYVal;
		this.endOfRunwayX = endOfRunwayXVal;
		this.triangleSizeX = triangleSizeXVal;
		this.triangleSizeY = triangleSizeYVal;
		
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
		landingApproachArea.addPoint(beginningOfRunwayX + triangleSizeX, runwayY + (triangleSizeY/2));
		landingApproachArea.addPoint(beginningOfRunwayX + triangleSizeX, runwayY - (triangleSizeY/2));
	}

	// ACCESSORS AND MODIFIERS

	/**
	 * 
	 * @return The waypoint representing the beginning of the runway
	 */
	public Waypoint getBeginningOfRunway() {
		return beginningOfRunway;
	}

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
	 * @return The waypoint representing the end of the runway
	 */
	public Waypoint getEndOfRunway() {
		return endOfRunway;
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
	 * @param planeLandingOrTakingOff
	 *            Boolean value that sets planeLanding variable
	 */
	public void setPlaneLanding(boolean planeLandingOrTakingOff) {
		this.planeLanding = planeLandingOrTakingOff;
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
	 * @param planeWaitingtoTakeoff
	 *            If there is a plane waiting to takeoff, the
	 *            planeWaitingToTakeoff variable is set to this
	 */
	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) {
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
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
	 * 
	 * @param landingApproachArea
	 *            sets the landing area
	 */
	public void setLandingApproachArea(Polygon landingApproachArea) {
		this.landingApproachArea = landingApproachArea;
	}
	
	public static int getEndOfRunwayX() {
		return endOfRunwayX;
	}
	
	public static int getBeginningOfRunwayX() {
		return beginningOfRunwayX;
	}
	
	public static int getRunwayY() {
		return runwayY;
	}
	
	public static int[] getTriangleSize() {
		int[] temp = {triangleSizeX, triangleSizeY};
		return temp;
	}
	
}