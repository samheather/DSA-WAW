package game.struct;

import org.newdawn.slick.geom.*;

public class Airport extends Point
{	
	
	private boolean 	planeLanding;
	private Plane 		planeWaitingtoTakeoff;
	private Waypoint 	beginningOfRunway, endOfRunway;
	private Polygon 	landingApproachArea;

	/**
	 * <p>
	 * Constructor
	 * </p>
	 * Initialises Airport class during runtime
	 *
	 */
    public Airport()
    {
    	// Placing the airport off screen as the airport graphics are part of the graphics already
    	this.x 							= -100; 
    	this.y 							= -100;
    	
    	// Airport status
    	this.planeLanding 				= false;
    	this.planeWaitingtoTakeoff	 	= null;
    	
    	// Creating the runway waypoints
    	this.beginningOfRunway 			= new Waypoint(720,460);
    	this.endOfRunway 				= new Waypoint(1180,465);
    	
    	// Creating the landing area
    	landingApproachArea 			= new Polygon();
    	landingApproachArea.addPoint(720, 465);
    	landingApproachArea.addPoint(400, 344);
    	landingApproachArea.addPoint(400, 576);
    }

    // ACCESSORS AND MODIFIERS

    /**
     * 
     * @return The waypoint representing the beginning of the runway
     */
	public Waypoint getBeginningOfRunway() 
	{
		return beginningOfRunway;
	}

	/**
	 * 
	 * @param beginningOfRunway  Waypoint that is to represent the beginning of the runway
	 */
	public void setBeginningOfRunway(Waypoint beginningOfRunway) 
	{
		this.beginningOfRunway = beginningOfRunway;
	}

	/**
	 * 
	 * @return  The waypoint representing the end of the runway
	 */
	public Waypoint getEndOfRunway() 
	{
		return endOfRunway;
	}

	/**
	 * 
	 * @param endOfRunway Waypoint that is to represent the end of the runway
	 */
	public void setEndOfRunway(Waypoint endOfRunway) 
	{
		this.endOfRunway = endOfRunway;
	}

	/**
	 * 
	 * @return Boolean representing whether a plane is landing
	 * <p> True - A plane is landing </p>
	 * <p> False - A plane is not landing </p>
	 */
	public boolean isPlaneLanding() 
	{
		return planeLanding;
	}

	/**
	 * 
	 * @param planeLandingOrTakingOff Boolean value that sets planeLanding variable
	 */
	public void setPlaneLanding(boolean planeLandingOrTakingOff) 
	{
		this.planeLanding = planeLandingOrTakingOff;
	}

	/**
	 * 
	 * @return A plane if there is one currently waiting to takeoff
	 */
	public Plane getPlaneWaitingtoTakeoff() 
	{
		return planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @param planeWaitingtoTakeoff If there is a plane waiting to takeoff, the planeWaitingToTakeoff variable is set to this
	 */
	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) 
	{
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @return A polygon representing the landing approach area i.e. the area which a plane must be in before landing
	 */
	public Polygon getLandingApproachArea() 
	{
		return landingApproachArea;
	}

	/**
	 * 
	 * @param landingApproachArea sets the landing area
	 */
	public void setLandingApproachArea(Polygon landingApproachArea) 
	{
		this.landingApproachArea = landingApproachArea;
	}
}