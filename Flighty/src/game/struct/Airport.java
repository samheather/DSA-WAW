package game.struct;

import org.newdawn.slick.geom.*;

public class Airport extends Point
{	
	
	private int		 	landingAltitude;
	private boolean 	planeLanding;
	private Plane 		planeWaitingtoTakeoff;
	private Waypoint 	beginningOfRunway, endOfRunway;
	private Polygon 	landingApproachArea;

    public Airport()
    {
    	// Placing the airport off screen as the airport graphics are part of the graphics already
    	this.x 							= -100; 
    	this.y 							= -100;
    	
    	// Airport status
    	this.planeLanding 				= false;
    	this.planeWaitingtoTakeoff	 	= null;
    	
    	
    	// TODO what's this for?
    	this.landingAltitude 			= 20000;
    	
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
     * @return the beginning of the runway
     */
	public Waypoint getBeginningOfRunway() 
	{
		return beginningOfRunway;
	}

	/**
	 * 
	 * @param beginningOfRunway sets the beginning of the runway
	 */
	public void setBeginningOfRunway(Waypoint beginningOfRunway) 
	{
		this.beginningOfRunway = beginningOfRunway;
	}

	/**
	 * 
	 * @return the end of the runway
	 */
	public Waypoint getEndOfRunway() 
	{
		return endOfRunway;
	}

	/**
	 * 
	 * @param endOfRunway set the end of the runway
	 */
	public void setEndOfRunway(Waypoint endOfRunway) 
	{
		this.endOfRunway = endOfRunway;
	}

	/**
	 * 
	 * @return the landing altitude
	 */
	public int getLandingAltitude() 
	{
		return landingAltitude;
	}

	/**
	 * 
	 * @param landingAltitude set the landing altitude
	 */
	public void setLandingAltitude(int landingAltitude) 
	{
		this.landingAltitude = landingAltitude;
	}
	/**
	 * 
	 * @return checks whether a plane is landing or not
	 */
	public boolean isPlaneLanding() 
	{
		return planeLanding;
	}

	/**
	 * 
	 * @param planeLandingOrTakingOff set whether a plane is landing
	 */
	public void setPlaneLanding(boolean planeLandingOrTakingOff) 
	{
		this.planeLanding = planeLandingOrTakingOff;
	}

	/**
	 * 
	 * @return whether a plane is waiting to take off
	 */
	public Plane getPlaneWaitingtoTakeoff() 
	{
		return planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @param planeWaitingtoTakeoff sets whether a plane is waiting to take off
	 */
	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) 
	{
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
	}

	/**
	 * 
	 * @return the landing area
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