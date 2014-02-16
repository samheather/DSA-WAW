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
    	this.planeLanding 				= false;
    	this.planeWaitingtoTakeoff	 	= null;
    	this.landingAltitude 			= 20000;
    	this.beginningOfRunway 			= new Waypoint( 720,460);
    	this.endOfRunway 				= new Waypoint(1180,465);
    	landingApproachArea 			= new Polygon();
    	landingApproachArea.addPoint(720, 465);
    	landingApproachArea.addPoint(400, 344);
    	landingApproachArea.addPoint(400, 576);
    }

    // ACCESSORS AND MODIFIERS

	public Waypoint getBeginningOfRunway() 
	{
		return beginningOfRunway;
	}

	public void setBeginningOfRunway(Waypoint beginningOfRunway) 
	{
		this.beginningOfRunway = beginningOfRunway;
	}

	public Waypoint getEndOfRunway() 
	{
		return endOfRunway;
	}

	public void setEndOfRunway(Waypoint endOfRunway) 
	{
		this.endOfRunway = endOfRunway;
	}

	public int getLandingAltitude() 
	{
		return landingAltitude;
	}


	public void setLandingAltitude(int landingAltitude) 
	{
		this.landingAltitude = landingAltitude;
	}

	public boolean isPlaneLanding() 
	{
		return planeLanding;
	}


	public void setPlaneLanding(boolean planeLandingOrTakingOff) 
	{
		this.planeLanding = planeLandingOrTakingOff;
	}


	public Plane getPlaneWaitingtoTakeoff() 
	{
		return planeWaitingtoTakeoff;
	}


	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) 
	{
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
	}

	public Polygon getLandingApproachArea() 
	{
		return landingApproachArea;
	}

	public void setLandingApproachArea(Polygon landingApproachArea) 
	{
		this.landingApproachArea = landingApproachArea;
	}
}